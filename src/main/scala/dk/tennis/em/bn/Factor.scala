package dk.tennis.em.bn

import Factor._
import AssignmentUtil._

object Factor {

  def apply(variable: Var, values: Double*): Factor = {
    Factor(variable :: Nil, List(values: _*))
  }
  def apply(variable1: Var, variable2: Var, values: Double*): Factor = {
    Factor(variable1 :: variable2 :: Nil, List(values: _*))
  }
  def apply(variable1: Var, variable2: Var, variable3: Var, values: Double*): Factor = {
    Factor(variable1 :: variable2 :: variable3 :: Nil, List(values: _*))
  }

  object Var {
    def apply(name: String, values: Tuple2[String, String]): Var = {
      Var(name, values.productIterator.toList.asInstanceOf[List[String]])
    }
    def apply(name: String, values: Tuple3[String, String, String]): Var = {
      Var(name, values.productIterator.toList.asInstanceOf[List[String]])
    }
  }
  /**
   * Defines variable in a Bayesian network.
   *
   * @param name Unique variable identifier.
   * @param values List of all possible discrete variable values.
   *
   */
  case class Var(name: String, values: List[String]) {
    require(!values.isEmpty, "List of variable values can't be empty")
  }
}

/**
 * Represents factorisation over a set of variables in a Bayesian network.
 *
 * @param Vars List of variables over which factor is defined.
 *
 * @param values Values for all variable combinations. Variable values are incremented from right to left, similarly to how binary system works.
 * Example: variables: V1 (0, 1) , V2 (0, 1,2)
 *
 * values:
 * V1 V2 Value
 * -----------
 * 0 0 val1
 * 0 1 val2
 * 0 2 val3
 * 1 0 val4
 * 1 1 val5
 * 1 2 val6
 */
case class Factor(variables: Seq[Var], values: Seq[Double]) {
  val dimensions = variables.map(v => v.values.size)
  require(values.size == dimensions.product, "Number of factor values must be equal to product of variable dimensions")

  /**
   * Produce product of two factors.
   *
   * @return Product of factors.
   */
  def product(factor: Factor): Factor = {

    val newVariables = this.variables.union(factor.variables).distinct
    val newDimensions = newVariables.map(v => v.values.size)

    /** Map variable indices of input factor to variables indices in product factor.*/
    def mapToNewFactor(factor: Factor): Seq[Int] = {
      factor.variables.map(v => newVariables.indexOf(v))
    }
    val mapFactorA = mapToNewFactor(this)
    val mapFactorB = mapToNewFactor(factor)

    val factorADimensions = this.dimensions
    val factorBDimensions = factor.variables.map(v => v.values.size)

    val assignments = computeAllAssignments(newDimensions)

    val values = assignments.map { a =>
      val factorAAssignment = mapFactorA.map(indice => a(indice))
      val factorBAssignment = mapFactorB.map(indice => a(indice))
      val factorAValue = this.values(assignmentToIndex(factorAAssignment, factorADimensions))
      val factorBValue = factor.values(assignmentToIndex(factorBAssignment, factorBDimensions))

      val factorValue = factorAValue * factorBValue
      factorValue
    }

    Factor(newVariables, values)
  }

  /**
   * Multiply this factor by all factors in a 'factors' parameter.
   *
   * @return Product of factors.
   *
   */
  def product(factors: Seq[Factor]): Factor = {
    factors.foldLeft(this)((factorProduct, factor) => factorProduct.product(factor))
  }

  /**
   * Set probabilities for all factor entries not consistent with evidence to 0.
   *
   * @param evidence List of tuples. Tuple2[variableName, variableValue]
   *
   * @return Factor consistent with evidence.
   */
  def evidence(evidence: List[Tuple2[String, String]]): Factor = {

    val newValues = evidence.foldLeft(values) { (values, evidence) =>

      val evidenceVarIndex = variables.findIndexOf(v => v.name.equals(evidence._1))
      val evidenceValueIndex = variables(evidenceVarIndex).values.findIndexOf(v => v.equals(evidence._2))

      values.zipWithIndex.map {
        case (v, index) =>
          val assignment = indexToAssignment(index, dimensions)
          if (assignment(evidenceVarIndex) == evidenceValueIndex) v else 0
      }
    }
    val newFactor = this.copy(values = newValues)
    newFactor
  }

  /**
   * Sum out all variables not included in 'variableNames' parameter.
   *
   * @return Marginals over variables specified by 'variableNames' parameter.
   *
   */
  def marginal(variableNames: List[String]): Factor = {
    require(!variableNames.isEmpty, "List of marginal variables is empty")

    /**Tuple2 [variable, index]*/
    val marginalVariables: Seq[Tuple2[Var, Int]] = variables.zipWithIndex.filter { case (v, index) => variableNames.contains(v.name) }
    val marginalDimensions = marginalVariables.map(v => v._1.values.size)

    /**List of Tuple2 [Marginal assignment, value]*/
    val assignmentMapping: Seq[Tuple2[Seq[Int], Double]] = values.zipWithIndex.map {
      case (v, i) =>
        val assignment = indexToAssignment(i, dimensions)
        val marginalAssignment = marginalVariables.map(v => assignment(v._2))

        marginalAssignment -> v
    }

    val marginalAssignmentValues = assignmentMapping.groupBy(m => m._1).mapValues(v => v.map(v => v._2).sum)
    val marginalValues = marginalAssignmentValues.toList.sortBy(v => assignmentToIndex(v._1, marginalDimensions)).map { case (a, v) => v }
    println(marginalValues)
    val marginalFactor = Factor(marginalVariables.map(v => v._1), marginalValues)
    marginalFactor
  }

  /**Normalize all factor values so they sum up to 1.*/
  def normalize(): Factor = {
    val normConst = values.sum
    val normValues = values.map(v => v / normConst)
    this.copy(values = normValues)
  }
}