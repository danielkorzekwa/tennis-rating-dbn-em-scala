package dk.tennis.em.bn

import Factor._
import AssignmentUtil._
import scala.util.control.Breaks._

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
  case class Var(name: String, values: Seq[String]) {
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

  /**
   * Step sizes for all dimensions of this factor.
   * Step size - how many steps are there before reaching next assignment for a given dimension.
   */
  val stepSizes: Seq[Int] = dimensions.zipWithIndex.map {
    case (d, index) => dimensions.drop(index + 1).product
  }
  require(values.size == dimensions.product, "Number of factor values must be equal to product of variable dimensions")
  /**
   * Multiply this factor by all factors in a 'factors' parameter.
   *
   * @return Product of factors.
   *
   */
  def product(factors: Factor*): Factor = {

    def productTwoFactors(factorA: Factor, factorB: Factor): Factor = {
      val newVariables = factorA.variables.union(factorB.variables).distinct
      val newDimensions = newVariables.map(v => v.values.size)

      /**Returns mapping between new dimensions and step sizes for dimensions of factors A and B*/
      def factorStepMappings(factor: Factor): Seq[Int] = newVariables.map { newVar =>
        val varMappingIndex = factor.variables.indexOf(newVar)
        if (varMappingIndex >= 0) factor.stepSizes(varMappingIndex) else 0
      }
      val factorAStepMappings = factorStepMappings(factorA)
      val factorBStepMappings = factorStepMappings(factorB)

      val dimProduct = newDimensions.product
      val values = new Array[Double](dimProduct)
      var indexFactorA = 0
      var indexFactorB = 0
      var assignment: Array[Int] = new Array(newDimensions.size)
      var i = 0

      while (i < dimProduct) {

        val factorValue = factorA.values(indexFactorA) * factorB.values(indexFactorB)

        var dimIndex = newDimensions.size - 1
        var continue = true
        while (continue && dimIndex >= 0) {

          if (assignment(dimIndex) != newDimensions(dimIndex) - 1) {
            assignment(dimIndex) += 1
            indexFactorA += factorAStepMappings(dimIndex)
            indexFactorB += factorBStepMappings(dimIndex)
            continue = false
          } else {
            indexFactorA -= (newDimensions(dimIndex) - 1) * factorAStepMappings(dimIndex)
            indexFactorB -= (newDimensions(dimIndex) - 1) * factorBStepMappings(dimIndex)
            assignment(dimIndex) = 0
          }

          dimIndex -= 1
        }

        values(i) = factorValue
        i += 1
      }

      Factor(newVariables, values)
    }
    factors.foldLeft(this)((factorProduct, factor) => productTwoFactors(factorProduct, factor))
  }

  /**
   * Set probabilities for all factor entries not consistent with evidence to 0.
   *
   * @param evidence List of tuples. Tuple2[variableName, variableValue]
   *
   * @return Factor consistent with evidence.
   */
  def evidence(evidence: Tuple2[String, String]*): Factor = {

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
  def marginal(variableNames: String*): Factor = {
    require(!variableNames.isEmpty, "List of marginal variables is empty")

    /**Tuple2 [variable, index]*/
    val marginalVariables: Seq[Tuple2[Var, Int]] = variables.zipWithIndex.filter { case (v, index) => variableNames.contains(v.name) }
    val marginalDimensions = marginalVariables.map(v => v._1.values.size)

    /**List of Tuple2 [Marginal assignment, value]*/
    val assignmentMapping = computeAllAssignments(dimensions).map(a => marginalVariables.map(v => a(v._2))).zip(values)

    val marginalAssignmentValues = assignmentMapping.groupBy(m => m._1).mapValues(v => v.map(v => v._2).sum)
    val marginalValues = marginalAssignmentValues.toList.sortBy(v => assignmentToIndex(v._1, marginalDimensions)).map { case (a, v) => v }

    val marginalFactor = Factor(marginalVariables.map(v => v._1), marginalValues)

    marginalFactor
  }

  /**Normalize all factor values so they sum up to 1.*/
  def normalize(): Factor = {
    val normConst = values.sum
    val normValues = values.map(v => v / normConst)
    this.copy(values = normValues)
  }

  /**Normalize all factor values so they form a conditional probability table.*/
  def toCPD(): Factor = {
    val sliceSize = variables.last.values.size

    val normValues = values.grouped(sliceSize).map { slice =>
      slice.map(elem => elem / slice.sum)
    }.flatten.toSeq

    this.copy(values = normValues)
  }
}