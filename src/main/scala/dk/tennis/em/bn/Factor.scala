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
  val stepSizes: Seq[Int] = calcStepSizes(dimensions)

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

      /**Returns mapping between new variables and step sizes for dimensions of factors A and B*/
      val factorAStepMappings = calcStepMappings(factorA.variables, factorA.stepSizes, newVariables)
      val factorBStepMappings = calcStepMappings(factorB.variables, factorB.stepSizes, newVariables)

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

    val marginalVariables: Seq[Var] = variables.filter { v => variableNames.contains(v.name) }
    val marginalDimensions = marginalVariables.map(v => v.values.size)
    val marginalValues = new Array[Double](marginalDimensions.product)

    val marginalStepSizes: Seq[Int] = calcStepSizes(marginalDimensions)
    val marginalStepMappings = calcStepMappings(marginalVariables, marginalStepSizes, variables)

    var marginalIndex = 0
    var assignment: Array[Int] = new Array(dimensions.size)

    val dimProduct = dimensions.product
    var i = 0
    while (i < dimProduct) {

      marginalValues(marginalIndex) += values(i)

      var dimIndex = dimensions.size - 1
      var continue = true
      while (continue && dimIndex >= 0) {

        if (assignment(dimIndex) != dimensions(dimIndex) - 1) {
          assignment(dimIndex) += 1
          marginalIndex += marginalStepMappings(dimIndex)
          continue = false
        } else {
          marginalIndex -= (dimensions(dimIndex) - 1) * marginalStepMappings(dimIndex)
          assignment(dimIndex) = 0
        }

        dimIndex -= 1
      }

      i += 1
    }

    Factor(marginalVariables, marginalValues)
  }

  /**
   * Calculates step sizes for dimensions.
   * @returns Step size - how many steps are there before reaching next assignment for a given dimension.
   */
  private def calcStepSizes(dimensions: Seq[Int]): Seq[Int] = dimensions.zipWithIndex.map {
    case (d, index) => dimensions.drop(index + 1).product
  }

  /**Returns mapping between factor and variables*/
  def calcStepMappings(factorVariables: Seq[Var], factorStepSizes: Seq[Int], variables: Seq[Var]): Seq[Int] =
    variables.map { v =>
      val varMappingIndex = factorVariables.indexOf(v)
      if (varMappingIndex >= 0) factorStepSizes(varMappingIndex) else 0
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