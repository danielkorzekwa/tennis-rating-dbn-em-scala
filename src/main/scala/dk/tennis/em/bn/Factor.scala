package dk.tennis.em.bn

import Factor._
import AssignmentUtil._
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.LinkedHashSet
import scala.collection._

object Factor {

  def apply(variable: Var, values: Array[Double]): Factor = {
    Factor(Array(variable), values)
  }
  def apply(variable1: Var, variable2: Var, values: Array[Double]): Factor = {
    Factor(Array(variable1, variable2), values)
  }
  def apply(variable1: Var, variable2: Var, variable3: Var, values: Array[Double]): Factor = {
    Factor(Array(variable1, variable2, variable3), values)
  }

  /**
   * Defines variable in a Bayesian network.
   *
   * @param name Unique variable identifier.
   * @param values List of all possible discrete variable values.
   *
   */
  case class Var(id: Int, dim: Int)

  /**
   * This class is used by factor product function to map between
   * variable in a new factor and step sizes for this variables in source factors.
   *
   * @param targetVariable
   * @param factorAStep
   * @param factorBStep
   */
  case class VariableMapping(val targetVariable: Var, var factorAStep: Option[Int], var factorBStep: Option[Int])
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
case class Factor(variables: Array[Var], values: Array[Double]) {

  val stepSizes: Array[Int] = calcStepSizes(variables)

  /**
   * Multiply this factor by all factors in a 'factors' parameter.
   *
   * @return Product of factors.
   *
   */
  def product(factors: Seq[Factor], variableMapping: Option[(Factor, Factor) => Array[VariableMapping]] = None): Factor = {
    factors.foldLeft(this)((factorProduct, factor) => productTwoFactors(factorProduct, factor, variableMapping))
  }
  def productSingle(factor: Factor, variableMapping: Option[(Factor, Factor) => Array[VariableMapping]] = None): Factor = productTwoFactors(this, factor, variableMapping)

  private def productTwoFactors(factorA: Factor, factorB: Factor, variableMapping: Option[(Factor, Factor) => Array[VariableMapping]] = None): Factor = {

    val variablesMappingArray: Array[VariableMapping] = variableMapping match {
      case None => {
        val variablesMappingMap: LinkedHashMap[Int, VariableMapping] = calcUnionVariables(factorA, factorB)
        val variablesMappingArray = new Array[VariableMapping](variablesMappingMap.size)

        var i = 0
        for (u <- variablesMappingMap) {

          variablesMappingArray(i) = u._2

          i += 1
        }
        variablesMappingArray
      }
      case Some(variableMapping) => variableMapping(factorA, factorB)
    }
    /**Map[variableId,variableMapping]*/

    var dimProduct = 1

    val newVariables = new Array[Var](variablesMappingArray.size)
    var i = 0
    while (i < variablesMappingArray.size) {
      val varMapping = variablesMappingArray(i)
      newVariables(i) = varMapping.targetVariable
      dimProduct *= varMapping.targetVariable.dim
      i += 1
    }

    val values = new Array[Double](dimProduct)
    var indexFactorA = 0
    var indexFactorB = 0
    var assignment: Array[Int] = new Array(newVariables.size)
    i = 0
    while (i < dimProduct) {

      val factorValue = factorA.values(indexFactorA) * factorB.values(indexFactorB)

      var dimIndex = newVariables.size - 1
      var continue = true
      while (continue && dimIndex >= 0) {

        val variableMapping = variablesMappingArray(dimIndex)
        if (assignment(dimIndex) != newVariables(dimIndex).dim - 1) {
          assignment(dimIndex) += 1
          if (variableMapping.factorAStep.isDefined) indexFactorA += factorA.stepSizes(variableMapping.factorAStep.get)
          if (variableMapping.factorBStep.isDefined) indexFactorB += factorB.stepSizes(variableMapping.factorBStep.get)
          continue = false
        } else {
          if (variableMapping.factorAStep.isDefined) indexFactorA -= (newVariables(dimIndex).dim - 1) * factorA.stepSizes(variableMapping.factorAStep.get)
          if (variableMapping.factorBStep.isDefined) indexFactorB -= (newVariables(dimIndex).dim - 1) * factorB.stepSizes(variableMapping.factorBStep.get)
          assignment(dimIndex) = 0
        }

        dimIndex -= 1
      }
      //
      values(i) = factorValue
      i += 1
    }

    Factor(newVariables, values)
  }

  private def calcUnionVariables(factorA: Factor, factorB: Factor): LinkedHashMap[Int, VariableMapping] = {

    val unionVariables = new mutable.LinkedHashMap[Int, VariableMapping]

    var i = 0
    while (i < factorA.variables.size) {
      val variable = factorA.variables(i)
      unionVariables(variable.id) = new VariableMapping(variable, Some(i), None)
      i += 1
    }

    i = 0
    while (i < factorB.variables.size) {
      val variable = factorB.variables(i)
      val step = unionVariables.getOrElseUpdate(variable.id, new VariableMapping(variable, None, None))
      step.factorBStep = Some(i)
      i += 1
    }

    unionVariables
  }

  /**
   * Set probabilities for all factor entries not consistent with evidence to 0.
   *
   * @param evidence List of tuples. Tuple2[variableId, variableValueIndex]
   *
   * @return Factor consistent with evidence.
   */
  def evidence(evidence: Tuple2[Int, Int]*): Factor = {

    val dimensions = variables.map(_.dim)

    val newValues = evidence.foldLeft(values) { (values, evidence) =>

      val evidenceVarIndex = variables.findIndexOf(v => v.id == evidence._1)

      values.zipWithIndex.map {
        case (v, index) =>
          val assignment = indexToAssignment(index, dimensions)
          if (assignment(evidenceVarIndex) == evidence._2) v else 0
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
  def marginal(variableId: Int): Factor = marginal(Array(variableId))
  def marginal(variableIds: Array[Int]): Factor = {
    require(!variableIds.isEmpty, "List of marginal variables is empty")

    val marginalVariables = new Array[Var](variableIds.size)

    var marginalDimProduct = 1
    var marginalVarIndex = 0
    var i = 0
    while (i < variables.size) {
      val variable = variables(i)

      var j = 0
      var continue = true
      while (j < variableIds.size && continue) {
        if (variable.id == variableIds(j)) {
          marginalVariables(marginalVarIndex) = variable
          marginalDimProduct *= variable.dim
          marginalVarIndex += 1
          continue = false
        }
        j += 1
      }
      i += 1
    }

    val marginalValues = new Array[Double](marginalDimProduct)

    val marginalStepSizes: Array[Int] = calcStepSizes(marginalVariables)
    val marginalStepMappings = calcStepMappings(marginalVariables, marginalStepSizes, variables)

    val dimensions = variables.map(_.dim)
    var marginalIndex = 0
    var assignment: Array[Int] = new Array(dimensions.size)

    val dimProduct = dimensions.product
    i = 0
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

  /**Returns mapping between factor and variables*/
  def calcStepMappings(factorVariables: Array[Var], factorStepSizes: Array[Int], variables: Array[Var]): Array[Int] = {

    val stepMapping = new Array[Int](variables.size)

    var i = 0
    while (i < variables.size) {
      val variable = variables(i)
      val varMappingIndex = factorVariables.indexWhere(v => v.id == variable.id)
      stepMapping(i) = if (varMappingIndex >= 0) factorStepSizes(varMappingIndex) else 0
      i += 1
    }
    stepMapping
  }
  /**
   * Calculates step sizes for dimensions.
   * @returns Step size - how many steps are there before reaching next assignment for a given dimension.
   */
  private def calcStepSizes(variables: Array[Var]): Array[Int] = {

    val variablesSize = variables.size

    val stepSizes = if (variablesSize == 1) Array(1)
    else {
      val stepSizes = new Array[Int](variablesSize)

      var i = 0
      while (i < variablesSize) {
        stepSizes(i) = 1
        var j = i + 1
        while (j < variablesSize) {
          stepSizes(i) *= variables(j).dim
          j += 1
        }
        i += 1
      }
      stepSizes
    }
    stepSizes

  }

  /**Normalize all factor values so they sum up to 1.*/
  def normalize(): Factor = {
    val normConst = values.sum
    val normValues = values.map(v => v / normConst)
    this.copy(values = normValues)
  }

  /**Normalize all factor values so they form a conditional probability table.*/
  def toCPD(): Factor = {
    val sliceSize = variables.last.dim

    val normValues = values.grouped(sliceSize).map { slice =>
      slice.map(elem => elem / slice.sum)
    }.flatten.toArray

    this.copy(values = normValues)
  }
}