package dk.tennis.em.dbn
import dk.tennis.em.bn.Factor
import scala.Math._

case class GenericInferDbnTennis(factors: Seq[Factor]) extends InferDbnTennis {

  require(!factors.isEmpty, "List of factors is empty")

  val fullJoin = factors.head.product(factors.tail: _*)

  /**@see InferDbnTennis.*/
  def getRatingPriorProbabilities(): Seq[Seq[Double]] = {

    val priorFactors = marginalizeFactors(factors, 1)

    val emissionProbs = priorFactors.map(f => f.values)
    emissionProbs

  }

  /**@see InferDbnTennis.*/
  def getScoreEmissionProbabilities(): Seq[Seq[Double]] = {

    val emissionFactorsWithEvidence = marginalizeFactors(factors, 3)

    val emissionProbs = emissionFactorsWithEvidence.map(f => f.values)
    emissionProbs
  }

  /**@see InferDbnTennis.*/
  def getRatingTransitionProbabilities(): Seq[Seq[Double]] = {

    val transitionFactors = marginalizeFactors(factors, 2)

    val emissionProbs = transitionFactors.map(f => f.values)
    emissionProbs

  }

  /**@see InferDbnTennis.*/
  def logLikelihood(): Double = log(fullJoin.values.sum)

  /**Find all factors with a given number of variables and marginalize them from a full join distribution.*/
  private def marginalizeFactors(factors: Seq[Factor], factorVarNum: Int): Seq[Factor] = {

    val filteredFactors = factors.filter(f => f.variables.size == factorVarNum)

    val marginalsWithEvidence = filteredFactors.map(f => fullJoin.marginal(f.variables.map(v => v.name): _*).normalize())

    marginalsWithEvidence

  }

}