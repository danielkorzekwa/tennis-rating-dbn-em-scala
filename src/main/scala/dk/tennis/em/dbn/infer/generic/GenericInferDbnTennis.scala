package dk.tennis.em.dbn.infer.generic
import dk.tennis.em.bn.Factor
import scala.Math._
import dk.tennis.em.dbn._
import infer.InferDbnTennis

case class GenericInferDbnTennis(factors: Seq[Factor]) extends InferDbnTennis {

  require(!factors.isEmpty, "List of factors is empty")

  val fullJoin = factors.head.product(factors.tail)

  /**@see InferDbnTennis.*/
  def getRatingPriorProbabilities(): Seq[Seq[Double]] = {

    val priorFactors = marginalizeFactors(factors, 1)

    val priorProbs = priorFactors.map(f => f.values.toSeq)
    priorProbs

  }

  /**@see InferDbnTennis.*/
  def getScoreEmissionProbabilities(): Seq[Seq[Double]] = {

    val emissionFactorsWithEvidence = marginalizeFactors(factors, 3)

    val emissionProbs = emissionFactorsWithEvidence.map(f => f.values.toSeq)
    emissionProbs
  }

  /**@see InferDbnTennis.*/
  def getRatingTransitionProbabilities(): Seq[Seq[Double]] = {

    val transitionFactors = marginalizeFactors(factors, 2)

    val transitionProbs = transitionFactors.map(f => f.values.toSeq)
    transitionProbs

  }

  /**@see InferDbnTennis.*/
  def logLikelihood(): Double = log(fullJoin.values.sum)

  /**Find all factors with a given number of variables and marginalize them from a full join distribution.*/
  private def marginalizeFactors(factors: Seq[Factor], factorVarNum: Int): Seq[Factor] = {

    val filteredFactors = factors.filter(f => f.variables.size == factorVarNum)

    val marginalsWithEvidence = filteredFactors.map(f => fullJoin.marginal(f.variables.map(v => v.id)).normalize())

    marginalsWithEvidence

  }
  
  /** @see InferDbnTennis*/
  def getPlayerAWinningProb(playerA:String, playerB:String, t:Int):Double = throw new UnsupportedOperationException("Not implemented yet.")

  /** @see InferDbnTennis*/
  def getPlayerRating(playerName:String,timeSlice:Int):Seq[Double]  = throw new UnsupportedOperationException("Not implemented yet.")
}