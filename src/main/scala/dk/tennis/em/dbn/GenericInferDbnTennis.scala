package dk.tennis.em.dbn
import dk.tennis.em.bn.Factor

object GenericInferDbnTennis extends InferDbnTennis {

  /**@see InferDbnTennis.*/
  def getRatingPriorProbabilities(factors: Seq[Factor]): Seq[Seq[Double]] = {

    val priorFactors = marginalizeFactors(factors, 1)

    val emissionProbs = priorFactors.map(f => f.values)
    emissionProbs

  }

  /**@see InferDbnTennis.*/
  def getScoreEmissionProbabilities(factors: Seq[Factor]): Seq[Seq[Double]] = {

    val emissionFactorsWithEvidence = marginalizeFactors(factors, 3)

    val emissionProbs = emissionFactorsWithEvidence.map(f => f.values)
    emissionProbs
  }

  /**@see InferDbnTennis.*/
  def getRatingTransitionProbabilities(factors: Seq[Factor]): Seq[Seq[Double]] = {

    val transitionFactors = marginalizeFactors(factors, 2)

    val emissionProbs = transitionFactors.map(f => f.values)
    emissionProbs

  }

  /**Find all factors with a given number of variables and marginalize them from a full join distribution.*/
  private def marginalizeFactors(factors: Seq[Factor], factorVarNum: Int): Seq[Factor] = {
    factors.isEmpty match {
      case true => Nil
      case false => {

        val marginals = factors.filter(f => f.variables.size == factorVarNum)

        val fullJoin = factors.head.product(factors.tail: _*)

        val marginalsWithEvidence = marginals.map(f => fullJoin.marginal(f.variables.map(v => v.name): _*).normalize())

        marginalsWithEvidence
      }
    }
  }

}