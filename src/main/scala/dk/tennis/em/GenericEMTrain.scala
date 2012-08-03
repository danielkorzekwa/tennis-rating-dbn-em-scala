package dk.tennis.em

import EMTrain._
import scala.annotation.tailrec
import dbn._
import dbn.DbnTennis._
import dk.tennis.em.dbn.GenericInferDbnTennis

/**
 * @see EMTrain
 */
class GenericEMTrain extends EMTrain {

  /** @see EMTrain */
  def train(parameters: Params, results: List[Result], iterNum: Int, progress: (Int, Double) => Unit): Params = {

    @tailrec
    def trainIteration(parameters: Params, iter: Int): Params = {

      val sufficientStats = expectationStep(parameters, results)
      val newParameters = maximizationStep(sufficientStats)
      progress(iter, 0d)
      if (iter < iterNum) trainIteration(newParameters, iter + 1) else newParameters
    }

    val newParameters = trainIteration(parameters, 1)
    newParameters
  }

  /** @see EMTrain */
  def expectationStep(parameters: Params, results: List[Result]): SufficientStats = {

    val dbnTennis = new GenericDbnTennis(parameters.priorProb, parameters.emissionProb, parameters.transitionProb)
    results.foreach(r => dbnTennis.addResult(r))

    val priorRatings = GenericInferDbnTennis.getRatingPriorProbabilities(dbnTennis.getFactors())

    val priorRatingsStats = priorRatings.reduceLeft { (reduced, rating) =>
      reduced.zip(rating).map(e => e._1 + e._2)
    }

    SufficientStats(priorRatingsStats, priorRatings.size, Nil, 0, Nil, 0)
  }

  /** @see EMTrain */
  def maximizationStep(sufficientStats: SufficientStats): Params = Params(Nil, Nil, Nil)
}