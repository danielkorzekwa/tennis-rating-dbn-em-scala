package dk.tennis.em

import EMTrain._
import scala.annotation.tailrec

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
    SufficientStats()
  }

  /** @see EMTrain */
  def maximizationStep(sufficientStats: SufficientStats): Params = Params(Nil, Nil, Nil)
}