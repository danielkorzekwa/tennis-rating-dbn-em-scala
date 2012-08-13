package dk.tennis.em

import EMTrain._
import dbn.DbnTennis._

/**
 * Learn parameters for Dynamic Bayesian Network, which models tennis ratings and match results over the time.
 * Go here for more details: https://github.com/danielkorzekwa/tennis-rating-dbn-em-scala/wiki
 * Also refer to:
 *  - http://en.wikipedia.org/wiki/Baum%E2%80%93Welch_algorithm
 *  - 'Probabilistic Graphical Models: Principles and Techniques' book by Daphne Koller and Nir Friedman
 */

object EMTrain {

  /**
   *  Refer to DbnTennis on how prior,emission and transition probabilities are represented.
   */
  case class Params(priorProb: Seq[Double], emissionProb: Seq[Double], transitionProb: Seq[Double])

  /**
   * @param priorStats
   * @param priorStatsNum Number of cases (prior rating variables) used for aggregating prior statistics.
   * @param emissionStats
   * @param emissionStatsNum
   * @param transitionStats
   * @param transitionStatsNum
   *
   * Refer to DbnTennis on how prior,emission and transition stats are represented.
   */
  case class SufficientStats(priorStats: Seq[Double], priorStatsNum: Int, emissionStats: Seq[Double], emissionStatsNum: Int,
    transitionStats: Seq[Double], transitionStatsNum: Int)
}

trait EMTrain {

  /**
   * Learn parameters with EM algorithm.
   *
   * @param parameters Parameters (prior, emission and transition) to be learned with Expectation Maximizatio algorithm.
   * @param results Tennis results used as observed variables in dynamic bayesian network, which represents player's skills and tennis match results over the time.
   * @param iterNum Number of iterations, for which EM is executed.
   * @progress Allows for tracking progress of algorithm. (currentIteration, Log likelihood) => Unit
   *
   * @return Learnt parameters (prior, emission and transition).
   *
   */
  def train(parameters: Params, results: List[Result], iterNum: Int, progress: (Int, Double) => Unit): Params

  /**
   * E-step of EM algorithm.
   *
   * @param parameters Parameters (prior, emission and transition) used for building dynamic bayesian network
   * and performing bayesian inference, for a purpose of collecting sufficient statistics.
   *
   * @param results Tennis results used as observed variables in dynamic bayesian network.
   * They represent player's skills and tennis match results over the time.
   *
   * @return Sufficient statistics (Refer to EM algorithm, 'Probabilistic Graphical Models: Principles and Techniques' book by Daphne Koller and Nir Friedman)
   */
  def expectationStep(parameters: Params, results: List[Result]): SufficientStats

  /**
   * M-step of EM algorithm.
   *
   * @param sufficientStats Sufficient statistics used for estimating new values of parameters (prior, emission and transition).
   *
   * @return New values of estimated parameters (prior, emission and transition).
   */
  def maximizationStep(sufficientStats: SufficientStats): Params
}