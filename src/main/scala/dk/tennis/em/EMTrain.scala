package dk.tennis.em

import EMTrain._

/**
 * Learn parameters for Dynamic Bayesian Network, which models tennis ratings and match results over the time.
 * Go here for more details: https://github.com/danielkorzekwa/tennis-rating-dbn-em-scala/wiki
 * Also refer to:
 *  - http://en.wikipedia.org/wiki/Baum%E2%80%93Welch_algorithm
 *  - 'Probabilistic Graphical Models: Principles and Techniques' book by Daphne Koller and Nir Friedman
 */

object EMTrain {

  case class Rating(ratingValue: Int, prob: Double)
  case class EmissionProb(ratingOnServe: Int, ratingOnReturn: Int, prob: Double)
  class TransitionProb(currRating: Int, prevRating: Int, prob: Double)

  case class Params(priorProb: List[Rating], emissionProb: List[EmissionProb], transitionProb: List[TransitionProb])

  case class SufficientStats

  case class Result
}

trait EMTrain {

  /**
   * Learn parameters with EM algorithm.
   *
   * @param parameters Parameters (prior, emission and transition) to be learned with Expectation Maximization.
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
   * @param parameters Parameters (prior, emission and transition),
   * which are used for building dynamic bayesian network and performing bayesian inference for a purpose of collecting sufficient statistics.
   *
   * @param results Tennis results used as observed variables in dynamic bayesian network, which represents player's skills and tennis match results over the time.
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