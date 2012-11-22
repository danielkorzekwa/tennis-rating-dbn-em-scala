package dk.tennis.dbn.em

import TennisEM._
import dk.tennis.dbn.MatchOutcome
import dk.tennis.dbn.TennisDBN
import dk.tennis.dbn.clustergraph.TennisClusterGraph

/**
 * Learn parameters for Dynamic Bayesian Network, which models tennis ratings and match results over the time.
 * Go here for more details: https://github.com/danielkorzekwa/tennis-rating-dbn-em-scala/wiki
 * Also refer to:
 *  - http://en.wikipedia.org/wiki/Baum%E2%80%93Welch_algorithm
 *  - 'Probabilistic Graphical Models: Principles and Techniques' book by Daphne Koller and Nir Friedman
 *
 *  @author Daniel Korzekwa
 */
trait TennisEM {

  /**
   * Learn parameters in Tennis DBN network with EM algorithm.
   *
   * @param tennisClusterGraph Represents tennis results over the time
   * @param iterNum Number of iterations, for which EM is executed
   * @progress Allows for tracking progress of algorithm. (currentIteration, Log likelihood) => Unit
   *
   * @return Learnt parameters (prior, emission and transition)
   *
   */
  def train(tennisClusterGraph: TennisClusterGraph, iterNum: Int, progress: (Int, Double) => Unit): Params
}

object TennisEM {

  /**
   * Represents prior, emission and transition parameters in a Tennis DBN network.
   *
   * Parameters example:
   * priorProb = 0.2 0.5 0.3 //three rating values: 0,1,2 with prior probabilities 0.2 0.5 0.3 respectively.
   *
   * Tennis match score (emission probabilities, 18 values) specified in the following order:
   *
   * playerA_rating, playeB_rating, win/lose
   * 0,0,w - 0.5
   * 0,0,l - 0.5
   * 0,1,w - 1/3
   * 0,1,l - 2/3
   * 0,2,w - 0.25
   * 0,2,l - 0.75
   * 1,0,w - 2/3
   * 1,0,l - 1/3
   * ...
   *
   * Rating transition probabilities (9 values) specified in the following order:
   * player_old_rating,player_new_rating
   * 0,0 - 0.98
   * 0,1 - 0.01
   * 0,2 - 0.01
   * 1,0 - 0.01
   * 1,1 - 0.97
   * 1,2 - 0.02
   * ...
   *
   */
  case class Params(priorProb: Array[Double], emissionProb: Array[Double], transitionProb: Array[Double])

}