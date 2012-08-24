package dk.tennis.em.dbn

import dk.tennis.em.bn.Factor

/**
 * Performs bayesian inference in dynamic bayesian network representing tennis players and matches over the time.
 *
 */
object InferDbnTennis {
   case class Result(playerA: String, playerB: String, playerAWinner: Boolean, timeSlice: Int)
}
trait InferDbnTennis {

  /**
   * Returns normalised rating prior probabilities for all players.
   *
   *
   * @return Seq[player rating prior probabilities]
   * Example rating prior probabilities for two players: List(0.2,0.5,0.3) :: List(0.1,0.2,0.7) :: Nil.
   * Three rating values: 0,1,2 with probabilities 0.2 0.5 0.3 respectively for the first player.
   *
   */
  def getRatingPriorProbabilities(): Seq[Seq[Double]]

  /**
   * Returns normalised score emission probabilities for all scores.
   *
   * @return Seq[score emission probabilities]
   *
   * Example score emission probabilities for two scores, given there are three prior probability values for each of the two players.
   * In this example, there are 18 values of emission probabilities in total for each score:
   *
   *   List(0.04, 0, 0.0666, 0, 0.03, 0, 0.1333, 0, 0.25, 0, 0.12, 0, 0.09, 0, 0.18, 0, 0.09, 0) ::
   *   List(0, 0.0517, 0, 0.1725, 0, 0.1164, 0, 0.0634, 0, 0.2379, 0, 0.1713, 0, 0.0233, 0, 0.0932, 0, 0.0699) :: Nil
   *
   * Emission probabilities (18 values, for the example above), are specified in the following order:
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
   */
  def getScoreEmissionProbabilities(): Seq[Seq[Double]]

  /**
   * Returns normalised transition probabilities between player rating probabilities in times t and t + 1.
   *
   *
   * @return Seq[score emission probabilities]
   *
   * Rating transition probabilities (9 values if a single player rating consists of three probabilities) specified in the following order:
   * player_old_rating,player_new_rating
   * 0,0 - 0.98
   * 0,1 - 0.01
   * 0,2 - 0.01
   * 1,0 - 0.01
   * 1,1 - 0.97
   * 1,2 - 0.02
   * ...
   */
  def getRatingTransitionProbabilities(): Seq[Seq[Double]]
  
  /**Calculates log likelihood of a dbn network.*/
  def logLikelihood():Double

}