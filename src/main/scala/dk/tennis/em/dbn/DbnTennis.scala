package dk.tennis.em.dbn

import dk.tennis.em.EMTrain._
import DbnTennis._
import dk.tennis.em.bn.Factor

/**
 * Dynamic Bayesian Network for tennis players and match results over the time.
 * @author korzekwad
 */
object DbnTennis {
  case class Result(playerA: String, playerB: String, playerAWinner: Boolean, timeSlice: Int)
}

trait DbnTennis {

  /**
   * Returns rating prior probabilities for all players.
   *
   * @return Seq[player rating prior probabilities]
   *
   * Player rating prior probabilities example: List(0.2,0.5,0.3). Three rating values: 0,1,2 with probabilities 0.2 0.5 0.3 respectively.
   *
   */
  def getRatingPriorProbabilities(): Seq[Seq[Double]]

  /**
   * Add tennis result between two tennis players to dynamic bayesian network.
   * @param result
   */
  def addResult(result: Result)

  /**
   * Gets all player ids in a bayesian network.
   *
   */
  def getPlayerIds(): List[String]
  
  /**Returns underlying list of factors for dynamic bayesian network.*/
  def getFactors():List[Factor]
  
  /**Returns results.*/
  def getResults():List[Result]
  

}