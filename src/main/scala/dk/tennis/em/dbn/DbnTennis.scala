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