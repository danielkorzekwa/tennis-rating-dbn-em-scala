package dk.tennis.em.dbn.factorgraph

import dk.tennis.em.EMTrain._
import dk.tennis.em.bn.Factor
import dk.tennis.em.bn.Factor._
import DbnTennis._

/**
 * Dynamic Bayesian Network for tennis players and match results over the time.
 * @author korzekwad
 */

object DbnTennis {
  object Result {
    def apply(playerA: String, playerB: String, playerAWinner: Boolean, timeSlice: Int): Result =
      Result(playerA, playerB, Option(playerAWinner), timeSlice)
  }
  case class Result(playerA: String, playerB: String, playerAWinner: Option[Boolean], timeSlice: Int)
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
  def getFactors(): List[Factor]

  /**Returns results.*/
  def getResults(): List[Result]

  /**
   * Returns the mapping between results and result variables in a factor graph.
   * It allows for calculating the winning probability of a tennis match by a tennis player.
   */
  def getResultVariables(): Map[Result, Var]

  /**
   * Returns variables for all players and all time slices.
   *
   * @return Map[time slice,Map[playerName,player variable]]
   */
  def getPlayerVariables(): Map[Int, Map[String, Var]]

}