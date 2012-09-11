package dk.tennis.em.dbn.generic

import dk.tennis.em.EMTrain._
import dk.tennis.em.bn.Factor
import dk.tennis.em.dbn.InferDbnTennis._
import dk.tennis.em.bn.Factor._

/**
 * Dynamic Bayesian Network for tennis players and match results over the time.
 * @author korzekwad
 */
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
  
  /**return Seq[Tuple2[evidence variable, variable assignment index]]*/
  def getEvidenceVariables():Seq[Tuple2[Var,Int]]

}