package dk.tennis.dbn

import dk.bayes.factor._
import dk.bayes.clustergraph.ClusterGraph
import dk.bayes.clustergraph.Cluster
import clustergraph.TennisClusterGraph

/**
 * Dynamic Bayesian Network for tennis players and match results over the time.
 * @author korzekwad
 */

trait TennisDBN {

  /**
   * Add tennis match between two tennis players to Dynamic Bayesian Betwork.
   * @param result
   */
  def addMatch(matchOutcome: MatchOutcome)

  def updateMatch(matchOutcome: MatchOutcome)

  def getTennisClusterGraph(): TennisClusterGraph

  def calibrate(iterNum: (Int) => Unit = (iterNum: Int) => {})
  def getPlayerRating(playerName: String, matchTime: Long): Array[Double]
  def getMatchProbability(playerA: String, playerB: String, matchTime: Long): Double

}