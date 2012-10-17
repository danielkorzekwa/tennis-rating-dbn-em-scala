package dk.tennis.em.loopybp

import org.junit._
import dk.tennis.em.loopybp._
import Assert._
import dk.tennis.em.bn.Factor
import dk.tennis.em.bn.Factor._
import ClusterGraph._
import GenericClusterGraph._
import dk.tennis.em.util.VectorAssert._
class GenericClusterGraphTennisNetworkTest {

  private val priorProb = List(0.2, 0.5, 0.3)

  private val emissionProb = List(
    0.5, 0.5,
    1d / 3, 2d / 3,
    0.25, 0.75,
    2d / 3, 1d / 3,
    0.5, 0.5,
    2d / 5, 3d / 5,
    3d / 4, 1d / 4,
    3d / 5, 2d / 5,
    0.5, 0.5)

  def progress(iterNum: Int): Unit = println("Iter: " + iterNum)

  @Test def single_tennis_result {
    val player1Var = Var("Player1", ("1", "2", "3"))
    val player2Var = Var("Player2", ("1", "2", "3"))
    val scoreVar = Var("Score", ("W", "L"))

    val clusterPlayer1 = Cluster(1, Factor(List(player1Var), priorProb))
    val clusterPlayer2 = Cluster(2, Factor(List(player2Var), priorProb))
    val clusterScore = Cluster(3, Factor(List(player1Var, player2Var, scoreVar), emissionProb).evidence(("Score", "W")))

    val edges = List((1, 3), (2, 3))

    val clusterGraph = GenericClusterGraph(List(clusterPlayer1, clusterPlayer2, clusterScore), edges)
    val calibratedClusterGraph = clusterGraph.calibrate(progress)

    //Check cluster beliefs
    vectorAssert(List(0.1366, 0.5033, 0.36), calibratedClusterGraph.clusterBelief(1).values, 0.0001)
    vectorAssert(List(0.2633, 0.49666, 0.24), calibratedClusterGraph.clusterBelief(2).values, 0.0001)
    vectorAssert(List(0.04, 0, 0.0666, 0, 0.03, 0, 0.1333, 0, 0.25, 0, 0.12, 0, 0.09, 0, 0.18, 0, 0.09, 0), calibratedClusterGraph.clusterBelief(3).values, 0.0001)

    //Check log likelihood
    val assignment = List(Assignment("Player1", "1"), Assignment("Player2", "1"), Assignment("Score", "W"))
    val llh = calibratedClusterGraph.logLikelihood(assignment)
    assertEquals(-3.2188, llh, 0.0001)

    //Check marginals
    assertEquals(List(Var("Player1", ("1", "2", "3"))), calibratedClusterGraph.marginal(player1Var.name).variables)
    vectorAssert(List(0.1366, 0.5033, 0.36), calibratedClusterGraph.marginal(player1Var.name).values, 0.0001)

    assertEquals(List(Var("Player2", ("1", "2", "3"))), calibratedClusterGraph.marginal(player2Var.name).variables)
    vectorAssert(List(0.2633, 0.49666, 0.24), calibratedClusterGraph.marginal(player2Var.name).values, 0.0001)

    assertEquals(List(Var("Score", ("W", "L"))), calibratedClusterGraph.marginal(scoreVar.name).variables)
    vectorAssert(List(1.0, 0),
      calibratedClusterGraph.marginal(scoreVar.name).values, 0.0001)
  }

  @Test def two_tennis_results_in_a_single_time_slice {
    val player1Var = Var("Player1", ("1", "2", "3"))
    val player2Var = Var("Player2", ("1", "2", "3"))
    val player3Var = Var("Player3", ("1", "2", "3"))
    val score12Var = Var("Score12", ("W", "L"))
    val score13Var = Var("Score13", ("W", "L"))

    val clusterPlayer1 = Cluster(1, Factor(List(player1Var), priorProb))
    val clusterPlayer2 = Cluster(2, Factor(List(player2Var), priorProb))
    val clusterPlayer3 = Cluster(3, Factor(List(player3Var), priorProb))
    val cluster12Score = Cluster(4, Factor(List(player1Var, player2Var, score12Var), emissionProb).evidence(("Score12", "W")))
    val cluster13Score = Cluster(5, Factor(List(player1Var, player3Var, score13Var), emissionProb).evidence(("Score13", "W")))

    val clusters = List(clusterPlayer1, clusterPlayer2, clusterPlayer3, cluster12Score, cluster13Score)
    val edges = List((1, 4), (2, 4), (1, 5), (3, 5))

    val clusterGraph = GenericClusterGraph(clusters, edges)
    val calibratedClusterGraph = clusterGraph.calibrate(progress)

    vectorAssert(List(0.0904, 0.4909, 0.4185), calibratedClusterGraph.clusterBelief(1).values, 0.0001)
    vectorAssert(List(0.2611, 0.4972, 0.2415), calibratedClusterGraph.clusterBelief(2).values, 0.0001)
    vectorAssert(List(0.2611, 0.4972, 0.2415), calibratedClusterGraph.clusterBelief(3).values, 0.0001)

    vectorAssert(List(0.0265, 0.0000, 0.0441, 0.0000, 0.0199, 0.0000, 0.1301, 0.0000, 0.2438, 0.0000, 0.1170, 0.0000, 0.1046, 0.0000, 0.2093, 0.0000, 0.1046, 0.0000), calibratedClusterGraph.clusterBelief(4).values, 0.0001)
    vectorAssert(List(0.0265, 0.0000, 0.0441, 0.0000, 0.0199, 0.0000, 0.1301, 0.0000, 0.2438, 0.0000, 0.1170, 0.0000, 0.1046, 0.0000, 0.2093, 0.0000, 0.1046, 0.0000), calibratedClusterGraph.clusterBelief(5).values, 0.0001)

    val assignment = List(Assignment("Player1", "1"), Assignment("Player2", "1"), Assignment("Player3", "1"), Assignment("Score12", "W"), Assignment("Score13", "W"))
    val llh = calibratedClusterGraph.logLikelihood(assignment)
    assertEquals(-4.8598, llh, 0.0001)
  }

  /**
   * Test for log likelihood of evidence
   */
  @Test def single_tennis_result_loglikelihood_of_evidence {
    val player1Var = Var("Player1", ("1", "2", "3"))
    val player2Var = Var("Player2", ("1", "2", "3"))
    val scoreVar = Var("Score", ("W", "L"))

    val clusterPlayer1 = Cluster(1, Factor(List(player1Var), priorProb))
    val clusterPlayer1WithEvidence = Cluster(1, Factor(List(player1Var), priorProb).evidence(("Player1", "2")))

    val clusterPlayer2 = Cluster(2, Factor(List(player2Var), priorProb))

    val clusterScore = Cluster(3, Factor(List(player1Var, player2Var, scoreVar), emissionProb))
    val clusterScoreKnown = Cluster(3, Factor(List(player1Var, player2Var, scoreVar), emissionProb).evidence(("Score", "W")))

    val edges = List((1, 3), (2, 3))

    val clusterGraphNoEvidence = GenericClusterGraph(List(clusterPlayer1, clusterPlayer2, clusterScore), edges)
    val clusterGraph = GenericClusterGraph(List(clusterPlayer1WithEvidence, clusterPlayer2, clusterScoreKnown), edges)
    val calibratedClusterGraph = clusterGraph.calibrate(progress)

    val assignment = List(Assignment("Player1", "2"), Assignment("Player2", "1"), Assignment("Score", "W"))

    val llhProduct = clusterGraphNoEvidence.logLikelihood(assignment)
    val llhEvidence = calibratedClusterGraph.logLikelihood(assignment)

    assertEquals(-2.7080, llhProduct, 0.0001)
    assertEquals(-1.3284, llhEvidence, 0.0001)
    assertEquals(-1.3796, llhProduct - llhEvidence, 0.0001)
  }
}