package dk.tennis.dbn

import org.junit._
import Assert._
import dk.bayes.factor.Factor._
import dk.bayes.factor.Factor
import dk.bayes.factor._
import dk.tennis.dbn.util.AssertUtil._

class GenericDbnTennisTest {

  val priorProb = Array(0.2, 0.5, 0.3)

  val emissionProb = Array(
    0.5, 0.5,
    1d / 3, 2d / 3,
    0.25, 0.75,
    2d / 3, 1d / 3,
    0.5, 0.5,
    2d / 5, 3d / 5,
    3d / 4, 1d / 4,
    3d / 5, 2d / 5,
    0.5, 0.5)

  val transitionProb = Array(0.98, 0.01, 0.01, 0.01, 0.98, 0.01, 0.01, 0.02, 0.97)

  val emissionProbForPlayerAWon = Array(0.5000, 0.0000, 0.3333, 0.0000, 0.2500, 0.0000, 0.6667, 0.0000, 0.5000, 0.0000, 0.4000, 0.0000, 0.7500, 0.0000, 0.6000, 0.0000, 0.5000, 0.0000)

  val emissionProbForPlayerALost = Array(0.0000, 0.5000, 0.0000, 0.6667, 0.0000, 0.7500, 0.0000, 0.3333, 0.0000, 0.5000, 0.0000, 0.6000, 0.0000, 0.2500, 0.0000, 0.4000, 0.0000, 0.5000)

  private val dbnTennis = new GenericTennisDBN(priorProb, emissionProb, transitionProb, 1)

  /**
   * Tests for addMatchOutcome.
   */
  @Test def addMatchOutcome_twice_on_the_same_day {
    dbnTennis.addMatch(MatchOutcome("playerA", "playerB", true, hours(4)))
    dbnTennis.addMatch(MatchOutcome("playerA", "playerB", true, hours(7)))

    val tennisClusterGraph = dbnTennis.getTennisClusterGraph().getClusterGraph()

    assertEquals(4, tennisClusterGraph.getClusters().size)
    assertFactor(Factor(Var(1, 3), priorProb), tennisClusterGraph.getCluster(1).getFactor(), 0)
    assertFactor(Factor(Var(2, 3), priorProb), tennisClusterGraph.getCluster(2).getFactor(), 0)

    val emmissionFactorAB = Factor(Var(1, 3), Var(2, 3), Var(3, 2), emissionProbForPlayerAWon)
    assertFactor(emmissionFactorAB, tennisClusterGraph.getCluster(3).getFactor(), 0.0001)

    val emmissionFactorAB2 = Factor(Var(1, 3), Var(2, 3), Var(4, 2), emissionProbForPlayerAWon)
    assertFactor(emmissionFactorAB2, tennisClusterGraph.getCluster(4).getFactor(), 0.0001)

  }

  @Test def addMatchOutcome_twice_on_the_same_day_different_outcomes {
    dbnTennis.addMatch(MatchOutcome("playerA", "playerB", true, 4))
    dbnTennis.addMatch(MatchOutcome("playerA", "playerB", false, 4))

    val tennisClusterGraph = dbnTennis.getTennisClusterGraph().getClusterGraph()

    assertEquals(4, tennisClusterGraph.getClusters().size)
    assertFactor(Factor(Var(1, 3), priorProb), tennisClusterGraph.getCluster(1).getFactor(), 0)
    assertFactor(Factor(Var(2, 3), priorProb), tennisClusterGraph.getCluster(2).getFactor(), 0)

    val emmissionFactorAB = Factor(Var(1, 3), Var(2, 3), Var(3, 2), emissionProbForPlayerAWon)
    assertFactor(emmissionFactorAB, tennisClusterGraph.getCluster(3).getFactor(), 0.0001)

    val emmissionFactorAB2 = Factor(Var(1, 3), Var(2, 3), Var(4, 2), emissionProbForPlayerALost)

    assertFactor(emmissionFactorAB2, tennisClusterGraph.getCluster(4).getFactor(), 0.0001)
  }

  @Test(expected = classOf[IllegalArgumentException]) def addMatchOutcome_from_the_past {
    dbnTennis.addMatch(MatchOutcome("playerA", "playerB", true, 5))
    dbnTennis.addMatch(MatchOutcome("playerC", "playerD", false, 4))
  }

  @Test def addMatchOutcome_AB_BA_AB {
    dbnTennis.addMatch(MatchOutcome("playerA", "playerB", true, 5))
    dbnTennis.addMatch(MatchOutcome("playerB", "playerA", false, 5))
    dbnTennis.addMatch(MatchOutcome("playerA", "playerB", false, 5))

    val tennisClusterGraph = dbnTennis.getTennisClusterGraph().getClusterGraph()

    assertEquals(5, tennisClusterGraph.getClusters().size)
  }

  /**
   * Tests for getFactors - check prior and emission factors.
   */
  @Test def addToFactors_single_result_duplicate {
    dbnTennis.addMatch(MatchOutcome("playerA", "playerB", true, 4))
    val tennisClusterGraph = dbnTennis.getTennisClusterGraph().getClusterGraph()

    assertEquals(3, tennisClusterGraph.getClusters().size)
    dbnTennis.addMatch(MatchOutcome("playerA", "playerB", true, 4))

    assertEquals(4, tennisClusterGraph.getClusters().size)
  }

  @Test def addToFactors_single_result {
    dbnTennis.addMatch(MatchOutcome("playerA", "playerB", true, 4))
    val tennisClusterGraph = dbnTennis.getTennisClusterGraph().getClusterGraph()

    assertEquals(3, tennisClusterGraph.getClusters().size)
    assertFactor(Factor(Var(1, 3), priorProb), tennisClusterGraph.getCluster(1).getFactor(), 0)
    assertFactor(Factor(Var(2, 3), priorProb), tennisClusterGraph.getCluster(2).getFactor(), 0)

    val emmissionFactor = Factor(Var(1, 3), Var(2, 3), Var(3, 2), emissionProbForPlayerAWon)
    assertFactor(emmissionFactor, tennisClusterGraph.getCluster(3).getFactor(), 0.0001)
  }

  @Test def addToFactors_single_result_dont_set_evidence {
    dbnTennis.addMatch(MatchOutcome("playerA", "playerB", None, 4))
    val tennisClusterGraph = dbnTennis.getTennisClusterGraph().getClusterGraph()

    assertEquals(3, tennisClusterGraph.getClusters().size)
    assertFactor(Factor(Var(1, 3), priorProb), tennisClusterGraph.getCluster(1).getFactor())
    assertFactor(Factor(Var(2, 3), priorProb), tennisClusterGraph.getCluster(2).getFactor())

    val emmissionFactor = Factor(Var(1, 3), Var(2, 3), Var(3, 2), emissionProb)
    assertFactor(emmissionFactor, tennisClusterGraph.getCluster(3).getFactor())
  }

  @Test def addToFactors_two_results_for_player_A_in_time_slice_4 {
    dbnTennis.addMatch(MatchOutcome("playerA", "playerB", true, 4))
    dbnTennis.addMatch(MatchOutcome("playerA", "playerC", true, 4))
    val tennisClusterGraph = dbnTennis.getTennisClusterGraph().getClusterGraph()

    assertEquals(5, tennisClusterGraph.getClusters().size)

    assertFactor(Factor(Var(1, 3), priorProb), tennisClusterGraph.getCluster(1).getFactor())
    assertFactor(Factor(Var(2, 3), priorProb), tennisClusterGraph.getCluster(2).getFactor())

    val emmissionFactorAB = Factor(Var(1, 3), Var(2, 3), Var(3, 2), emissionProbForPlayerAWon)
    assertFactor(emmissionFactorAB, tennisClusterGraph.getCluster(3).getFactor(), 0.0001)

    assertFactor(Factor(Var(4, 3), priorProb), tennisClusterGraph.getCluster(4).getFactor())

    val emmissionFactorAC = Factor(Var(1, 3), Var(4, 3), Var(5, 2), emissionProbForPlayerAWon)
    assertFactor(emmissionFactorAC, tennisClusterGraph.getCluster(5).getFactor(), 0.0001)
  }

  /**
   * Tests for getFactors - check transition factors.
   */
  @Test(expected = classOf[IllegalArgumentException]) def addToFactors_AB_in_time_4_when_factors_in_time_5_already_exist {
    dbnTennis.addMatch(MatchOutcome("playerA", "playerC", true, 5))
    dbnTennis.addMatch(MatchOutcome("playerA", "playerB", true, 4))
  }

  @Test def addToFactors_AB_in_time_4_AC_in_time_5 {
    dbnTennis.addMatch(MatchOutcome("playerA", "playerB", true, days(4)))
    dbnTennis.addMatch(MatchOutcome("playerA", "playerC", true, days(5)))
    val tennisClusterGraph = dbnTennis.getTennisClusterGraph().getClusterGraph()

    assertEquals(6, tennisClusterGraph.getClusters().size)

    assertFactor(Factor(Var(1, 3), priorProb), tennisClusterGraph.getCluster(1).getFactor())
    assertFactor(Factor(Var(2, 3), priorProb), tennisClusterGraph.getCluster(2).getFactor())

    val emmissionFactorAB = Factor(Var(1, 3), Var(2, 3), Var(3, 2), emissionProbForPlayerAWon)
    assertFactor(emmissionFactorAB, tennisClusterGraph.getCluster(3).getFactor(), 0.0001)

    assertFactor(Factor(Var(1, 3), Var(4, 3), transitionProb), tennisClusterGraph.getCluster(4).getFactor())
    assertFactor(Factor(Var(5, 3), priorProb), tennisClusterGraph.getCluster(5).getFactor())

    val emmissionFactorAC = Factor(Var(4, 3), Var(5, 3), Var(6, 2), emissionProbForPlayerAWon)
    assertFactor(emmissionFactorAC, tennisClusterGraph.getCluster(6).getFactor(), 0.0001)
  }

  @Test def addToFactors_AB_in_time_4_AC_in_time_6 {
    dbnTennis.addMatch(MatchOutcome("playerA", "playerB", true, days(4)))
    dbnTennis.addMatch(MatchOutcome("playerA", "playerC", true, days(6)))
    val tennisClusterGraph = dbnTennis.getTennisClusterGraph().getClusterGraph()

    assertEquals(7, tennisClusterGraph.getClusters().size)

    assertFactor(Factor(Var(1, 3), priorProb), tennisClusterGraph.getCluster(1).getFactor())
    assertFactor(Factor(Var(2, 3), priorProb), tennisClusterGraph.getCluster(2).getFactor())

    val emmissionFactorAB = Factor(Var(1, 3), Var(2, 3), Var(3.hashCode(), 2), emissionProbForPlayerAWon)
    assertFactor(emmissionFactorAB, tennisClusterGraph.getCluster(3).getFactor(), 0.0001)

    assertFactor(Factor(Var(1, 3), Var(4, 3), transitionProb), tennisClusterGraph.getCluster(4).getFactor())
    assertFactor(Factor(Var(4, 3), Var(5, 3), transitionProb), tennisClusterGraph.getCluster(5).getFactor())
    assertFactor(Factor(Var(6, 3), priorProb), tennisClusterGraph.getCluster(6).getFactor())

    val emmissionFactorAC = Factor(Var(5, 3), Var(6, 3), Var(7, 2), emissionProbForPlayerAWon)
    assertFactor(emmissionFactorAC, tennisClusterGraph.getCluster(7).getFactor(), 0.0001)
  }

  @Test def addToFactors_AB_in_time_4_BC_in_time_5 {
    dbnTennis.addMatch(MatchOutcome("playerA", "playerB", true, days(4)))
    dbnTennis.addMatch(MatchOutcome("playerB", "playerC", true, days(5)))
    val tennisClusterGraph = dbnTennis.getTennisClusterGraph().getClusterGraph()

    assertEquals(6, tennisClusterGraph.getClusters().size)

    assertFactor(Factor(Var(1, 3), priorProb), tennisClusterGraph.getCluster(1).getFactor())
    assertFactor(Factor(Var(2, 3), priorProb), tennisClusterGraph.getCluster(2).getFactor())

    val emmissionFactorAB = Factor(Var(1, 3), Var(2, 3), Var(3, 2), emissionProbForPlayerAWon)
    assertFactor(emmissionFactorAB, tennisClusterGraph.getCluster(3).getFactor(), 0.0001)

    assertFactor(Factor(Var(2, 3), Var(4, 3), transitionProb), tennisClusterGraph.getCluster(4).getFactor())
    assertFactor(Factor(Var(5, 3), priorProb), tennisClusterGraph.getCluster(5).getFactor())

    val emmissionFactorAC = Factor(Var(4, 3), Var(5, 3), Var(6, 2), emissionProbForPlayerAWon)
    assertFactor(emmissionFactorAC, tennisClusterGraph.getCluster(6).getFactor(), 0.0001)
  }

  @Test def addToFactors_AB_in_time_4_BC_in_time_6 {
    dbnTennis.addMatch(MatchOutcome("playerA", "playerB", true, days(4)))
    dbnTennis.addMatch(MatchOutcome("playerB", "playerC", true, days(6)))
    val tennisClusterGraph = dbnTennis.getTennisClusterGraph().getClusterGraph()

    assertEquals(7, tennisClusterGraph.getClusters().size)

    assertFactor(Factor(Var(1, 3), priorProb), tennisClusterGraph.getCluster(1).getFactor())
    assertFactor(Factor(Var(2, 3), priorProb), tennisClusterGraph.getCluster(2).getFactor())

    val emmissionFactorAB = Factor(Var(1, 3), Var(2, 3), Var(3, 2), emissionProbForPlayerAWon)
    assertFactor(emmissionFactorAB, tennisClusterGraph.getCluster(3).getFactor(), 0.0001)

    assertFactor(Factor(Var(2, 3), Var(4, 3), transitionProb), tennisClusterGraph.getCluster(4).getFactor())
    assertFactor(Factor(Var(4, 3), Var(5, 3), transitionProb), tennisClusterGraph.getCluster(5).getFactor())
    assertFactor(Factor(Var(6, 3), priorProb), tennisClusterGraph.getCluster(6).getFactor())

    val emmissionFactorAC = Factor(Var(5, 3), Var(6, 3), Var(7, 2), emissionProbForPlayerAWon)
    assertFactor(emmissionFactorAC, tennisClusterGraph.getCluster(7).getFactor(), 0.0001)
  }

  @Test def addToFactors_AB_in_time_4_AC_in_time_5_BC_in_5 {
    dbnTennis.addMatch(MatchOutcome("playerA", "playerB", true, days(4)))
    dbnTennis.addMatch(MatchOutcome("playerA", "playerC", true, days(5)))
    dbnTennis.addMatch(MatchOutcome("playerB", "playerC", true, days(5)))
    val tennisClusterGraph = dbnTennis.getTennisClusterGraph().getClusterGraph()

    assertEquals(8, tennisClusterGraph.getClusters().size)

    assertFactor(Factor(Var(1, 3), priorProb), tennisClusterGraph.getCluster(1).getFactor())
    assertFactor(Factor(Var(2, 3), priorProb), tennisClusterGraph.getCluster(2).getFactor())

    val emmissionFactorAB = Factor(Var(1, 3), Var(2, 3), Var(3, 2), emissionProbForPlayerAWon)
    assertFactor(emmissionFactorAB, tennisClusterGraph.getCluster(3).getFactor(), 0.0001)

    assertFactor(Factor(Var(1, 3), Var(4, 3), transitionProb), tennisClusterGraph.getCluster(4).getFactor())
    assertFactor(Factor(Var(5.hashCode(), 3), priorProb), tennisClusterGraph.getCluster(5).getFactor())

    val emmissionFactorAC = Factor(Var(4, 3), Var(5, 3), Var(6.hashCode(), 2), emissionProbForPlayerAWon)
    assertFactor(emmissionFactorAC, tennisClusterGraph.getCluster(6).getFactor(), 0.0001)

    assertFactor(Factor(Var(2, 3), Var(7, 3), transitionProb), tennisClusterGraph.getCluster(7).getFactor())

    val emmissionFactorBC = Factor(Var(7, 3), Var(5, 3), Var(8, 2), emissionProbForPlayerAWon)
    assertFactor(emmissionFactorBC, tennisClusterGraph.getCluster(8).getFactor(), 0.0001)
  }

  private def hours(num: Long): Long = num * (1000 * 60 * 60)

  private def days(num: Long): Long = num * (1000 * 60 * 60 * 24)
}