package dk.tennis.em.dbn

import org.junit._
import Assert._
import dk.tennis.em.EMTrain._
import DbnTennis._
import dk.tennis.em.bn.Factor
import Factor._

class GenericDbnTennisTest {

  val priorProb = List(0.2, 0.5, 0.3)

  val emissionProb = List(
    0.5, 0.5,
    1d / 3, 2d / 3,
    0.25, 0.75,
    2d / 3, 1d / 3,
    0.5, 0.5,
    2d / 5, 3d / 5,
    3d / 4, 1d / 4,
    3d / 5, 2d / 5,
    0.5, 0.5)

  val transitionProb = List(0.98, 0.01, 0.01, 0.01, 0.98, 0.01, 0.01, 0.02, 0.97)

  private val dbnTennis = new GenericDbnTennis(priorProb, emissionProb, transitionProb)

  /**
   * Tests for addResult.
   */
  @Test(expected = classOf[IllegalArgumentException]) def addResult_duplicate {
    dbnTennis.addResult(Result("playerA", "playerB", true, 4))
    dbnTennis.addResult(Result("playerA", "playerB", true, 4))
  }
  @Test(expected = classOf[IllegalArgumentException]) def addResult_duplicate2 {
    dbnTennis.addResult(Result("playerA", "playerB", true, 4))
    dbnTennis.addResult(Result("playerA", "playerB", false, 4))
  }

  @Test(expected = classOf[IllegalArgumentException]) def addResult_from_the_past {
    dbnTennis.addResult(Result("playerA", "playerB", true, 5))
    dbnTennis.addResult(Result("playerC", "playerD", false, 4))
  }

  @Test(expected = classOf[IllegalArgumentException]) def addResult_AB_BA_AB {
    dbnTennis.addResult(Result("playerA", "playerB", true, 5))
    dbnTennis.addResult(Result("playerB", "playerA", false, 5))
    dbnTennis.addResult(Result("playerA", "playerB", false, 5))
  }

  @Test def addResult_AB_BA {
    dbnTennis.addResult(Result("playerA", "playerB", true, 5))
    dbnTennis.addResult(Result("playerB", "playerA", false, 5))
    assertEquals(2, dbnTennis.getResults().size)
  }

  @Test def addResult_single {
    dbnTennis.addResult(Result("playerA", "playerB", true, 4))
    assertEquals(1, dbnTennis.getResults().size)
  }

  @Test def addResult_double_the_same_time_slice {
    dbnTennis.addResult(Result("playerA", "playerB", true, 4))
    dbnTennis.addResult(Result("playerA", "playerC", true, 4))
    assertEquals(2, dbnTennis.getResults().size)
  }

  @Test def addResult_double_different_time_slices {
    dbnTennis.addResult(Result("playerA", "playerB", true, 4))
    dbnTennis.addResult(Result("playerA", "playerB", true, 5))
    assertEquals(2, dbnTennis.getResults().size)
  }

  /**
   * Tests for getFactors - check prior and emission factors.
   */
  @Test(expected = classOf[IllegalArgumentException]) def addToFactors_single_result_duplicate {
    dbnTennis.addResult(Result("playerA", "playerB", true, 4))
    val factors = dbnTennis.getFactors()

    assertEquals(3, factors.size)
    dbnTennis.addResult(Result("playerA", "playerB", true, 4))

  }

  @Test def addToFactors_single_result {
    dbnTennis.addResult(Result("playerA", "playerB", true, 4))
    val factors = dbnTennis.getFactors()

    assertEquals(3, factors.size)
    assertEquals(Factor(Var("playerA_rating_4", ("1", "2", "3")), 0.2, 0.5, 0.3), factors(0))
    assertEquals(Factor(Var("playerB_rating_4", ("1", "2", "3")), 0.2, 0.5, 0.3), factors(1))

    val emmissionFactor = Factor(Var("playerA_rating_4", ("1", "2", "3")), Var("playerB_rating_4", ("1", "2", "3")), Var("score_playerA_playerB_4", ("w", "l")),
      11, 0, 7, 0, 5, 0, 14, 0, 11, 0, 8, 0, 16, 0, 13, 0, 11, 0)
    assertEquals(emmissionFactor.variables, factors(2).variables)
    assertEquals(emmissionFactor.values, factors(2).values.map(v => (v * 100).toInt).toList)

  }

  @Test def addToFactors_two_results_for_player_A_in_time_slice_4 {
    dbnTennis.addResult(Result("playerA", "playerB", true, 4))
    dbnTennis.addResult(Result("playerA", "playerC", true, 4))
    val factors = dbnTennis.getFactors()

    assertEquals(5, factors.size)

    assertEquals(Factor(Var("playerA_rating_4", ("1", "2", "3")), 0.2, 0.5, 0.3), factors(0))
    assertEquals(Factor(Var("playerB_rating_4", ("1", "2", "3")), 0.2, 0.5, 0.3), factors(1))

    val emmissionFactorAB = Factor(Var("playerA_rating_4", ("1", "2", "3")), Var("playerB_rating_4", ("1", "2", "3")), Var("score_playerA_playerB_4", ("w", "l")),
      11, 0, 7, 0, 5, 0, 14, 0, 11, 0, 8, 0, 16, 0, 13, 0, 11, 0)
    assertEquals(emmissionFactorAB.variables, factors(2).variables)
    assertEquals(emmissionFactorAB.values, factors(2).values.map(v => (v * 100).toInt))

    assertEquals(Factor(Var("playerC_rating_4", ("1", "2", "3")), 0.2, 0.5, 0.3), factors(3))

    val emmissionFactorAC = Factor(Var("playerA_rating_4", ("1", "2", "3")), Var("playerC_rating_4", ("1", "2", "3")), Var("score_playerA_playerC_4", ("w", "l")),
      11, 0, 7, 0, 5, 0, 14, 0, 11, 0, 8, 0, 16, 0, 13, 0, 11, 0)
    assertEquals(emmissionFactorAC.variables, factors(4).variables)
    assertEquals(emmissionFactorAC.values, factors(4).values.map(v => (v * 100).toInt))

  }

  /**
   * Tests for getFactors - check transition factors.
   */
  @Test(expected = classOf[IllegalArgumentException]) def addToFactors_AB_in_time_4_when_factors_in_time_5_already_exist {
    dbnTennis.addResult(Result("playerA", "playerC", true, 5))
    dbnTennis.addResult(Result("playerA", "playerB", true, 4))
  }

  @Test def addToFactors_AB_in_time_4_AC_in_time_5 {
    dbnTennis.addResult(Result("playerA", "playerB", true, 4))
    dbnTennis.addResult(Result("playerA", "playerC", true, 5))
    val factors = dbnTennis.getFactors()

    assertEquals(6, factors.size)

    assertEquals(Factor(Var("playerA_rating_4", ("1", "2", "3")), 0.2, 0.5, 0.3), factors(0))
    assertEquals(Factor(Var("playerB_rating_4", ("1", "2", "3")), 0.2, 0.5, 0.3), factors(1))

    val emmissionFactorAB = Factor(Var("playerA_rating_4", ("1", "2", "3")), Var("playerB_rating_4", ("1", "2", "3")), Var("score_playerA_playerB_4", ("w", "l")),
      11, 0, 7, 0, 5, 0, 14, 0, 11, 0, 8, 0, 16, 0, 13, 0, 11, 0)
    assertEquals(emmissionFactorAB.variables, factors(2).variables)
    assertEquals(emmissionFactorAB.values, factors(2).values.map(v => (v * 100).toInt))

    assertEquals(Factor(Var("playerA_rating_4", ("1", "2", "3")), Var("playerA_rating_5", ("1", "2", "3")), transitionProb: _*), factors(3))
    assertEquals(Factor(Var("playerC_rating_5", ("1", "2", "3")), priorProb: _*), factors(4))

    val emmissionFactorAC = Factor(Var("playerA_rating_5", ("1", "2", "3")), Var("playerC_rating_5", ("1", "2", "3")), Var("score_playerA_playerC_5", ("w", "l")),
      11, 0, 7, 0, 5, 0, 14, 0, 11, 0, 8, 0, 16, 0, 13, 0, 11, 0)
    assertEquals(emmissionFactorAC.variables, factors(5).variables)
    assertEquals(emmissionFactorAC.values, factors(5).values.map(v => (v * 100).toInt))
  }

  @Test def addToFactors_AB_in_time_4_AC_in_time_6 {
    dbnTennis.addResult(Result("playerA", "playerB", true, 4))
    dbnTennis.addResult(Result("playerA", "playerC", true, 6))
    val factors = dbnTennis.getFactors()

    assertEquals(7, factors.size)

    assertEquals(Factor(Var("playerA_rating_4", ("1", "2", "3")), 0.2, 0.5, 0.3), factors(0))
    assertEquals(Factor(Var("playerB_rating_4", ("1", "2", "3")), 0.2, 0.5, 0.3), factors(1))

    val emmissionFactorAB = Factor(Var("playerA_rating_4", ("1", "2", "3")), Var("playerB_rating_4", ("1", "2", "3")), Var("score_playerA_playerB_4", ("w", "l")),
      11, 0, 7, 0, 5, 0, 14, 0, 11, 0, 8, 0, 16, 0, 13, 0, 11, 0)
    assertEquals(emmissionFactorAB.variables, factors(2).variables)
    assertEquals(emmissionFactorAB.values, factors(2).values.map(v => (v * 100).toInt))

    assertEquals(Factor(Var("playerA_rating_4", ("1", "2", "3")), Var("playerA_rating_5", ("1", "2", "3")), transitionProb: _*), factors(3))
    assertEquals(Factor(Var("playerA_rating_5", ("1", "2", "3")), Var("playerA_rating_6", ("1", "2", "3")), transitionProb: _*), factors(4))
    assertEquals(Factor(Var("playerC_rating_6", ("1", "2", "3")), priorProb: _*), factors(5))

    val emmissionFactorAC = Factor(Var("playerA_rating_6", ("1", "2", "3")), Var("playerC_rating_6", ("1", "2", "3")), Var("score_playerA_playerC_6", ("w", "l")),
      11, 0, 7, 0, 5, 0, 14, 0, 11, 0, 8, 0, 16, 0, 13, 0, 11, 0)
    assertEquals(emmissionFactorAC.variables, factors(6).variables)
    assertEquals(emmissionFactorAC.values, factors(6).values.map(v => (v * 100).toInt))
  }

  @Test def addToFactors_AB_in_time_4_BC_in_time_5 {
    dbnTennis.addResult(Result("playerA", "playerB", true, 4))
    dbnTennis.addResult(Result("playerB", "playerC", true, 5))
    val factors = dbnTennis.getFactors()

    assertEquals(6, factors.size)

    assertEquals(Factor(Var("playerA_rating_4", ("1", "2", "3")), 0.2, 0.5, 0.3), factors(0))
    assertEquals(Factor(Var("playerB_rating_4", ("1", "2", "3")), 0.2, 0.5, 0.3), factors(1))

    val emmissionFactorAB = Factor(Var("playerA_rating_4", ("1", "2", "3")), Var("playerB_rating_4", ("1", "2", "3")), Var("score_playerA_playerB_4", ("w", "l")),
      11, 0, 7, 0, 5, 0, 14, 0, 11, 0, 8, 0, 16, 0, 13, 0, 11, 0)
    assertEquals(emmissionFactorAB.variables, factors(2).variables)
    assertEquals(emmissionFactorAB.values, factors(2).values.map(v => (v * 100).toInt))

    assertEquals(Factor(Var("playerB_rating_4", ("1", "2", "3")), Var("playerB_rating_5", ("1", "2", "3")), transitionProb: _*), factors(3))
    assertEquals(Factor(Var("playerC_rating_5", ("1", "2", "3")), priorProb: _*), factors(4))

    val emmissionFactorAC = Factor(Var("playerB_rating_5", ("1", "2", "3")), Var("playerC_rating_5", ("1", "2", "3")), Var("score_playerB_playerC_5", ("w", "l")),
      11, 0, 7, 0, 5, 0, 14, 0, 11, 0, 8, 0, 16, 0, 13, 0, 11, 0)
    assertEquals(emmissionFactorAC.variables, factors(5).variables)
    assertEquals(emmissionFactorAC.values, factors(5).values.map(v => (v * 100).toInt))
  }

  @Test def addToFactors_AB_in_time_4_BC_in_time_6 {
    dbnTennis.addResult(Result("playerA", "playerB", true, 4))
    dbnTennis.addResult(Result("playerB", "playerC", true, 6))
    val factors = dbnTennis.getFactors()

    assertEquals(7, factors.size)

    assertEquals(Factor(Var("playerA_rating_4", ("1", "2", "3")), 0.2, 0.5, 0.3), factors(0))
    assertEquals(Factor(Var("playerB_rating_4", ("1", "2", "3")), 0.2, 0.5, 0.3), factors(1))

    val emmissionFactorAB = Factor(Var("playerA_rating_4", ("1", "2", "3")), Var("playerB_rating_4", ("1", "2", "3")), Var("score_playerA_playerB_4", ("w", "l")),
      11, 0, 7, 0, 5, 0, 14, 0, 11, 0, 8, 0, 16, 0, 13, 0, 11, 0)
    assertEquals(emmissionFactorAB.variables, factors(2).variables)
    assertEquals(emmissionFactorAB.values, factors(2).values.map(v => (v * 100).toInt))

    assertEquals(Factor(Var("playerB_rating_4", ("1", "2", "3")), Var("playerB_rating_5", ("1", "2", "3")), transitionProb: _*), factors(3))
    assertEquals(Factor(Var("playerB_rating_5", ("1", "2", "3")), Var("playerB_rating_6", ("1", "2", "3")), transitionProb: _*), factors(4))
    assertEquals(Factor(Var("playerC_rating_6", ("1", "2", "3")), priorProb: _*), factors(5))

    val emmissionFactorAC = Factor(Var("playerB_rating_6", ("1", "2", "3")), Var("playerC_rating_6", ("1", "2", "3")), Var("score_playerB_playerC_6", ("w", "l")),
      11, 0, 7, 0, 5, 0, 14, 0, 11, 0, 8, 0, 16, 0, 13, 0, 11, 0)
    assertEquals(emmissionFactorAC.variables, factors(6).variables)
    assertEquals(emmissionFactorAC.values, factors(6).values.map(v => (v * 100).toInt))
  }

  @Test def addToFactors_AB_in_time_4_AC_in_time_5_BC_in_5 {
    dbnTennis.addResult(Result("playerA", "playerB", true, 4))
    dbnTennis.addResult(Result("playerA", "playerC", true, 5))
    dbnTennis.addResult(Result("playerB", "playerC", true, 5))
    val factors = dbnTennis.getFactors()

    assertEquals(8, factors.size)

    assertEquals(Factor(Var("playerA_rating_4", ("1", "2", "3")), 0.2, 0.5, 0.3), factors(0))
    assertEquals(Factor(Var("playerB_rating_4", ("1", "2", "3")), 0.2, 0.5, 0.3), factors(1))

    val emmissionFactorAB = Factor(Var("playerA_rating_4", ("1", "2", "3")), Var("playerB_rating_4", ("1", "2", "3")), Var("score_playerA_playerB_4", ("w", "l")),
      11, 0, 7, 0, 5, 0, 14, 0, 11, 0, 8, 0, 16, 0, 13, 0, 11, 0)
    assertEquals(emmissionFactorAB.variables, factors(2).variables)
    assertEquals(emmissionFactorAB.values, factors(2).values.map(v => (v * 100).toInt))

    assertEquals(Factor(Var("playerA_rating_4", ("1", "2", "3")), Var("playerA_rating_5", ("1", "2", "3")), transitionProb: _*), factors(3))
    assertEquals(Factor(Var("playerC_rating_5", ("1", "2", "3")), priorProb: _*), factors(4))

    val emmissionFactorAC = Factor(Var("playerA_rating_5", ("1", "2", "3")), Var("playerC_rating_5", ("1", "2", "3")), Var("score_playerA_playerC_5", ("w", "l")),
      11, 0, 7, 0, 5, 0, 14, 0, 11, 0, 8, 0, 16, 0, 13, 0, 11, 0)
    assertEquals(emmissionFactorAC.variables, factors(5).variables)
    assertEquals(emmissionFactorAC.values, factors(5).values.map(v => (v * 100).toInt))

    assertEquals(Factor(Var("playerB_rating_4", ("1", "2", "3")), Var("playerB_rating_5", ("1", "2", "3")), transitionProb: _*), factors(6))

    val emmissionFactorBC = Factor(Var("playerB_rating_5", ("1", "2", "3")), Var("playerC_rating_5", ("1", "2", "3")), Var("score_playerB_playerC_5", ("w", "l")),
      11, 0, 7, 0, 5, 0, 14, 0, 11, 0, 8, 0, 16, 0, 13, 0, 11, 0)
    assertEquals(emmissionFactorAC.variables, factors(5).variables)
    assertEquals(emmissionFactorAC.values, factors(7).values.map(v => (v * 100).toInt))
  }

  /**Tests for getPlayerIds.*/
  @Test def getPlayerIds_no_players {
    assertEquals(Nil, dbnTennis.getPlayerIds())
  }

  @Test def getPlayerIds {
    dbnTennis.addResult(Result("playerA", "playerB", true, 1))
    dbnTennis.addResult(Result("playerB", "playerC", true, 1))
    assertEquals(List("playerA", "playerB", "playerC"), dbnTennis.getPlayerIds())
  }

}