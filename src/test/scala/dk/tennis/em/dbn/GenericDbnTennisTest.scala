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
   * Tests for getPriorRating
   *
   */

  @Test def getPriorRating_no_results_exist {
    val priorRating = dbnTennis.getRatingPriorProbabilities()
    assertEquals(Nil, priorRating)
  }

  @Test def getPriorRating_single_result {
    dbnTennis.addResult(Result("playerA", "playerB", true, 1))

    val priorRatings = dbnTennis.getRatingPriorProbabilities()

    assertRating(List(0.1366, 0.5033, 0.36), priorRatings(0), 0.0001)
    assertRating(List(0.2633, 0.49666, 0.24), priorRatings(1), 0.0001)
  }

  @Test def getPriorRating_two_results_for_player_A_in_time_slice_4 {
    dbnTennis.addResult(Result("playerA", "playerB", true, 4))
    dbnTennis.addResult(Result("playerA", "playerC", true, 4))

    val priorRatings = dbnTennis.getRatingPriorProbabilities()

    assertRating(List(0.0904, 0.4909, 0.4185), priorRatings(0), 0.0001)
    assertRating(List(0.2611, 0.4972, 0.2415), priorRatings(1), 0.0001)
    assertRating(List(0.2611, 0.4972, 0.2415), priorRatings(2), 0.0001)
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
    assertEquals(emmissionFactor.values, factors(2).values.map(v => (v * 100).toInt))

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

   // assertEquals(7, factors.size)
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
  
  private def assertRating(expected: Seq[Double], actual: Seq[Double], delta: Double) {
    for ((expected, actual) <- expected.zip(actual)) {
      assertEquals(expected, actual, delta)
    }
  }
}