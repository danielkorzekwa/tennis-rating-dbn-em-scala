package dk.tennis.em.dbn.factorgraph

import org.junit._
import Assert._
import dk.tennis.em.EMTrain._
import dk.tennis.em.bn.Factor
import Factor._
import dk.tennis.em.util.VectorAssert._
import DbnTennis._

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

  val emissionProbForPlayerAWon = List(0.5000, 0.0000, 0.3333, 0.0000, 0.2500, 0.0000, 0.6667, 0.0000, 0.5000, 0.0000, 0.4000, 0.0000, 0.7500, 0.0000, 0.6000, 0.0000, 0.5000, 0.0000)

  val emissionProbForPlayerALost = List(0.0000, 0.5000, 0.0000, 0.6667, 0.0000, 0.7500, 0.0000, 0.3333, 0.0000, 0.5000, 0.0000, 0.6000, 0.0000, 0.2500, 0.0000, 0.4000, 0.0000, 0.5000)

  private val dbnTennis = new GenericDbnTennis(priorProb, emissionProb, transitionProb)

  /**
   * Tests for addResult.
   */
  @Test def addResult_duplicate {
    dbnTennis.addResult(Result("playerA", "playerB", true, 4))
    dbnTennis.addResult(Result("playerA", "playerB", true, 4))

    val factors = dbnTennis.getFactors()

    assertEquals(4, factors.size)

    assertEquals(Factor(Var("playerA_rating_4", ("1", "2", "3")), 0.2, 0.5, 0.3), factors(0))
    assertEquals(Factor(Var("playerB_rating_4", ("1", "2", "3")), 0.2, 0.5, 0.3), factors(1))

    val emmissionFactorAB = Factor(Var("playerA_rating_4", ("1", "2", "3")), Var("playerB_rating_4", ("1", "2", "3")), Var("score_playerA_playerB_4_0", ("w", "l")),
      emissionProbForPlayerAWon: _*)

    assertEquals(emmissionFactorAB.variables, factors(2).variables)
    vectorAssert(emmissionFactorAB.values, factors(2).values, 0.0001)

    val emmissionFactorAB2 = Factor(Var("playerA_rating_4", ("1", "2", "3")), Var("playerB_rating_4", ("1", "2", "3")), Var("score_playerA_playerB_4_1", ("w", "l")),
      emissionProbForPlayerAWon: _*)

    assertEquals(emmissionFactorAB2.variables, factors(3).variables)
    vectorAssert(emmissionFactorAB2.values, factors(3).values, 0.0001)
  }
  @Test def addResult_duplicate2 {
    dbnTennis.addResult(Result("playerA", "playerB", true, 4))
    dbnTennis.addResult(Result("playerA", "playerB", false, 4))

    val factors = dbnTennis.getFactors()

    assertEquals(4, factors.size)

    assertEquals(Factor(Var("playerA_rating_4", ("1", "2", "3")), 0.2, 0.5, 0.3), factors(0))
    assertEquals(Factor(Var("playerB_rating_4", ("1", "2", "3")), 0.2, 0.5, 0.3), factors(1))

    val emmissionFactorAB = Factor(Var("playerA_rating_4", ("1", "2", "3")), Var("playerB_rating_4", ("1", "2", "3")), Var("score_playerA_playerB_4_0", ("w", "l")),
      emissionProbForPlayerAWon: _*)

    assertEquals(emmissionFactorAB.variables, factors(2).variables)
    vectorAssert(emmissionFactorAB.values, factors(2).values, 0.0001)

    val emmissionFactorAB2 = Factor(Var("playerA_rating_4", ("1", "2", "3")), Var("playerB_rating_4", ("1", "2", "3")), Var("score_playerA_playerB_4_1", ("w", "l")),
      emissionProbForPlayerALost: _*)

    assertEquals(emmissionFactorAB2.variables, factors(3).variables)
    vectorAssert(emmissionFactorAB2.values, factors(3).values, 0.0001)

  }

  @Test(expected = classOf[IllegalArgumentException]) def addResult_from_the_past {
    dbnTennis.addResult(Result("playerA", "playerB", true, 5))
    dbnTennis.addResult(Result("playerC", "playerD", false, 4))
  }

  @Test def addResult_AB_BA_AB {
    dbnTennis.addResult(Result("playerA", "playerB", true, 5))
    dbnTennis.addResult(Result("playerB", "playerA", false, 5))
    dbnTennis.addResult(Result("playerA", "playerB", false, 5))

    val factors = dbnTennis.getFactors()

    assertEquals(5, factors.size)
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
  @Test def addToFactors_single_result_duplicate {
    dbnTennis.addResult(Result("playerA", "playerB", true, 4))
    val factors = dbnTennis.getFactors()

    assertEquals(3, factors.size)
    dbnTennis.addResult(Result("playerA", "playerB", true, 4))

    assertEquals(4, dbnTennis.getFactors().size)
  }

  @Test def addToFactors_single_result {
    dbnTennis.addResult(Result("playerA", "playerB", true, 4))
    val factors = dbnTennis.getFactors()

    assertEquals(3, factors.size)
    assertEquals(Factor(Var("playerA_rating_4", ("1", "2", "3")), 0.2, 0.5, 0.3), factors(0))
    assertEquals(Factor(Var("playerB_rating_4", ("1", "2", "3")), 0.2, 0.5, 0.3), factors(1))

    val emmissionFactor = Factor(Var("playerA_rating_4", ("1", "2", "3")), Var("playerB_rating_4", ("1", "2", "3")), Var("score_playerA_playerB_4_0", ("w", "l")),
      emissionProbForPlayerAWon: _*)

    assertEquals(emmissionFactor.variables, factors(2).variables)
    vectorAssert(emmissionFactor.values, factors(2).values, 0.0001)
  }

  @Test def addToFactors_single_result_dont_set_evidence {
    val dbnTennis = new GenericDbnTennis(priorProb, emissionProb, transitionProb)
    dbnTennis.addResult(Result("playerA", "playerB", None, 4))
    val factors = dbnTennis.getFactors()

    assertEquals(3, factors.size)
    assertEquals(Factor(Var("playerA_rating_4", ("1", "2", "3")), priorProb: _*), factors(0))
    assertEquals(Factor(Var("playerB_rating_4", ("1", "2", "3")), priorProb: _*), factors(1))

    val emmissionFactor = Factor(Var("playerA_rating_4", ("1", "2", "3")), Var("playerB_rating_4", ("1", "2", "3")), Var("score_playerA_playerB_4_0", ("w", "l")),
      emissionProb: _*)

    assertEquals(emmissionFactor.variables, factors(2).variables)
    vectorAssert(emmissionFactor.values, factors(2).values, 0.0001)
  }

  @Test def addToFactors_two_results_for_player_A_in_time_slice_4 {
    dbnTennis.addResult(Result("playerA", "playerB", true, 4))
    dbnTennis.addResult(Result("playerA", "playerC", true, 4))
    val factors = dbnTennis.getFactors()

    assertEquals(5, factors.size)

    assertEquals(Factor(Var("playerA_rating_4", ("1", "2", "3")), 0.2, 0.5, 0.3), factors(0))
    assertEquals(Factor(Var("playerB_rating_4", ("1", "2", "3")), 0.2, 0.5, 0.3), factors(1))

    val emmissionFactorAB = Factor(Var("playerA_rating_4", ("1", "2", "3")), Var("playerB_rating_4", ("1", "2", "3")), Var("score_playerA_playerB_4_0", ("w", "l")),
      emissionProbForPlayerAWon: _*)

    assertEquals(emmissionFactorAB.variables, factors(2).variables)
    vectorAssert(emmissionFactorAB.values, factors(2).values, 0.0001)

    assertEquals(Factor(Var("playerC_rating_4", ("1", "2", "3")), 0.2, 0.5, 0.3), factors(3))

    val emmissionFactorAC = Factor(Var("playerA_rating_4", ("1", "2", "3")), Var("playerC_rating_4", ("1", "2", "3")), Var("score_playerA_playerC_4_1", ("w", "l")),
      emissionProbForPlayerAWon: _*)

    assertEquals(emmissionFactorAC.variables, factors(4).variables)
    vectorAssert(emmissionFactorAC.values, factors(4).values, 0.0001)

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

    val emmissionFactorAB = Factor(Var("playerA_rating_4", ("1", "2", "3")), Var("playerB_rating_4", ("1", "2", "3")), Var("score_playerA_playerB_4_0", ("w", "l")),
      emissionProbForPlayerAWon: _*)
    assertEquals(emmissionFactorAB.variables, factors(2).variables)
    vectorAssert(emmissionFactorAB.values, factors(2).values, 0.0001)

    assertEquals(Factor(Var("playerA_rating_4", ("1", "2", "3")), Var("playerA_rating_5", ("1", "2", "3")), transitionProb: _*), factors(3))
    assertEquals(Factor(Var("playerC_rating_5", ("1", "2", "3")), priorProb: _*), factors(4))

    val emmissionFactorAC = Factor(Var("playerA_rating_5", ("1", "2", "3")), Var("playerC_rating_5", ("1", "2", "3")), Var("score_playerA_playerC_5_1", ("w", "l")),
      emissionProbForPlayerAWon: _*)
    assertEquals(emmissionFactorAC.variables, factors(5).variables)
    vectorAssert(emmissionFactorAC.values, factors(5).values, 0.0001)
  }

  @Test def addToFactors_AB_in_time_4_AC_in_time_6 {
    dbnTennis.addResult(Result("playerA", "playerB", true, 4))
    dbnTennis.addResult(Result("playerA", "playerC", true, 6))
    val factors = dbnTennis.getFactors()

    assertEquals(7, factors.size)

    assertEquals(Factor(Var("playerA_rating_4", ("1", "2", "3")), 0.2, 0.5, 0.3), factors(0))
    assertEquals(Factor(Var("playerB_rating_4", ("1", "2", "3")), 0.2, 0.5, 0.3), factors(1))

    val emmissionFactorAB = Factor(Var("playerA_rating_4", ("1", "2", "3")), Var("playerB_rating_4", ("1", "2", "3")), Var("score_playerA_playerB_4_0", ("w", "l")),
      emissionProbForPlayerAWon: _*)
    assertEquals(emmissionFactorAB.variables, factors(2).variables)
    vectorAssert(emmissionFactorAB.values, factors(2).values, 0.0001)

    assertEquals(Factor(Var("playerA_rating_4", ("1", "2", "3")), Var("playerA_rating_5", ("1", "2", "3")), transitionProb: _*), factors(3))
    assertEquals(Factor(Var("playerA_rating_5", ("1", "2", "3")), Var("playerA_rating_6", ("1", "2", "3")), transitionProb: _*), factors(4))
    assertEquals(Factor(Var("playerC_rating_6", ("1", "2", "3")), priorProb: _*), factors(5))

    val emmissionFactorAC = Factor(Var("playerA_rating_6", ("1", "2", "3")), Var("playerC_rating_6", ("1", "2", "3")), Var("score_playerA_playerC_6_1", ("w", "l")),
      emissionProbForPlayerAWon: _*)
    assertEquals(emmissionFactorAC.variables, factors(6).variables)
    vectorAssert(emmissionFactorAC.values, factors(6).values, 0.0001)
  }

  @Test def addToFactors_AB_in_time_4_BC_in_time_5 {
    dbnTennis.addResult(Result("playerA", "playerB", true, 4))
    dbnTennis.addResult(Result("playerB", "playerC", true, 5))
    val factors = dbnTennis.getFactors()

    assertEquals(6, factors.size)

    assertEquals(Factor(Var("playerA_rating_4", ("1", "2", "3")), 0.2, 0.5, 0.3), factors(0))
    assertEquals(Factor(Var("playerB_rating_4", ("1", "2", "3")), 0.2, 0.5, 0.3), factors(1))

    val emmissionFactorAB = Factor(Var("playerA_rating_4", ("1", "2", "3")), Var("playerB_rating_4", ("1", "2", "3")), Var("score_playerA_playerB_4_0", ("w", "l")),
      emissionProbForPlayerAWon: _*)
    assertEquals(emmissionFactorAB.variables, factors(2).variables)
    vectorAssert(emmissionFactorAB.values, factors(2).values, 0.0001)

    assertEquals(Factor(Var("playerB_rating_4", ("1", "2", "3")), Var("playerB_rating_5", ("1", "2", "3")), transitionProb: _*), factors(3))
    assertEquals(Factor(Var("playerC_rating_5", ("1", "2", "3")), priorProb: _*), factors(4))

    val emmissionFactorAC = Factor(Var("playerB_rating_5", ("1", "2", "3")), Var("playerC_rating_5", ("1", "2", "3")), Var("score_playerB_playerC_5_1", ("w", "l")),
      emissionProbForPlayerAWon: _*)
    assertEquals(emmissionFactorAC.variables, factors(5).variables)
    vectorAssert(emmissionFactorAC.values, factors(5).values, 0.0001)
  }

  @Test def addToFactors_AB_in_time_4_BC_in_time_6 {
    dbnTennis.addResult(Result("playerA", "playerB", true, 4))
    dbnTennis.addResult(Result("playerB", "playerC", true, 6))
    val factors = dbnTennis.getFactors()

    assertEquals(7, factors.size)

    assertEquals(Factor(Var("playerA_rating_4", ("1", "2", "3")), 0.2, 0.5, 0.3), factors(0))
    assertEquals(Factor(Var("playerB_rating_4", ("1", "2", "3")), 0.2, 0.5, 0.3), factors(1))

    val emmissionFactorAB = Factor(Var("playerA_rating_4", ("1", "2", "3")), Var("playerB_rating_4", ("1", "2", "3")), Var("score_playerA_playerB_4_0", ("w", "l")),
      emissionProbForPlayerAWon: _*)
    assertEquals(emmissionFactorAB.variables, factors(2).variables)
    vectorAssert(emmissionFactorAB.values, factors(2).values, 0.0001)

    assertEquals(Factor(Var("playerB_rating_4", ("1", "2", "3")), Var("playerB_rating_5", ("1", "2", "3")), transitionProb: _*), factors(3))
    assertEquals(Factor(Var("playerB_rating_5", ("1", "2", "3")), Var("playerB_rating_6", ("1", "2", "3")), transitionProb: _*), factors(4))
    assertEquals(Factor(Var("playerC_rating_6", ("1", "2", "3")), priorProb: _*), factors(5))

    val emmissionFactorAC = Factor(Var("playerB_rating_6", ("1", "2", "3")), Var("playerC_rating_6", ("1", "2", "3")), Var("score_playerB_playerC_6_1", ("w", "l")),
      emissionProbForPlayerAWon: _*)
    assertEquals(emmissionFactorAC.variables, factors(6).variables)
    vectorAssert(emmissionFactorAC.values, factors(6).values, 0.0001)
  }

  @Test def addToFactors_AB_in_time_4_AC_in_time_5_BC_in_5 {
    dbnTennis.addResult(Result("playerA", "playerB", true, 4))
    dbnTennis.addResult(Result("playerA", "playerC", true, 5))
    dbnTennis.addResult(Result("playerB", "playerC", true, 5))
    val factors = dbnTennis.getFactors()

    assertEquals(8, factors.size)

    assertEquals(Factor(Var("playerA_rating_4", ("1", "2", "3")), 0.2, 0.5, 0.3), factors(0))
    assertEquals(Factor(Var("playerB_rating_4", ("1", "2", "3")), 0.2, 0.5, 0.3), factors(1))

    val emmissionFactorAB = Factor(Var("playerA_rating_4", ("1", "2", "3")), Var("playerB_rating_4", ("1", "2", "3")), Var("score_playerA_playerB_4_0", ("w", "l")),
      emissionProbForPlayerAWon: _*)
    assertEquals(emmissionFactorAB.variables, factors(2).variables)
    vectorAssert(emmissionFactorAB.values, factors(2).values, 0.0001)

    assertEquals(Factor(Var("playerA_rating_4", ("1", "2", "3")), Var("playerA_rating_5", ("1", "2", "3")), transitionProb: _*), factors(3))
    assertEquals(Factor(Var("playerC_rating_5", ("1", "2", "3")), priorProb: _*), factors(4))

    val emmissionFactorAC = Factor(Var("playerA_rating_5", ("1", "2", "3")), Var("playerC_rating_5", ("1", "2", "3")), Var("score_playerA_playerC_5_1", ("w", "l")),
      emissionProbForPlayerAWon: _*)
    assertEquals(emmissionFactorAC.variables, factors(5).variables)
    vectorAssert(emmissionFactorAC.values, factors(5).values, 0.0001)

    assertEquals(Factor(Var("playerB_rating_4", ("1", "2", "3")), Var("playerB_rating_5", ("1", "2", "3")), transitionProb: _*), factors(6))

    val emmissionFactorBC = Factor(Var("playerB_rating_5", ("1", "2", "3")), Var("playerC_rating_5", ("1", "2", "3")), Var("score_playerB_playerC_5_2", ("w", "l")),
      emissionProbForPlayerAWon: _*)
    assertEquals(emmissionFactorAC.variables, factors(5).variables)
    vectorAssert(emmissionFactorAC.values, factors(7).values, 0.0001)
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

  /**
   * Tests for getEvidenceVariables
   */
  @Test def getEvidenceVariables_no_results {
    assertEquals(Map(), dbnTennis.getResultVariables())
  }

  @Test def getEvidenceVariables_one_unkown_result {
    dbnTennis.addResult(Result("playerA", "playerB", None, 1))

    val resultVariables = Map(Result("playerA", "playerB", None, 1) -> Var("score_playerA_playerB_1_0", List("w", "l")))
    assertEquals(resultVariables, dbnTennis.getResultVariables())
  }

  @Test def getEvidenceVariables_one_known_result {
    dbnTennis.addResult(Result("playerA", "playerB", false, 1))

    val resultVariables = Map(Result("playerA", "playerB", Some(false), 1) -> Var("score_playerA_playerB_1_0", List("w", "l")))
    assertEquals(resultVariables, dbnTennis.getResultVariables())
  }

  @Test def getEvidenceVariables_two_known_results {
    dbnTennis.addResult(Result("playerA", "playerB", false, 1))
    dbnTennis.addResult(Result("playerB", "playerC", true, 3))

    val resultVariables = Map(
      Result("playerA", "playerB", Some(false), 1) -> Var("score_playerA_playerB_1_0", List("w", "l")),
      Result("playerB", "playerC", Some(true), 3) -> Var("score_playerB_playerC_3_1", List("w", "l")))

    assertEquals(resultVariables, dbnTennis.getResultVariables())
  }

  /**Tests for getPlayerVariables*/
  @Test def getPlayerVariables_no_results {
    val playerVariables = dbnTennis.getPlayerVariables()
    val expectedVariables = Map()

    assertEquals(expectedVariables, playerVariables)
  }

  @Test def getPlayerVariables_single_result {
    dbnTennis.addResult(Result("playerA", "playerB", false, 1))

    val playerVariables = dbnTennis.getPlayerVariables()

    val timeSlice1Variables = Map(
      "playerA" -> Var("playerA_rating_1", List("1", "2", "3")),
      "playerB" -> Var("playerB_rating_1", List("1", "2", "3")))

    val expectedVariables = Map(1 -> timeSlice1Variables)

    assertEquals(expectedVariables, playerVariables)
  }

  @Test def getPlayerVariables_three_results_in_two_time_slices {
    dbnTennis.addResult(Result("playerA", "playerB", false, 1))
    dbnTennis.addResult(Result("playerB", "playerC", false, 3))
    dbnTennis.addResult(Result("playerA", "playerC", false, 3))

    val playerVariables = dbnTennis.getPlayerVariables()

    val timeSlice1Variables = Map(
      "playerA" -> Var("playerA_rating_1", List("1", "2", "3")),
      "playerB" -> Var("playerB_rating_1", List("1", "2", "3")))

    val timeSlice2Variables = Map(
      "playerA" -> Var("playerA_rating_2", List("1", "2", "3")),
      "playerB" -> Var("playerB_rating_2", List("1", "2", "3")))

    val timeSlice3Variables = Map(
      "playerA" -> Var("playerA_rating_3", List("1", "2", "3")),
      "playerB" -> Var("playerB_rating_3", List("1", "2", "3")),
      "playerC" -> Var("playerC_rating_3", List("1", "2", "3")))

    assertEquals(3, playerVariables.keys.size)

    assertEquals(timeSlice1Variables, playerVariables(1))
    assertEquals(timeSlice2Variables, playerVariables(2))
    assertEquals(timeSlice3Variables, playerVariables(3))
  }

}