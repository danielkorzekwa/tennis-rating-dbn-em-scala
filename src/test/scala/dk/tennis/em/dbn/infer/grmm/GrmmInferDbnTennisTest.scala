package dk.tennis.em.dbn.infer.grmm

import org.junit._
import Assert._
import dk.tennis.em.dbn.infer.InferDbnTennis
import dk.tennis.em.util.VectorAssert._
import dk.tennis.em.dbn.infer.InferDbnTennis
import dk.tennis.em.dbn.factorgraph.DbnTennis.Result
import edu.umass.cs.mallet.grmm.types.Variable
import edu.umass.cs.mallet.grmm.types.TableFactor
import edu.umass.cs.mallet.grmm.types.FactorGraph
import edu.umass.cs.mallet.grmm.types.LogTableFactor

class GrmmInferDbnTennisTest {

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

  private val transitionProb = List(0.98, 0.01, 0.01, 0.01, 0.98, 0.01, 0.01, 0.02, 0.97)

  def createInferDbnTennis(results: Seq[Result], priorProb: Seq[Double], emissionProb: Seq[Double], transitionProb: Seq[Double]): InferDbnTennis =
    GrmmInferDbnTennisFactory().create(results, priorProb, emissionProb, transitionProb)

  /**
   * Tests for getPriorRating
   *
   */

  @Test(expected = classOf[IllegalArgumentException]) def getPriorRating_no_results_exist {
    val priorProbs = createInferDbnTennis(Nil, priorProb, emissionProb, transitionProb).getRatingPriorProbabilities()
  }

  @Test def getPriorRating_single_result {
    val results = Result("playerA", "playerB", true, 1) :: Nil

    val priorProbs = createInferDbnTennis(results, priorProb, emissionProb, transitionProb).getRatingPriorProbabilities()
    assertEquals(2, priorProbs.size)

    vectorAssert(List(0.1366, 0.5033, 0.36), priorProbs(0), 0.0001)
    vectorAssert(List(0.2633, 0.49666, 0.24), priorProbs(1), 0.0001)
  }

  @Test def getPriorRating_two_results_for_player_A_in_time_slice_4 {
    val results = List(Result("playerA", "playerB", true, 4), Result("playerA", "playerC", true, 4))

    val priorProbs = createInferDbnTennis(results, priorProb, emissionProb, transitionProb).getRatingPriorProbabilities()
    assertEquals(3, priorProbs.size)

    vectorAssert(List(0.0904, 0.4909, 0.4185), priorProbs(0), 0.0001)
    vectorAssert(List(0.2611, 0.4972, 0.2415), priorProbs(1), 0.0001)
    vectorAssert(List(0.2611, 0.4972, 0.2415), priorProbs(2), 0.0001)
  }

  @Test def getPriorRating_two_opposite_results_between_players_A_and_B_in_the_same_time_slice {
    val results = List(Result("playerA", "playerB", true, 4), Result("playerA", "playerB", false, 4))

    val priorProbs = createInferDbnTennis(results, priorProb, emissionProb, transitionProb).getRatingPriorProbabilities()
    assertEquals(2, priorProbs.size)

    vectorAssert(List(0.1853, 0.5165, 0.2981), priorProbs(0), 0.0001)
    vectorAssert(List(0.1853, 0.5165, 0.2981), priorProbs(1), 0.0001)

  }

  @Test def getPriorRating_AB_in_time_4_AC_in_time_5 {
    val results = List(Result("playerA", "playerB", true, 4), Result("playerA", "playerC", true, 5))

    val priorProbs = createInferDbnTennis(results, priorProb, emissionProb, transitionProb).getRatingPriorProbabilities()
    assertEquals(3, priorProbs.size)

    vectorAssert(List(0.0919, 0.4916, 0.4165), priorProbs(0), 0.0001)
    vectorAssert(List(0.2612, 0.49725, 0.24149), priorProbs(1), 0.0001)
    vectorAssert(List(0.2613, 0.4972, 0.2414), priorProbs(2), 0.0001)
  }

  @Test def getPriorRating_AB_in_time_4_BC_in_time_5 {
    val results = List(Result("playerA", "playerB", true, 4), Result("playerB", "playerC", true, 5))

    val priorProbs = createInferDbnTennis(results, priorProb, emissionProb, transitionProb).getRatingPriorProbabilities()
    assertEquals(3, priorProbs.size)

    vectorAssert(List(0.1348, 0.5029, 0.3622), priorProbs(0), 0.0001)
    vectorAssert(List(0.1883, 0.5162, 0.2954), priorProbs(1), 0.0001)
    vectorAssert(List(0.2656, 0.4960, 0.2383), priorProbs(2), 0.0001)
  }

  /**
   * Tests for getScoreEmissionProbabilities
   */
  @Test(expected = classOf[IllegalArgumentException]) def getScoreEmissionProbabilities_no_results {
    val scoreProbs = createInferDbnTennis(Nil, priorProb, emissionProb, transitionProb).getScoreEmissionProbabilities()
  }

  @Test def getScoreEmissionProbabilities_AB_in_time_4 {
    val results = List(Result("playerA", "playerB", true, 4))

    val scoreProbs = createInferDbnTennis(results, priorProb, emissionProb, transitionProb).getScoreEmissionProbabilities()
    assertEquals(1, scoreProbs.size)

    vectorAssert(List(0.04, 0, 0.0666, 0, 0.03, 0, 0.1333, 0, 0.25, 0, 0.12, 0, 0.09, 0, 0.18, 0, 0.09, 0), scoreProbs(0), 0.0001)
  }

  @Test def getScoreEmissionProbabilities_AB_and_BC_in_time_4 {
    val results = List(Result("playerA", "playerB", true, 4), Result("playerB", "playerC", true, 4))

    val scoreProbs = createInferDbnTennis(results, priorProb, emissionProb, transitionProb).getScoreEmissionProbabilities()
    assertEquals(2, scoreProbs.size)

    vectorAssert(List(0.0282, 0, 0.0693, 0, 0.0371, 0, 0.0941, 0, 0.26, 0, 0.1487, 0, 0.0635, 0, 0.1872, 0, 0.1115, 0), scoreProbs(0), 0.0001)
    vectorAssert(List(0.0544, 0, 0.0906, 0, 0.0408, 0, 0.1368, 0, 0.2565, 0, 0.1231, 0, 0.0743, 0, 0.1487, 0, 0.0743, 0), scoreProbs(1), 0.0001)
  }

  @Test def getScoreEmissionProbabilities_AB_in_time_4_BC_in_time_6 {
    val results = List(Result("playerA", "playerB", true, 4), Result("playerB", "playerC", false, 6))

    val scoreProbs = createInferDbnTennis(results, priorProb, emissionProb, transitionProb).getScoreEmissionProbabilities()
    assertEquals(2, scoreProbs.size)

    vectorAssert(List(0.0503, 0, 0.0642, 0, 0.0237, 0, 0.1677, 0, 0.2409, 0, 0.0949, 0, 0.1132, 0, 0.1734, 0, 0.0712, 0), scoreProbs(0), 0.0001)
    vectorAssert(List(0, 0.0517, 0, 0.1725, 0, 0.1164, 0, 0.0634, 0, 0.2379, 0, 0.1713, 0, 0.0233, 0, 0.0932, 0, 0.0699), scoreProbs(1), 0.0001)
  }

  /**
   * Tests for getRatingTransitionProbabilities
   */
  @Test(expected = classOf[IllegalArgumentException]) def getRatingTransitionProbabilities_no_results {
    val transitionProbs = createInferDbnTennis(Nil, priorProb, emissionProb, transitionProb).getRatingTransitionProbabilities()
  }

  @Test def getRatingTransitionProbabilities_single_results_in_time_8 {
    val results = List(Result("playerA", "playerB", true, 8))

    val transitionProbs = createInferDbnTennis(results, priorProb, emissionProb, transitionProb).getRatingTransitionProbabilities()
    assertEquals(Nil, transitionProbs)
  }

  @Test def getRatingTransitionProbabilities_two_results_in_time_8 {
    val results = List(Result("playerA", "playerB", true, 8), Result("playerA", "playerC", false, 8))

    val transitionProbs = createInferDbnTennis(results, priorProb, emissionProb, transitionProb).getRatingTransitionProbabilities()
    assertEquals(Nil, transitionProbs)
  }

  @Test def getRatingTransitionProbabilities_AB_in_time_8_BC_in_time_9 {
    val results = List(Result("playerA", "playerB", true, 8), Result("playerB", "playerC", false, 9))

    val transitionProbs = createInferDbnTennis(results, priorProb, emissionProb, transitionProb).getRatingTransitionProbabilities()
    assertEquals(1, transitionProbs.size)

    vectorAssert(List(0.3290, 0.0025, 0.0020, 0.0063, 0.4682, 0.0038, 0.0031, 0.0046, 0.1803), transitionProbs(0), 0.0001)
  }

  @Test def getRatingTransitionProbabilities_AB_in_time_8_BC_in_time_10 {
    val results = List(Result("playerA", "playerB", true, 8), Result("playerB", "playerC", false, 10))

    val transitionProbs = createInferDbnTennis(results, priorProb, emissionProb, transitionProb).getRatingTransitionProbabilities()
    assertEquals(2, transitionProbs.size)

    vectorAssert(List(0.3268, 0.0025, 0.0021, 0.0063, 0.4685, 0.0039, 0.0030, 0.0046, 0.1823), transitionProbs(0), 0.0001)
    vectorAssert(List(0.3315, 0.0026, 0.0021, 0.0063, 0.4656, 0.0038, 0.0031, 0.0046, 0.1805), transitionProbs(1), 0.0001)
  }

  @Test def getRatingTransitionProbabilities_AB_in_time_8_BC_in_time_9_AC_in_time_10 {
    val results = List(Result("playerA", "playerB", true, 8), Result("playerB", "playerC", false, 9), Result("playerA", "playerC", true, 10))

    val transitionProbs = createInferDbnTennis(results, priorProb, emissionProb, transitionProb).getRatingTransitionProbabilities()
    assertEquals(4, transitionProbs.size)

    vectorAssert(List(0.3291, 0.0025, 0.0020, 0.0064, 0.4681, 0.0038, 0.0031, 0.0046, 0.1803), transitionProbs(0), 0.0001)
    vectorAssert(List(0.0904, 0.0014, 0.0016, 0.0034, 0.4831, 0.0059, 0.0024, 0.0070, 0.4049), transitionProbs(1), 0.0001)
    vectorAssert(List(0.0930, 0.0014, 0.0017, 0.0033, 0.4823, 0.0059, 0.0023, 0.0069, 0.4032), transitionProbs(2), 0.0001)
    vectorAssert(List(0.1825, 0.0014, 0.0011, 0.0068, 0.5048, 0.0042, 0.0048, 0.0073, 0.2871), transitionProbs(3), 0.0001)
  }

  /**Tests for log likelihood.*/
  @Test def loglikelihood_single_result {
    val results = List(Result("playerA", "playerB", true, 1))

    val loglikelihood = createInferDbnTennis(results, priorProb, emissionProb, transitionProb).logLikelihood()
    assertEquals(-0.6931, loglikelihood, 0.0001)

  }

  @Test def loglikelihood__AB_in_time_8_BC_in_time_9_AC_in_time_10 {
    val results = List(Result("playerA", "playerB", true, 8), Result("playerB", "playerC", false, 9), Result("playerA", "playerC", true, 10))

    val loglikelihood = createInferDbnTennis(results, priorProb, emissionProb, transitionProb).logLikelihood()
    assertEquals(-2.0499, loglikelihood, 0.0001)
  }

  @Test def loglikelihood__AB_in_time_8_BC_in_time_9_AC_in_time_10_all_results_unknown {
    val results = List(Result("playerA", "playerB", None, 8), Result("playerB", "playerC", None, 9), Result("playerA", "playerC", None, 10))

    val loglikelihood = createInferDbnTennis(results, priorProb, emissionProb, transitionProb).logLikelihood()
    assertEquals(0, loglikelihood, 0.0001)
  }

  @Test def loglikelihood_Gil_vs_Starace_the_same_result_twice {
    val results = List(Result("Frederico Gil", "Potito Starace", Some(false), 0), Result("Frederico Gil", "Potito Starace", Some(false), 0))

    val loglikelihood = createInferDbnTennis(results, priorProb, emissionProb, transitionProb).logLikelihood()
    assertEquals(-1.325, loglikelihood, 0.0001)
  }

  /**Tests for getPlayerAWinningProb.*/
  @Test def getPlayerAWinningProb {
    val results = List(Result("playerA", "playerB", true, 8), Result("playerB", "playerC", false, 9), Result("playerA", "playerC", None, 10))

    val inferDbnTennis = createInferDbnTennis(results, priorProb, emissionProb, transitionProb)

    assertEquals(0.4987, inferDbnTennis.getPlayerAWinningProb("playerA", "playerC", 10), 0.0001)
  }

  @Test def getPlayerAWinningProb_Federer_vs_Nadal {
    val results = List(Result("Roger Federer", "Rafael Nadal", true, 0), Result("Roger Federer", "Rafael Nadal", None, 0))

    val inferDbnTennis = createInferDbnTennis(results, priorProb, emissionProb, transitionProb)

    assertEquals(0.5320, inferDbnTennis.getPlayerAWinningProb("Roger Federer", "Rafael Nadal", 0), 0.0001)
  }

  @Test(expected = classOf[NoSuchElementException]) def getPlayerAWinningProb_no_result_variable_exists {
    val results = List(Result("playerA", "playerB", true, 8), Result("playerB", "playerC", false, 9), Result("playerA", "playerC", None, 10))

    val inferDbnTennis = createInferDbnTennis(results, priorProb, emissionProb, transitionProb)

    inferDbnTennis.getPlayerAWinningProb("playerA", "playerD", 8)
  }

  @Test def getPLayerAWinningProb_Federer_Nadal_infer_object_created_manually {

    val varR1 = new Variable(3)
    val varR2 = new Variable(3)
    val varScore = new Variable(2)

    val factorR1 = LogTableFactor.makeFromValues(Array(varR1), Array(5.647370307995246E-17, 2.4441886487929025E-5, 0.9999755581135146))
    val factorR2 = LogTableFactor.makeFromValues(Array(varR2), Array(1.0326870013899315E-11, 0.0014485133712921617, 0.9985514866183759))
    val factorScore = LogTableFactor.makeFromValues(Array(varR1, varR2, varScore), Array(0.5, 0.5, 1d / 3, 2d / 3, 0.25, 0.75, 2d / 3, 1d / 3, 0.5, 0.5, 0.4, 0.6, 0.75, 0.25, 0.6, 0.4, 0.5, 0.5))

    val factors = List(factorR1, factorR2, factorScore)

    val factorGraph = new FactorGraph()
    factors.foreach(f => factorGraph.addFactor(f))

    val resultVariables = List((Result("Roger Federer", "Rafael Nadal", None, 0) -> varScore))
    val playerVariables = Map[Int, Map[String, Variable]]()
    val inferTennis = GrmmInferDbnTennis(factorGraph, factorGraph, resultVariables, playerVariables)
    inferTennis.computeMarginals()

    val winningProb = inferTennis.getPlayerAWinningProb("Roger Federer", "Rafael Nadal", 0)
    assertEquals(0.5001, winningProb, 0.0001)

  }

  /**Tests for getPlayerRating.*/
  @Test(expected = classOf[NoSuchElementException]) def getPlayerRating_player_not_found {
    val results = List(Result("playerA", "playerB", true, 8), Result("playerB", "playerC", false, 9), Result("playerA", "playerC", None, 10))
    val inferDbnTennis = createInferDbnTennis(results, priorProb, emissionProb, transitionProb)
    inferDbnTennis.getPlayerRating("wrong player", 9)
  }

  @Test(expected = classOf[NoSuchElementException]) def getPlayerRating_player_not_found_player_not_found_in_a_time_slice {
    val results = List(Result("playerA", "playerB", true, 8), Result("playerB", "playerC", false, 9), Result("playerA", "playerC", None, 10))
    val inferDbnTennis = createInferDbnTennis(results, priorProb, emissionProb, transitionProb)
    inferDbnTennis.getPlayerRating("playerA", 11)
  }

  @Test def getPlayerRating {
    val results = List(Result("playerA", "playerB", true, 8), Result("playerB", "playerC", false, 9), Result("playerA", "playerC", None, 10))
    val inferDbnTennis = createInferDbnTennis(results, priorProb, emissionProb, transitionProb)
    val playerRating = inferDbnTennis.getPlayerRating("playerA", 9)

    vectorAssert(List(0.1442, 0.5021, 0.3535), playerRating, 0.0001)
  }

}