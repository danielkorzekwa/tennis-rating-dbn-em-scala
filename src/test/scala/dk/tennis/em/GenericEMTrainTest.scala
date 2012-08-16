package dk.tennis.em

import org.junit._
import Assert._
import EMTrain._
import dbn.DbnTennis._
import dk.tennis.em.util.VectorAssert._

class GenericEMTrainTest {

  val emTrain = new GenericEMTrain()

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

  val parameters = Params(priorProb, emissionProb, transitionProb)

  /**
   * Tests for train().
   */

  @Test def train {

    val results = Result("P1", "P2", true, 1) :: Result("P2", "P3", false, 1) :: Nil
    val iterNum = 1;
    def progress(currentIter: Int, logLikelihood: Double) = println("Log likelihood for iteration %d = %f".format(currentIter, logLikelihood))
    val trainedParams = emTrain.train(parameters, results, iterNum, progress)

    assertEquals(3, trainedParams.priorProb.size)
    assertEquals(18, trainedParams.emissionProb.size)
    assertEquals(0, trainedParams.transitionProb.size)

    //println(trainedParams.priorProb.map(e => e.formatted("%.4f")).toList)
    vectorAssert(List(0.2043, 0.4952, 0.3006), trainedParams.priorProb, 0.0001)
    vectorAssert(List(0.5000, 0.5000, 0.2739, 0.7261, 0.1684, 0.8316, 0.7261, 0.2739, 0.5000, 0.5000, 0.3493, 0.6507, 0.8316, 0.1684, 0.6507, 0.3493, 0.5000, 0.5000), trainedParams.emissionProb, 0.0001)
    vectorAssert(Nil, trainedParams.transitionProb, 0.0001)
  }

  @Test def train_all_results_for_two_time_slices {

    val results = Result("P1", "P2", true, 1) :: Result("P1", "P3", true, 1) :: Result("P2", "P3", false, 1) ::
      Result("P1", "P2", true, 2) :: Result("P1", "P3", false, 2) :: Result("P2", "P3", false, 2) ::
      Nil
    val iterNum = 1;
    def progress(currentIter: Int, logLikelihood: Double) = println("Log likelihood for iteration %d = %f".format(currentIter, logLikelihood))
    val trainedParams = emTrain.train(parameters, results, iterNum, progress)

    assertEquals(3, trainedParams.priorProb.size)
    assertEquals(18, trainedParams.emissionProb.size)
    assertEquals(9, trainedParams.transitionProb.size)

    vectorAssert(List(0.21967, 0.47392, 0.30641), trainedParams.priorProb, 0.0001)
    vectorAssert(List(0.5006, 0.4994, 0.1688, 0.8312, 0.1023, 0.8977, 0.8303, 0.1697, 0.5002, 0.4998, 0.3585, 0.6415, 0.8965, 0.1035, 0.6429, 0.3571, 0.5015, 0.4985), trainedParams.emissionProb, 0.0001)
    vectorAssert(List(0.9833, 0.0085, 0.0081, 0.0099, 0.9798, 0.0103, 0.0088, 0.0191, 0.9721), trainedParams.transitionProb, 0.0001)
  }

  /**
   * Tests for expectationStep().
   */
  @Test def expectationStep_no_results {
    val results = Nil
    val sufficientStats = emTrain.expectationStep(parameters, results)

    assertEquals(0, sufficientStats.priorStatsNum)
    assertEquals(0, sufficientStats.emissionStatsNum)
    assertEquals(0, sufficientStats.transitionStatsNum)

    vectorAssert(Nil, sufficientStats.priorStats, 0.0001)
    vectorAssert(Nil, sufficientStats.emissionStats, 0.0001)
    vectorAssert(Nil, sufficientStats.transitionStats, 0.0001)

  }

  @Test def expectationStep_single_result {
    val results = Result("P1", "P2", true, 1) :: Nil
    val sufficientStats = emTrain.expectationStep(parameters, results)

    assertEquals(2, sufficientStats.priorStatsNum)
    assertEquals(1, sufficientStats.emissionStatsNum)
    assertEquals(0, sufficientStats.transitionStatsNum)

    vectorAssert(List(0.4000, 1.0000, 0.6000), sufficientStats.priorStats, 0.0001)
    vectorAssert(List(0.0400, 0.0000, 0.0667, 0.0000, 0.0300, 0.0000, 0.1333, 0.0000, 0.2500, 0.0000, 0.1200, 0.0000, 0.0900, 0.0000, 0.1800, 0.0000, 0.0900, 0.0000), sufficientStats.emissionStats, 0.0001)
    vectorAssert(Nil, sufficientStats.transitionStats, 0.0001)

  }

  @Test def expectationStep {
    val results = Result("P1", "P2", true, 1) :: Result("P2", "P3", true, 2) :: Nil
    val sufficientStats = emTrain.expectationStep(parameters, results)

    assertEquals(3, sufficientStats.priorStatsNum)
    assertEquals(2, sufficientStats.emissionStatsNum)
    assertEquals(1, sufficientStats.transitionStatsNum)

    vectorAssert(List(0.5888, 1.5151, 0.8961), sufficientStats.priorStats, 0.0001)
    vectorAssert(List(0.0835, 0.0000, 0.1608, 0.0000, 0.0781, 0.0000, 0.2316, 0.0000, 0.5153, 0.0000, 0.2704, 0.0000, 0.1389, 0.0000, 0.3362, 0.0000, 0.1854, 0.0000), sufficientStats.emissionStats, 0.0001)
    vectorAssert(List(0.1823, 0.0027, 0.0033, 0.0035, 0.5065, 0.0062, 0.0017, 0.0050, 0.2888), sufficientStats.transitionStats, 0.0001)

  }

  @Test def expectationStep_all_results_for_two_time_slices {
    val results = Result("P1", "P2", true, 1) :: Result("P1", "P3", true, 1) :: Result("P2", "P3", false, 1) ::
      Result("P1", "P2", true, 2) :: Result("P1", "P3", false, 2) :: Result("P2", "P3", false, 2) ::
      Nil

    val sufficientStats = emTrain.expectationStep(parameters, results)

    assertEquals(3, sufficientStats.priorStatsNum)
    assertEquals(6, sufficientStats.emissionStatsNum)
    assertEquals(3, sufficientStats.transitionStatsNum)
    println(sufficientStats.priorStats.toList)
    vectorAssert(List(0.6590, 1.4218, 0.9192), sufficientStats.priorStats, 0.0001)
    vectorAssert(List(0.1119, 0.1117, 0.1112, 0.5474, 0.0459, 0.4028, 0.5450, 0.1114, 0.6651, 0.6645, 0.3051, 0.5459, 0.4011, 0.0463, 0.5469, 0.3039, 0.2677, 0.2661), sufficientStats.emissionStats, 0.0001)
    vectorAssert(List(0.6480, 0.0056, 0.0054, 0.0141, 1.3931, 0.0146, 0.0081, 0.0176, 0.8936), sufficientStats.transitionStats, 0.0001)

  }

  /**
   * Tests for maximizationStep().
   */
  @Test def maximizationStep_no_stats {
    val sufficientStats = SufficientStats(Nil, 0, Nil, 0, Nil, 0)
    val params = emTrain.maximizationStep(sufficientStats)

    vectorAssert(Nil, params.priorProb, 0.0001)
    vectorAssert(Nil, params.emissionProb, 0.0001)
    vectorAssert(Nil, params.transitionProb, 0.0001)
  }

  @Test def maximizationStep {
    val priorStats = List(0.5888, 1.5151, 0.8961)
    val emissionStats = List(0.0835, 0.0000, 0.1608, 0.0000, 0.0781, 0.0000, 0.2316, 0.0000, 0.5153, 0.0000, 0.2704, 0.0000, 0.1389, 0.0000, 0.3362, 0.0000, 0.1854, 0.0000)
    val transitionStats = List(0.1823, 0.0027, 0.0033, 0.0035, 0.5065, 0.0062, 0.0017, 0.0050, 0.2888)

    val sufficientStats = SufficientStats(priorStats, 3, emissionStats, 2, transitionStats, 1)
    val params = emTrain.maximizationStep(sufficientStats)

    vectorAssert(List(0.1963, 0.5050, 0.2987), params.priorProb, 0.0001)
    vectorAssert(List(0.0418, 0.0000, 0.0804, 0.0000, 0.0391, 0.0000, 0.1158, 0.0000, 0.2577, 0.0000, 0.1352, 0.0000, 0.0695, 0.0000, 0.1681, 0.0000, 0.0927, 0.0000), params.emissionProb, 0.0001)
    vectorAssert(List(0.1823, 0.0027, 0.0033, 0.0035, 0.5065, 0.0062, 0.0017, 0.0050, 0.2888), params.transitionProb, 0.0001)
  }

  @Test def maximizationStep_all_results_for_two_time_slices {
    val priorStats = List(0.6590, 1.4218, 0.9192)
    val emissionStats = List(0.1119, 0.1117, 0.1112, 0.5474, 0.0459, 0.4028, 0.5450, 0.1114, 0.6651, 0.6645, 0.3051, 0.5459, 0.4011, 0.0463, 0.5469, 0.3039, 0.2677, 0.2661)
    val transitionStats = List(0.6480, 0.0056, 0.0054, 0.0141, 1.3931, 0.0146, 0.0081, 0.0176, 0.8936)

    val sufficientStats = SufficientStats(priorStats, 3, emissionStats, 2, transitionStats, 1)
    val params = emTrain.maximizationStep(sufficientStats)

    vectorAssert(List(0.2197, 0.4739, 0.3064), params.priorProb, 0.0001)
    vectorAssert(List(0.0560, 0.0559, 0.0556, 0.2737, 0.0230, 0.2014, 0.2725, 0.0557, 0.3326, 0.3323, 0.1526, 0.2730, 0.2006, 0.0232, 0.2735, 0.1520, 0.1339, 0.1331), params.emissionProb, 0.0001)
    vectorAssert(List(0.6480, 0.0056, 0.0054, 0.0141, 1.3931, 0.0146, 0.0081, 0.0176, 0.8936), params.transitionProb, 0.0001)
  }
}