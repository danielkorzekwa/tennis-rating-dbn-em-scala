package dk.tennis.em

import org.junit._
import Assert._
import EMTrain._
import dk.tennis.em.util.AssertUtil._
import dk.tennis.em.dbn.infer.generic.GenericInferDbnTennisFactory
import dk.tennis.em.dbn.infer.grmm.GrmmInferDbnTennisFactory
import dk.tennis.em.dbn.factorgraph.DbnTennis.Result

class GenericEMTrainFullJoinInferenceTest {

  val emTrain = new GenericEMTrain(GenericInferDbnTennisFactory())

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

  private var llh: List[Double] = Nil
  private def progress(currentIter: Int, logLikelihood: Double) = {
    llh = logLikelihood :: llh
    println("Log likelihood for iteration %d = %f".format(currentIter, logLikelihood))
  }

  /**
   * Tests for train().
   */

  @Test def train {

    val results = Result("P1", "P2", true, 1) :: Result("P2", "P3", false, 1) :: Nil
    val iterNum = 1

    val trainedParams = emTrain.train(parameters, results, iterNum, progress)

    assertEquals(3, trainedParams.priorProb.size)
    assertEquals(18, trainedParams.emissionProb.size)
    assertEquals(0, trainedParams.transitionProb.size)

    //println(trainedParams.priorProb.map(e => e.formatted("%.4f")).toList)
    assertVector(List(0.2043, 0.4952, 0.3006), trainedParams.priorProb, 0.0001)
    assertVector(List(0.5000, 0.5000, 0.2739, 0.7261, 0.1684, 0.8316, 0.7261, 0.2739, 0.5000, 0.5000, 0.3493, 0.6507, 0.8316, 0.1684, 0.6507, 0.3493, 0.5000, 0.5000), trainedParams.emissionProb, 0.0001)
    assertVector(Nil, trainedParams.transitionProb, 0.0001)
  }

  @Test def train_2_iterations {

    val results = Result("P1", "P2", true, 1) :: Result("P2", "P3", false, 1) :: Nil
    val iterNum = 2
    val trainedParams = emTrain.train(parameters, results, iterNum, progress)

    assertEquals(3, trainedParams.priorProb.size)
    assertEquals(18, trainedParams.emissionProb.size)
    assertEquals(0, trainedParams.transitionProb.size)

    assertVector(List(0.2114, 0.4869, 0.3017), trainedParams.priorProb, 0.0001)
    assertVector(List(0.5000, 0.5000, 0.2092, 0.7908, 0.0924, 0.9076, 0.7908, 0.2092, 0.5000, 0.5000, 0.2779, 0.7221, 0.9076, 0.0924, 0.7221, 0.2779, 0.5000, 0.5000), trainedParams.emissionProb, 0.0001)
    assertVector(Nil, trainedParams.transitionProb, 0.0001)

    assertVector(List(-1.354720, -1.327130), llh.reverse, 0.0001)
  }

  @Test def train_5_iterations {

    val results = Result("P1", "P2", true, 1) :: Result("P2", "P3", false, 1) :: Nil
    val iterNum = 5
    val trainedParams = emTrain.train(parameters, results, iterNum, progress)

    assertEquals(3, trainedParams.priorProb.size)
    assertEquals(18, trainedParams.emissionProb.size)
    assertEquals(0, trainedParams.transitionProb.size)

    assertVector(List(-1.354720, -1.327130, -1.286615, -1.238214, -1.193132), llh.reverse, 0.0001)
  }

  @Test def train_check_for_convergence {

    val results = Result("P1", "P2", true, 1) :: Result("P2", "P3", false, 1) :: Nil
    val iterNum = 500
    val trainedParams = emTrain.train(parameters, results, iterNum, progress)

    assertEquals(3, trainedParams.priorProb.size)
    assertEquals(18, trainedParams.emissionProb.size)
    assertEquals(0, trainedParams.transitionProb.size)

    assertEquals(500, llh.size)
  }

  @Test def train_all_results_for_two_time_slices {

    val results = Result("P1", "P2", true, 1) :: Result("P1", "P3", true, 1) :: Result("P2", "P3", false, 1) ::
      Result("P1", "P2", true, 2) :: Result("P1", "P3", false, 2) :: Result("P2", "P3", false, 2) ::
      Nil
    val iterNum = 1
    val trainedParams = emTrain.train(parameters, results, iterNum, progress)

    assertEquals(3, trainedParams.priorProb.size)
    assertEquals(18, trainedParams.emissionProb.size)
    assertEquals(9, trainedParams.transitionProb.size)

    assertVector(List(0.21967, 0.47392, 0.30641), trainedParams.priorProb, 0.0001)
    assertVector(List(0.5006, 0.4994, 0.1688, 0.8312, 0.1023, 0.8977, 0.8303, 0.1697, 0.5002, 0.4998, 0.3585, 0.6415, 0.8965, 0.1035, 0.6429, 0.3571, 0.5015, 0.4985), trainedParams.emissionProb, 0.0001)
    assertVector(List(0.9833, 0.0085, 0.0081, 0.0099, 0.9798, 0.0103, 0.0088, 0.0191, 0.9721), trainedParams.transitionProb, 0.0001)
  }

  @Test def train_all_results_for_two_time_slices_two_iterations {

    val results = Result("P1", "P2", true, 1) :: Result("P1", "P3", true, 1) :: Result("P2", "P3", false, 1) ::
      Result("P1", "P2", true, 2) :: Result("P1", "P3", false, 2) :: Result("P2", "P3", false, 2) ::
      Nil

    val iterNum = 2
    val trainedParams = emTrain.train(parameters, results.toList, iterNum, progress)

    assertEquals(3, trainedParams.priorProb.size)
    assertEquals(18, trainedParams.emissionProb.size)
    assertEquals(9, trainedParams.transitionProb.size)

    assertVector(List(0.2583, 0.4343, 0.3073), trainedParams.priorProb, 0.0001)
    assertVector(List(0.5055, 0.4945, 0.0366, 0.9634, 0.0214, 0.9786, 0.9632, 0.0368, 0.5001, 0.4999, 0.3557, 0.6443, 0.9781, 0.0219, 0.6458, 0.3542, 0.5016, 0.4984), trainedParams.emissionProb, 0.0001)
    assertVector(List(0.9890, 0.0058, 0.0051, 0.0087, 0.9804, 0.0109, 0.0065, 0.0185, 0.9750), trainedParams.transitionProb, 0.0001)

    assertVector(List(-3.965451, -3.598777), llh.reverse, 0.0001)
  }

  /**
   * Tests for expectationStep().
   */
  @Test(expected = classOf[IllegalArgumentException]) def expectationStep_no_results {
    val results = Nil
    val sufficientStats = emTrain.expectationStep(parameters, results)
  }

  @Test def expectationStep_single_result {
    val results = Result("P1", "P2", true, 1) :: Nil
    val sufficientStats = emTrain.expectationStep(parameters, results)

    assertEquals(2, sufficientStats.priorStatsNum)
    assertEquals(1, sufficientStats.emissionStatsNum)
    assertEquals(0, sufficientStats.transitionStatsNum)

    assertVector(List(0.4000, 1.0000, 0.6000), sufficientStats.priorStats, 0.0001)
    assertVector(List(0.0400, 0.0000, 0.0667, 0.0000, 0.0300, 0.0000, 0.1333, 0.0000, 0.2500, 0.0000, 0.1200, 0.0000, 0.0900, 0.0000, 0.1800, 0.0000, 0.0900, 0.0000), sufficientStats.emissionStats, 0.0001)
    assertVector(Nil, sufficientStats.transitionStats, 0.0001)

  }

  @Test def expectationStep {
    val results = Result("P1", "P2", true, 1) :: Result("P2", "P3", true, 2) :: Nil
    val sufficientStats = emTrain.expectationStep(parameters, results)

    assertEquals(3, sufficientStats.priorStatsNum)
    assertEquals(2, sufficientStats.emissionStatsNum)
    assertEquals(1, sufficientStats.transitionStatsNum)

    assertVector(List(0.5888, 1.5151, 0.8961), sufficientStats.priorStats, 0.0001)
    assertVector(List(0.0835, 0.0000, 0.1608, 0.0000, 0.0781, 0.0000, 0.2316, 0.0000, 0.5153, 0.0000, 0.2704, 0.0000, 0.1389, 0.0000, 0.3362, 0.0000, 0.1854, 0.0000), sufficientStats.emissionStats, 0.0001)
    assertVector(List(0.1823, 0.0027, 0.0033, 0.0035, 0.5065, 0.0062, 0.0017, 0.0050, 0.2888), sufficientStats.transitionStats, 0.0001)

  }

  @Test def expectationStep_all_results_for_two_time_slices {
    val results = Result("P1", "P2", true, 1) :: Result("P1", "P3", true, 1) :: Result("P2", "P3", false, 1) ::
      Result("P1", "P2", true, 2) :: Result("P1", "P3", false, 2) :: Result("P2", "P3", false, 2) ::
      Nil

    val sufficientStats = emTrain.expectationStep(parameters, results)

    assertEquals(3, sufficientStats.priorStatsNum)
    assertEquals(6, sufficientStats.emissionStatsNum)
    assertEquals(3, sufficientStats.transitionStatsNum)
    assertVector(List(0.6590, 1.4218, 0.9192), sufficientStats.priorStats, 0.0001)
    assertVector(List(0.1119, 0.1117, 0.1112, 0.5474, 0.0459, 0.4028, 0.5450, 0.1114, 0.6651, 0.6645, 0.3051, 0.5459, 0.4011, 0.0463, 0.5469, 0.3039, 0.2677, 0.2661), sufficientStats.emissionStats, 0.0001)
    assertVector(List(0.6480, 0.0056, 0.0054, 0.0141, 1.3931, 0.0146, 0.0081, 0.0176, 0.8936), sufficientStats.transitionStats, 0.0001)

  }

  /**
   * Tests for maximizationStep().
   */
  @Test def maximizationStep_no_stats {
    val sufficientStats = SufficientStats(Nil, 0, Nil, 0, Nil, 0, -0.69)
    val params = emTrain.maximizationStep(sufficientStats)

    assertVector(Nil, params.priorProb, 0.0001)
    assertVector(Nil, params.emissionProb, 0.0001)
    assertVector(Nil, params.transitionProb, 0.0001)
  }

  @Test def maximizationStep {
    val priorStats = List(0.5888, 1.5151, 0.8961)
    val emissionStats = List(0.0835, 0.0000, 0.1608, 0.0000, 0.0781, 0.0000, 0.2316, 0.0000, 0.5153, 0.0000, 0.2704, 0.0000, 0.1389, 0.0000, 0.3362, 0.0000, 0.1854, 0.0000)
    val transitionStats = List(0.1823, 0.0027, 0.0033, 0.0035, 0.5065, 0.0062, 0.0017, 0.0050, 0.2888)

    val sufficientStats = SufficientStats(priorStats, 3, emissionStats, 2, transitionStats, 1, -0.69)
    val params = emTrain.maximizationStep(sufficientStats)

    assertVector(List(0.1963, 0.5050, 0.2987), params.priorProb, 0.0001)
    assertVector(List(1, 0.0000, 1, 0.0000, 1, 0.0000, 1, 0.0000, 1, 0.0000, 1, 0.0000, 1, 0.0000, 1, 0.0000, 1, 0.0000), params.emissionProb, 0.0001)
    assertVector(List(0.9681, 0.0143, 0.0175, 0.0068, 0.9812, 0.0120, 0.0058, 0.0169, 0.9773), params.transitionProb, 0.0001)
  }

  @Test def maximizationStep_all_results_for_two_time_slices {
    val priorStats = List(0.6590, 1.4218, 0.9192)
    val emissionStats = List(0.1119, 0.1117, 0.1112, 0.5474, 0.0459, 0.4028, 0.5450, 0.1114, 0.6651, 0.6645, 0.3051, 0.5459, 0.4011, 0.0463, 0.5469, 0.3039, 0.2677, 0.2661)
    val transitionStats = List(0.6480, 0.0056, 0.0054, 0.0141, 1.3931, 0.0146, 0.0081, 0.0176, 0.8936)

    val sufficientStats = SufficientStats(priorStats, 3, emissionStats, 2, transitionStats, 1, -0.69)
    val params = emTrain.maximizationStep(sufficientStats)

    assertVector(List(0.2197, 0.4739, 0.3064), params.priorProb, 0.0001)
    assertVector(List(0.5004, 0.4996, 0.1688, 0.8312, 0.1023, 0.8977, 0.8303, 0.1697, 0.5002, 0.4998, 0.3585, 0.6415, 0.8965, 0.1035, 0.6428, 0.3572, 0.5015, 0.4985), params.emissionProb, 0.0001)
    assertVector(List(0.9833, 0.0085, 0.0082, 0.0099, 0.9798, 0.0103, 0.0088, 0.0191, 0.9720), params.transitionProb, 0.0001)
  }
}