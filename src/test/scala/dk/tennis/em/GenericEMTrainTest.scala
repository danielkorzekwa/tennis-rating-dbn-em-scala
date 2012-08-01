package dk.tennis.em

import org.junit._
import Assert._
import EMTrain._
import dbn.DbnTennis._

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

  val results = Result("P1", "P2", true, 1) :: Result("P2", "P3", true, 2) :: Nil

  /**
   * Tests for train().
   */

  @Test @Ignore def train {

    val iterNum = 5;
    def progress(currentIter: Int, logLikelihood: Double) = println("Log likelihood for iteration %d = %f".format(currentIter, logLikelihood))
    val trainedParams = emTrain.train(parameters, results, iterNum, progress)

    assertEquals(3, trainedParams.priorProb.size)
    assertEquals(9, trainedParams.emissionProb.size)
    assertEquals(9, trainedParams.transitionProb.size)
  }

  /**
   * Tests for expectationStep().
   */
  @Test @Ignore def expectationStep {
    val sufficientStats = emTrain.expectationStep(parameters, results)

    assertEquals(3, sufficientStats.priorStatsNum)
    //assertEquals(9, sufficientStats.emissionStatsNum)
    // assertEquals(6, sufficientStats.transitionStatsNum)

    assertEquals(0.58674, sufficientStats.priorStats(0))
    assertEquals(1.51496, sufficientStats.priorStats(1))
    assertEquals(0.89831, sufficientStats.priorStats(2))

  }

  /**
   * Tests for maximizationStep().
   */
}