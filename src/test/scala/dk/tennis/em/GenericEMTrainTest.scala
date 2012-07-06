package dk.tennis.em

import org.junit._
import Assert._
import EMTrain._

class GenericEMTrainTest {

  val emTrain = new GenericEMTrain()

  /**
   * Tests for train().
   * */
  
  @Test def train {

    val parameters = Params(Nil, Nil, Nil)
    val results = Nil
    val iterNum = 5;
    def progress(currentIter: Int, logLikelihood: Double) = println("Log likelihood for iteration %d = %f".format(currentIter, logLikelihood))
    val trainedParams = emTrain.train(parameters, results, iterNum, progress)

    assertEquals(trainedParams.priorProb.size, parameters.priorProb.size)
    assertEquals(trainedParams.emissionProb.size, parameters.emissionProb.size)
    assertEquals(trainedParams.transitionProb.size, parameters.transitionProb.size)
  }
  
   /**
   * Tests for expectationStep().
   * */
  
   /**
   * Tests for maximizationStep().
   * */
}