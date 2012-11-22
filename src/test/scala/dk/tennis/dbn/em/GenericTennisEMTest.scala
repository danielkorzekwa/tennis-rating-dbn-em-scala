package dk.tennis.dbn.em

import org.junit._
import Assert._
import scala.util.Random
import dk.tennis.dbn.MatchOutcome
import dk.bayes.factor.FactorUtil
import dk.bayes.em.EMLearn._
import dk.tennis.dbn.MatchOutcome
import dk.tennis.dbn.GenericTennisDBN
import TennisEM._
import dk.tennis.dbn._
import dk.tennis.dbn.util.AssertUtil._

class GenericTennisEMTest {

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

  private var llh: List[Double] = Nil
  private def progress(currentIter: Int, logLikelihood: Double) = {
    llh = logLikelihood :: llh;
    println("Log likelihood for iteration %d = %f".format(currentIter, logLikelihood))
  }

  /**
   * Tests for train().
   */

  @Test def train {

    val matches = MatchOutcome("P1", "P2", true, days(1)) :: MatchOutcome("P2", "P3", false, days(1)) :: Nil
    val iterNum = 1
    val tennisClusterGraph = GenericTennisDBN(matches, priorProb, emissionProb, transitionProb, 1).getTennisClusterGraph()
    val trainedParams = GenericTennisEM.train(tennisClusterGraph, iterNum, progress)

    assertEquals(3, trainedParams.priorProb.size)
    assertEquals(18, trainedParams.emissionProb.size)
    assertEquals(0, trainedParams.transitionProb.size)

    //  println(trainedParams.priorProb.map(e => e.formatted("%.4f")).toList)
    //  println(trainedParams.emissionProb.map(e => e.formatted("%.4f")).toList)
    //  println(trainedParams.transitionProb.map(e => e.formatted("%.4f")).toList)

    assertVector(List(0.2043, 0.4952, 0.3006), trainedParams.priorProb, 0.0001)
    assertVector(List(0.5000, 0.5000, 0.2739, 0.7261, 0.1684, 0.8316, 0.7261, 0.2739, 0.5000, 0.5000, 0.3493, 0.6507, 0.8316, 0.1684, 0.6507, 0.3493, 0.5000, 0.5000), trainedParams.emissionProb, 0.0001)
    assertVector(Nil, trainedParams.transitionProb, 0.0001)
  }

  @Test def train_2_iterations {

    val matches = MatchOutcome("P1", "P2", true, days(1)) :: MatchOutcome("P2", "P3", false, days(1)) :: Nil
    val iterNum = 2
    val tennisClusterGraph = GenericTennisDBN(matches, priorProb, emissionProb, transitionProb, 1).getTennisClusterGraph()
    val trainedParams = GenericTennisEM.train(tennisClusterGraph, iterNum, progress)

    assertEquals(3, trainedParams.priorProb.size)
    assertEquals(18, trainedParams.emissionProb.size)
    assertEquals(0, trainedParams.transitionProb.size)

    assertVector(List(0.2114, 0.4869, 0.3017), trainedParams.priorProb, 0.0001)
    assertVector(List(0.5000, 0.5000, 0.2092, 0.7908, 0.0924, 0.9076, 0.7908, 0.2092, 0.5000, 0.5000, 0.2779, 0.7221, 0.9076, 0.0924, 0.7221, 0.2779, 0.5000, 0.5000), trainedParams.emissionProb, 0.0001)
    assertVector(Nil, trainedParams.transitionProb, 0.0001)

    assertVector(List(-1.354720, -1.327130), llh.reverse, 0.0001)
  }

  @Test def train_5_iterations {

    val matches = MatchOutcome("P1", "P2", true, days(1)) :: MatchOutcome("P2", "P3", false, days(1)) :: Nil
    val iterNum = 5
    val tennisClusterGraph = GenericTennisDBN(matches, priorProb, emissionProb, transitionProb, 1).getTennisClusterGraph()
    val trainedParams = GenericTennisEM.train(tennisClusterGraph, iterNum, progress)

    assertEquals(3, trainedParams.priorProb.size)
    assertEquals(18, trainedParams.emissionProb.size)
    assertEquals(0, trainedParams.transitionProb.size)

    assertVector(List(-1.354720, -1.327130, -1.286615, -1.238214, -1.193132), llh.reverse, 0.0001)
  }

  @Test def train_check_for_convergence {

    val matches = MatchOutcome("P1", "P2", true, days(1)) :: MatchOutcome("P2", "P3", false, days(1)) :: Nil
    val iterNum = 500
    val tennisClusterGraph = GenericTennisDBN(matches, priorProb, emissionProb, transitionProb, 1).getTennisClusterGraph()
    val trainedParams = GenericTennisEM.train(tennisClusterGraph, iterNum, progress)

    assertEquals(3, trainedParams.priorProb.size)
    assertEquals(18, trainedParams.emissionProb.size)
    assertEquals(0, trainedParams.transitionProb.size)

    assertTrue(llh.size > 100)
  }

  @Test def train_all_results_for_two_time_slices {

    val matches = MatchOutcome("P1", "P2", true, days(1)) :: MatchOutcome("P1", "P3", true, days(1)) :: MatchOutcome("P2", "P3", false, days(1)) ::
      MatchOutcome("P1", "P2", true, days(2)) :: MatchOutcome("P1", "P3", false, days(2)) :: MatchOutcome("P2", "P3", false, days(2)) ::
      Nil
    val iterNum = 1
    val tennisClusterGraph = GenericTennisDBN(matches, priorProb, emissionProb, transitionProb, 1).getTennisClusterGraph()
    val trainedParams = GenericTennisEM.train(tennisClusterGraph, iterNum, progress)

    assertEquals(3, trainedParams.priorProb.size)
    assertEquals(18, trainedParams.emissionProb.size)
    assertEquals(9, trainedParams.transitionProb.size)

    assertVector(List(0.2192, 0.4745, 0.3063), trainedParams.priorProb, 0.0001)
    assertVector(List(0.5008, 0.4992, 0.1737, 0.8263, 0.1069, 0.8931, 0.8253, 0.1747, 0.5002, 0.4998, 0.3635, 0.6365, 0.8918, 0.1082, 0.6380, 0.3620, 0.5015, 0.4985), trainedParams.emissionProb, 0.0001)
    assertVector(List(0.9832, 0.0086, 0.0082, 0.0099, 0.9798, 0.0103, 0.0088, 0.0191, 0.9720), trainedParams.transitionProb, 0.0001)
  }

  @Test def train_all_results_for_two_time_slices_two_iterations {

    val matches = MatchOutcome("P1", "P2", true, days(1)) :: MatchOutcome("P1", "P3", true, days(1)) :: MatchOutcome("P2", "P3", false, days(1)) ::
      MatchOutcome("P1", "P2", true, days(2)) :: MatchOutcome("P1", "P3", false, days(2)) :: MatchOutcome("P2", "P3", false, days(2)) ::
      Nil

    val iterNum = 2
    val tennisClusterGraph = GenericTennisDBN(matches, priorProb, emissionProb, transitionProb, 1).getTennisClusterGraph()
    val trainedParams = GenericTennisEM.train(tennisClusterGraph, iterNum, progress)

    assertEquals(3, trainedParams.priorProb.size)
    assertEquals(18, trainedParams.emissionProb.size)
    assertEquals(9, trainedParams.transitionProb.size)

    assertVector(List(0.2560, 0.4366, 0.3073), trainedParams.priorProb, 0.0001)
    assertVector(List(0.5072, 0.4928, 0.0412, 0.9588, 0.0258, 0.9742, 0.9585, 0.0415, 0.5001, 0.4999, 0.3735, 0.6265, 0.9736, 0.0264, 0.6280, 0.3720, 0.5018, 0.4982), trainedParams.emissionProb, 0.0001)
    assertVector(List(0.9887, 0.0059, 0.0054, 0.0088, 0.9804, 0.0109, 0.0068, 0.0186, 0.9746), trainedParams.transitionProb, 0.0001)

    assertVector(List(-3.9678, -3.6233), llh.reverse, 0.0001)
  }

  @Test def train_all_results_for_50_time_slices_10_iterations {

    val matches = (1 to 50).flatMap(i => MatchOutcome("P1", "P2", true, days(i)) :: MatchOutcome("P1", "P3", true, days(i)) :: MatchOutcome("P2", "P3", false, days(i)) :: Nil)

    val iterNum = 10
    val tennisClusterGraph = GenericTennisDBN(matches, priorProb, emissionProb, transitionProb, 1).getTennisClusterGraph()
    val trainedParams = GenericTennisEM.train(tennisClusterGraph, iterNum, progress)

    assertEquals(3, trainedParams.priorProb.size)
    assertEquals(18, trainedParams.emissionProb.size)
    assertEquals(9, trainedParams.transitionProb.size)

    assertVector(List(0.5000, 0.0493, 0.4507), trainedParams.priorProb, 0.0001)
    assertVector(List(0.0000, 1.0000, 0.0000, 1.0000, 0.0000, 1.0000, 1.0000, 0.0000, 1.0000, 0.0000, 1.0000, 0.0000, 1.0000, 0.0000, 1.0000, 0.0000, 1.0000, 0.0000), trainedParams.emissionProb, 0.0001)
    assertVector(List(1.0000, 0.0000, 0.0000, 0.0000, 0.9516, 0.0484, 0.0000, 0.0029, 0.9971), trainedParams.transitionProb, 0.0001)

    assertVector(List(-60.4578, -2.5032, -1.5138, -1.4014, -1.3886, Double.PositiveInfinity, Double.NaN), llh.reverse.take(7), 0.0001)
  }

  @Test def train_all_results_for_5_time_slices_10_iterations_64_matches_in_every_time_slice {

    val rand = new Random(System.currentTimeMillis())
    val matches = (1 to 5).flatMap { dayIndex =>
      (1 to 64).map { matchIndex =>
        val winner = rand.nextDouble() < (matchIndex.toDouble / (matchIndex + 10))
        if (rand.nextBoolean())
          MatchOutcome("P" + matchIndex, "P" + matchIndex + 1, winner, days(dayIndex))
        else
          MatchOutcome("P" + matchIndex + 1, "P" + matchIndex, !winner, days(dayIndex))
      }
    }

    val iterNum = 10
    val tennisClusterGraph = GenericTennisDBN(matches, priorProb, emissionProb, transitionProb, 1).getTennisClusterGraph()
    val trainedParams = GenericTennisEM.train(tennisClusterGraph, iterNum, progress)

    assertEquals(3, trainedParams.priorProb.size)
    assertEquals(18, trainedParams.emissionProb.size)
    assertEquals(9, trainedParams.transitionProb.size)

    println(trainedParams.priorProb.map(e => e.formatted("%.4f")).toList)
    println(trainedParams.emissionProb.map(e => e.formatted("%.4f")).toList)
    println(trainedParams.transitionProb.map(e => e.formatted("%.4f")).toList)

  }

  private def days(num: Long): Long = num * (1000 * 60 * 60 * 24)
}