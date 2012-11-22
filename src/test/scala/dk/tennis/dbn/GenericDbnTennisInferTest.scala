package dk.tennis.dbn

import org.junit._
import Assert._
import dk.tennis.dbn.util.AssertUtil._

class ClusterLoopyBPInferDbnTennisFactoryTest2 {

  private val priorProb = Array(0.2, 0.5, 0.3)

  private val emissionProb = Array(
    0.5, 0.5,
    1d / 3, 2d / 3,
    0.25, 0.75,
    2d / 3, 1d / 3,
    0.5, 0.5,
    2d / 5, 3d / 5,
    3d / 4, 1d / 4,
    3d / 5, 2d / 5,
    0.5, 0.5)

  private val transitionProb = Array(0.98, 0.01, 0.01, 0.01, 0.98, 0.01, 0.01, 0.02, 0.97)

  private val timeSliceInDays = 1

  /**
   * Tests for getPlayer Prior Rating
   *
   */
  /**Tests for getPlayerRating.*/
  @Test(expected = classOf[NoSuchElementException]) def getPlayerRating_player_not_found {
    val matches = List(MatchOutcome("playerA", "playerB", true, days(8)), MatchOutcome("playerB", "playerC", false, days(9)), MatchOutcome("playerA", "playerC", None, days(10)))

    val tennisDBN = GenericTennisDBN(matches, priorProb, emissionProb, transitionProb, timeSliceInDays)
    tennisDBN.calibrate()

    tennisDBN.getPlayerRating("wrong player", days(9))
  }

  @Test(expected = classOf[NoSuchElementException]) def getPlayerRating_player_not_found_player_not_found_in_a_time_slice {
    val matches = List(MatchOutcome("playerA", "playerB", true, days(8)), MatchOutcome("playerB", "playerC", false, days(9)), MatchOutcome("playerA", "playerC", None, days(10)))
    val tennisDBN = GenericTennisDBN(matches, priorProb, emissionProb, transitionProb, timeSliceInDays)
    tennisDBN.calibrate()

    tennisDBN.getPlayerRating("playerA", days(11))
  }

  @Test def getPriorRating_single_result {
    val matches = MatchOutcome("playerA", "playerB", true, days(1)) :: Nil

    val tennisDBN = GenericTennisDBN(matches, priorProb, emissionProb, transitionProb, timeSliceInDays)
    tennisDBN.calibrate()

    val playerARating = tennisDBN.getPlayerRating("playerA", days(1))
    val playerBRating = tennisDBN.getPlayerRating("playerB", days(1))

    assertVector(List(0.1366, 0.5033, 0.36), playerARating.toList, 0.0001)
    assertVector(List(0.2633, 0.49666, 0.24), playerBRating.toList, 0.0001)
  }

  @Test def getPriorRating_two_matches_for_player_A_in_time_slice_4 {
    val matches = List(MatchOutcome("playerA", "playerB", true, days(4)), MatchOutcome("playerA", "playerC", true, days(4)))

    val tennisDBN = GenericTennisDBN(matches, priorProb, emissionProb, transitionProb, timeSliceInDays)
    tennisDBN.calibrate()

    val playerARating = tennisDBN.getPlayerRating("playerA", days(4))
    val playerBRating = tennisDBN.getPlayerRating("playerB", days(4))
    val playerCRating = tennisDBN.getPlayerRating("playerC", days(4))

    assertVector(List(0.0904, 0.4909, 0.4185), playerARating, 0.0001)
    assertVector(List(0.2611, 0.4972, 0.2415), playerBRating, 0.0001)
    assertVector(List(0.2611, 0.4972, 0.2415), playerCRating, 0.0001)
  }

  @Test def getPriorRating_two_opposite_matches_between_players_A_and_B_in_the_same_time_slice {
    val matches = List(MatchOutcome("playerA", "playerB", true, hours(4)), MatchOutcome("playerA", "playerB", false, hours(5)))

    val tennisDBN = GenericTennisDBN(matches, priorProb, emissionProb, transitionProb, timeSliceInDays)
    tennisDBN.calibrate()

    val playerARating = tennisDBN.getPlayerRating("playerA", hours(4))
    val playerBRating = tennisDBN.getPlayerRating("playerB", hours(4))

    assertVector(List(0.1853, 0.5165, 0.2981), playerARating, 0.0001)
    assertVector(List(0.1853, 0.5165, 0.2981), playerBRating, 0.0001)

  }

  @Test def getPriorRating_AB_in_time_4_AC_in_time_5 {
    val matches = List(MatchOutcome("playerA", "playerB", true, days(4)), MatchOutcome("playerA", "playerC", true, days(5)))

    val tennisDBN = GenericTennisDBN(matches, priorProb, emissionProb, transitionProb, timeSliceInDays)
    tennisDBN.calibrate()

    val playerARating = tennisDBN.getPlayerRating("playerA", days(4))
    val playerBRating = tennisDBN.getPlayerRating("playerB", days(4))
    val playerCRating = tennisDBN.getPlayerRating("playerC", days(5))

    assertVector(List(0.0919, 0.4916, 0.4165), playerARating, 0.0001)
    assertVector(List(0.2612, 0.49725, 0.24149), playerBRating, 0.0001)
    assertVector(List(0.2613, 0.4972, 0.2414), playerCRating, 0.0001)
  }

  @Test def getPriorRating_AB_in_time_4_BC_in_time_5 {
    val matches = List(MatchOutcome("playerA", "playerB", true, days(4)), MatchOutcome("playerB", "playerC", true, days(5)))

    val tennisDBN = GenericTennisDBN(matches, priorProb, emissionProb, transitionProb, timeSliceInDays)
    tennisDBN.calibrate()

    val playerARating = tennisDBN.getPlayerRating("playerA", days(4))
    val playerBRating = tennisDBN.getPlayerRating("playerB", days(4))
    val playerCRating = tennisDBN.getPlayerRating("playerC", days(5))

    assertVector(List(0.1348, 0.5029, 0.3622), playerARating, 0.0001)
    assertVector(List(0.1883, 0.5162, 0.2954), playerBRating, 0.0001)
    assertVector(List(0.2656, 0.4960, 0.2383), playerCRating, 0.0001)
  }

  /**
   * Tests for getPlayerRating
   *
   */

  @Test def getRatingTransitionProbabilities_AB_in_time_8_BC_in_time_9 {
    val matches = List(MatchOutcome("playerA", "playerB", true, days(8)), MatchOutcome("playerB", "playerC", false, days(9)))

    val tennisDBN = GenericTennisDBN(matches, priorProb, emissionProb, transitionProb, timeSliceInDays)
    tennisDBN.calibrate()

    val playerCRating = tennisDBN.getPlayerRating("playerC", days(9))

    assertVector(List(0.1385, 0.5037, 0.3578), playerCRating, 0.0001)
  }

  @Test def getRatingTransitionProbabilities_AB_in_time_8_BC_in_time_10 {
    val matches = List(MatchOutcome("playerA", "playerB", true, days(8)), MatchOutcome("playerB", "playerC", false, days(10)))

    val tennisDBN = GenericTennisDBN(matches, priorProb, emissionProb, transitionProb, timeSliceInDays)
    tennisDBN.calibrate()

    val playerBRating = tennisDBN.getPlayerRating("playerB", days(10))
    val playerCRating = tennisDBN.getPlayerRating("playerC", days(10))

    assertVector(List(0.3408, 0.4727, 0.1864), playerBRating, 0.0001)
    assertVector(List(0.1385, 0.5037, 0.3577), playerCRating, 0.0001)
  }

  @Test def getRatingTransitionProbabilities_AB_in_time_8_BC_in_time_9_AC_in_time_10 {
    val matches = List(MatchOutcome("playerA", "playerB", true, days(8)), MatchOutcome("playerB", "playerC", false, days(9)), MatchOutcome("playerA", "playerC", true, days(10)))

    val tennisDBN = GenericTennisDBN(matches, priorProb, emissionProb, transitionProb, timeSliceInDays)
    tennisDBN.calibrate()

    val playerB9Rating = tennisDBN.getPlayerRating("playerB", days(9))
    val playerC9Rating = tennisDBN.getPlayerRating("playerC", days(9))

    val playerA10Rating = tennisDBN.getPlayerRating("playerA", days(10))
    val playerC10Rating = tennisDBN.getPlayerRating("playerC", days(10))

    assertVector(List(0.3386, 0.4753, 0.1861), playerB9Rating, 0.0001)
    assertVector(List(0.1851, 0.5157, 0.2992), playerC9Rating, 0.0001)
    assertVector(List(0.0986, 0.4906, 0.4108), playerA10Rating, 0.0001)
    assertVector(List(0.1941, 0.5135, 0.2924), playerC10Rating, 0.0001)
  }

  @Test def getPlayerRating {
    val matches = List(MatchOutcome("playerA", "playerB", true, days(8)), MatchOutcome("playerB", "playerC", false, days(9)), MatchOutcome("playerA", "playerC", None, days(10)))
    val tennisDBN = GenericTennisDBN(matches, priorProb, emissionProb, transitionProb, timeSliceInDays)
    tennisDBN.calibrate()

    val playerRating = tennisDBN.getPlayerRating("playerA", days(9))

    assertVector(List(0.1442, 0.5021, 0.3535), playerRating, 0.0001)
  }

  /**
   * Tests for getMatchProbability
   */

  @Test def getScoreEmissionProbabilities_AB_in_time_4 {
    val matches = List(MatchOutcome("playerA", "playerB", true, days(4)))

    val tennisDBN = GenericTennisDBN(matches, priorProb, emissionProb, transitionProb, timeSliceInDays)
    tennisDBN.calibrate()

    val matchProb = tennisDBN.getMatchProbability("playerA", "playerB", days(4))
    assertEquals(1d, matchProb, 0)
  }

  @Test def getScoreEmissionProbabilities_AB_and_BC_in_time_4 {
    val matches = List(MatchOutcome("playerA", "playerB", true, days(4)), MatchOutcome("playerB", "playerC", true, days(4)))

    val tennisDBN = GenericTennisDBN(matches, priorProb, emissionProb, transitionProb, timeSliceInDays)
    tennisDBN.calibrate()

    val matchProbAB = tennisDBN.getMatchProbability("playerA", "playerB", days(4))
    val matchProbBC = tennisDBN.getMatchProbability("playerB", "playerC", days(4))

    assertEquals(1d, matchProbAB, 0.00001)
    assertEquals(1d, matchProbBC, 0.00001)
  }

  @Test def getScoreEmissionProbabilities_AB_in_time_4_BC_in_time_6 {
    val matches = List(MatchOutcome("playerA", "playerB", true, days(4)), MatchOutcome("playerB", "playerC", false, days(6)))

    val tennisDBN = GenericTennisDBN(matches, priorProb, emissionProb, transitionProb, timeSliceInDays)
    tennisDBN.calibrate()

    val matchProbAB = tennisDBN.getMatchProbability("playerA", "playerB", days(4))
    val matchProbBC = tennisDBN.getMatchProbability("playerB", "playerC", days(6))

    assertEquals(1d, matchProbAB, 0.000001)
    assertEquals(0d, matchProbBC, 0.000001)
  }

  @Test def getPlayerAWinningProb {
    val matches = List(MatchOutcome("playerA", "playerB", true, days(8)), MatchOutcome("playerB", "playerC", false, days(9)), MatchOutcome("playerA", "playerC", None, days(10)))

    val tennisDBN = GenericTennisDBN(matches, priorProb, emissionProb, transitionProb, timeSliceInDays)
    tennisDBN.calibrate()

    val matchProbAC = tennisDBN.getMatchProbability("playerA", "playerC", days(10))
    assertEquals(0.4987, matchProbAC, 0.0001)
  }

  @Test def getPlayerAWinningProb_Federer_vs_Nadal {
    val matches = List(MatchOutcome("Roger Federer", "Rafael Nadal", true, hours(5)), MatchOutcome("Roger Federer", "Rafael Nadal", None, hours(6)))

    val tennisDBN = GenericTennisDBN(matches, priorProb, emissionProb, transitionProb, timeSliceInDays)
    tennisDBN.calibrate()

    val matchProb = tennisDBN.getMatchProbability("Roger Federer", "Rafael Nadal", hours(6))

    assertEquals(0.5320, matchProb, 0.0001)
  }

  @Test(expected = classOf[NoSuchElementException]) def getPlayerAWinningProb_no_result_variable_exists {
    val matches = List(MatchOutcome("playerA", "playerB", true, 8), MatchOutcome("playerB", "playerC", false, 9), MatchOutcome("playerA", "playerC", None, 10))

    val tennisDBN = GenericTennisDBN(matches, priorProb, emissionProb, transitionProb, timeSliceInDays)
    tennisDBN.calibrate()

    tennisDBN.getMatchProbability("playerA", "playerD", 8)
  }

  private def hours(num: Long): Long = num * (1000 * 60 * 60)

  private def days(num: Long): Long = num * (1000 * 60 * 60 * 24)
}