package dk.tennis.dbn

import org.junit._
import Assert._
import dk.tennis.dbn.util.AssertUtil._
import dk.atp.api.CSVATPMatchesLoader
import dk.atp.api.domain.MatchComposite
import dk.atp.api.domain.SurfaceEnum._
import TennisDBNTestUtil._

class ClusterLoopyBPInferDbnTennisFactoryRealResultsTest2 {

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

  @Test def inference_real_tennis_results {

    val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile("./src/test/resources/match_data_2010_2011.csv")
    val matches: Seq[MatchComposite] = (2010 to 2011).flatMap(year => atpMatchesLoader.loadMatches(year))
    val filteredMatches = matches.filter(m => m.tournament.surface == HARD && m.tournament.numOfSet == 2).take(5000)

    val matchOutcomes = filteredMatches.map(m => toMatchOutcome(m))

    val timeSliceInDays = 7
    val tennisDBN = GenericTennisDBN(matchOutcomes, priorProb, emissionProb, transitionProb, timeSliceInDays)
    tennisDBN.calibrate(iter => println("LoopyBP iter=" + iter))

    val matchOutcome = matchOutcomes(500)
    val playerRating = tennisDBN.getPlayerRating(matchOutcome.playerA, matchOutcome.matchTime)
    val matchProb = tennisDBN.getMatchProbability(matchOutcome.playerA, matchOutcome.playerB, matchOutcome.matchTime)

    assertVector(List(0.2536, 0.5190, 0.2274), playerRating.toList, 0.0001)
    assertEquals(1d, matchProb, 0.0001)

  }

}