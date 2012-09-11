package dk.tennis.em.dbn.grmm

import org.junit._
import Assert._
import dk.tennis.em.dbn.InferDbnTennis
import InferDbnTennis.Result
import dk.tennis.em.util.VectorAssert._
import dk.atp.api.CSVATPMatchesLoader
import dk.atp.api.domain.MatchComposite
import dk.atp.api.domain.SurfaceEnum._
import org.joda.time.DateTime

class GrmmInferDbnTennisRealTennisResultsTest {

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

  /**
   * Tests for getPriorRating
   *
   */

  @Test def inference_real_tennis_results {

    val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile("./src/test/resources/match_data_2010_2011.csv")
    val matches: Seq[MatchComposite] = (2010 to 2010).flatMap(year => atpMatchesLoader.loadMatches(year))
    val filteredMatches = matches.filter(m => m.tournament.surface == HARD && m.tournament.numOfSet == 2)

    val results = for (m <- filteredMatches.take(200)) yield toResult(m)

    val inferDbnTennisdef = GrmmInferDbnTennisFactory().create(results, priorProb, emissionProb, transitionProb)

    assertEquals(149, inferDbnTennisdef.getRatingPriorProbabilities().size)
    assertEquals(200, inferDbnTennisdef.getScoreEmissionProbabilities().size)
    assertEquals(186, inferDbnTennisdef.getRatingTransitionProbabilities().size)

    println(inferDbnTennisdef.getRatingPriorProbabilities()(76).map(e => e.formatted("%.4f")).toList)
    println(inferDbnTennisdef.getScoreEmissionProbabilities()(120).map(e => e.formatted("%.4f")).toList)
    println(inferDbnTennisdef.getRatingTransitionProbabilities()(140).map(e => e.formatted("%.4f")).toList)

    assertEquals(-138.118, inferDbnTennisdef.logLikelihood(), 0.001)

    vectorAssert(List(0.1198, 0.5317, 0.3485), inferDbnTennisdef.getRatingPriorProbabilities()(76), 0.0001)
    vectorAssert(List(0.0442, 0.0000, 0.0549, 0.0000, 0.0201, 0.0000, 0.1802, 0.0000, 0.2521, 0.0000, 0.0982, 0.0000, 0.1131, 0.0000, 0.1688, 0.0000, 0.0685, 0.0000), inferDbnTennisdef.getScoreEmissionProbabilities()(120), 0.0001)
    vectorAssert(List(0.0544, 0.0012, 0.0016, 0.0023, 0.4668, 0.0067, 0.0016, 0.0067, 0.4587), inferDbnTennisdef.getRatingTransitionProbabilities()(140), 0.0001)
  }

  private def toResult(m: MatchComposite): Result = {

    val timeDate = new DateTime(m.tournament.tournamentTime)
    val timeSlice = if (timeDate.getWeekOfWeekyear() == 53) 0 else timeDate.getWeekOfWeekyear() //53 - last week of the previous year

    val playerAName = m.matchFacts.playerAFacts.playerName
    val playerBName = m.matchFacts.playerBFacts.playerName
    val playerAWinner = m.matchFacts.winner.equals(m.matchFacts.playerAFacts.playerName)

    Result(playerAName, playerBName, playerAWinner, timeSlice)
  }

}