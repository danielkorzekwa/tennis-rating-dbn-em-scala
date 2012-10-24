package dk.tennis.em.dbn.infer.grmm

import org.junit._
import Assert._
import dk.tennis.em.dbn.infer.InferDbnTennis
import dk.tennis.em.util.AssertUtil._
import dk.atp.api.CSVATPMatchesLoader
import dk.atp.api.domain.MatchComposite
import dk.atp.api.domain.SurfaceEnum._
import org.joda.time.DateTime
import org.joda.time.Duration
import dk.tennis.em.dbn.factorgraph.DbnTennis.Result

class GrmmInferDbnTennisFactoryRealTennisResultsTest {

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
    val filteredMatches = matches.filter(m => m.tournament.surface == HARD && m.tournament.numOfSet == 2).take(2000)

    val firstMatchTime = filteredMatches.head.tournament.tournamentTime.getTime()
    val results = for (m <- filteredMatches) yield toResult(firstMatchTime, m)

    val inferDbnTennisdef = GrmmInferDbnTennisFactory().create(results, priorProb, emissionProb, transitionProb)

   // assertEquals(149, inferDbnTennisdef.getRatingPriorProbabilities().size)
   // assertEquals(200, inferDbnTennisdef.getScoreEmissionProbabilities().size)
   // assertEquals(163, inferDbnTennisdef.getRatingTransitionProbabilities().size)

    println(inferDbnTennisdef.getRatingPriorProbabilities()(76).map(e => e.formatted("%.4f")).toList)
    println(inferDbnTennisdef.getScoreEmissionProbabilities()(120).map(e => e.formatted("%.4f")).toList)
    println(inferDbnTennisdef.getRatingTransitionProbabilities()(140).map(e => e.formatted("%.4f")).toList)

//    assertEquals(-138.107, inferDbnTennisdef.logLikelihood(), 0.001)
//
//    assertVector(List(0.1197, 0.5317, 0.3486), inferDbnTennisdef.getRatingPriorProbabilities()(76), 0.0001)
//    assertVector(List(0.0441, 0.0000, 0.0549, 0.0000, 0.0200, 0.0000, 0.1802, 0.0000, 0.2521, 0.0000, 0.0982, 0.0000, 0.1131, 0.0000, 0.1688, 0.0000, 0.0685, 0.0000), inferDbnTennisdef.getScoreEmissionProbabilities()(120), 0.0001)
//    assertVector(List(0.3306, 0.0026, 0.0021, 0.0061, 0.4630, 0.0038, 0.0030, 0.0047, 0.1840), inferDbnTennisdef.getRatingTransitionProbabilities()(140), 0.0001)
  }

  private def toResult(firstMatchTime: Long, m: MatchComposite): Result = {

    val timeDate = new DateTime(m.tournament.tournamentTime)
    val durationSinceFirstMatch = new Duration(timeDate.getMillis() - firstMatchTime).getStandardDays() / 7
    val timeSlice = durationSinceFirstMatch.toInt

    val playerAName = m.matchFacts.playerAFacts.playerName
    val playerBName = m.matchFacts.playerBFacts.playerName
    val playerAWinner = m.matchFacts.winner.equals(m.matchFacts.playerAFacts.playerName)

    Result(playerAName, playerBName, playerAWinner, timeSlice)
  }

}