package dk.tennis.dbn.em

import org.junit._
import Assert._
import dk.atp.api.CSVATPMatchesLoader
import dk.atp.api.domain.MatchComposite
import dk.atp.api.domain.SurfaceEnum._
import scala.util.Random
import org.joda.time.DateTime
import org.joda.time.Duration
import dk.tennis.dbn.MatchOutcome
import TennisEM._
import dk.tennis.dbn._
import EMTestUtil._

class GenericTennisEMRealResultsTest {

  val ratingSize = 10

  val priorProb = getPriorProb(ratingSize)
  val emissionProb = getEmissionProb(ratingSize)
  val transitionProb = getTransitionProb(ratingSize)

  private var llh: List[Double] = Nil
  private def progress(currentIter: Int, logLikelihood: Double) = {
    llh = logLikelihood :: llh;
    println("Log likelihood for iteration %d = %f".format(currentIter, logLikelihood))
  }

  val iterNum = 10

  @Test def emTrain_for_tennis_results_2010_and_2011 {

    val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile("./src/test/resources/match_data_2006_2011.csv")
    val matches: Seq[MatchComposite] = (2010 to 2011).flatMap(year => atpMatchesLoader.loadMatches(year))
    val filteredMatches = matches.filter(m => m.tournament.surface == HARD && m.tournament.numOfSet == 2)

    val rand = new Random(System.currentTimeMillis())
    val shuffledMatches = filteredMatches.map { m =>
      rand.nextBoolean() match {
        case true => {
          val newMatchFacts = m.matchFacts.copy(playerAFacts = m.matchFacts.playerBFacts, playerBFacts = m.matchFacts.playerAFacts)
          m.copy(matchFacts = newMatchFacts)
        }
        case false => m
      }
    }.toList.take(500)

    val matchOutcomes = shuffledMatches.map(m => toMatchOutcome(m))
    println("Matches size: " + matchOutcomes.size)

    val timeSliceInDays = 30
    val tennisClusterGraph = GenericTennisDBN(matchOutcomes, priorProb, emissionProb, transitionProb, timeSliceInDays).getTennisClusterGraph()

    val trainedParams = GenericTennisEM.train(tennisClusterGraph, iterNum, progress)

    println(trainedParams.priorProb.map(e => e.formatted("%.4f")).toList)
    println(trainedParams.emissionProb.map(e => e.formatted("%.4f")).toList)
    println(trainedParams.transitionProb.map(e => e.formatted("%.4f")).toList)
  }

  private def toMatchOutcome(m: MatchComposite): MatchOutcome = {

    val playerAName = m.matchFacts.playerAFacts.playerName
    val playerBName = m.matchFacts.playerBFacts.playerName
    val playerAWinner = m.matchFacts.winner.equals(m.matchFacts.playerAFacts.playerName)

    MatchOutcome(playerAName, playerBName, playerAWinner, m.tournament.tournamentTime.getTime)
  }
}