package dk.tennis.em

import org.junit._
import Assert._
import dk.atp.api.CSVATPMatchesLoader
import dk.atp.api.domain.MatchComposite
import dk.atp.api.domain.SurfaceEnum._
import dbn.grmm.GrmmInferDbnTennisFactory
import EMTrain.Params
import dbn.InferDbnTennis._
import scala.util.Random
import org.joda.time.DateTime

class GenericEMTrainRealTennisResultsTest {

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

  val emTrain = new GenericEMTrain(GrmmInferDbnTennisFactory())

  private var llh: List[Double] = Nil
  private def progress(currentIter: Int, logLikelihood: Double) = {
    llh = logLikelihood :: llh;
    println("Log likelihood for iteration %d = %f".format(currentIter, logLikelihood))
  }

  val iterNum = 5

  @Test def emTrain_for_tennis_results_2010_and_2011 {

    val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile("./src/test/resources/match_data_2010_2011.csv")
    val matches: Seq[MatchComposite] = (2010 to 2010).flatMap(year => atpMatchesLoader.loadMatches(year))
    val filteredMatches = matches.filter(m => m.tournament.surface == HARD && m.tournament.numOfSet == 2)

    val rand = new Random(System.currentTimeMillis())
    val schuffledMatches = filteredMatches.map { m =>
      rand.nextBoolean match {
        case true => {
          val newMatchFacts = m.matchFacts.copy(playerAFacts = m.matchFacts.playerBFacts, playerBFacts = m.matchFacts.playerAFacts)
          m.copy(matchFacts = newMatchFacts)
        }
        case false => m
      }
    }

    val results = for (m <- schuffledMatches.take(500)) yield {
      toResult(m)
    }

    val trainedParams = emTrain.train(parameters, results, iterNum, progress)

    println(trainedParams.priorProb.map(e => e.formatted("%.4f")).toList)
    println(trainedParams.emissionProb.map(e => e.formatted("%.4f")).toList)
    println(trainedParams.transitionProb.map(e => e.formatted("%.4f")).toList)
  }

  private def toResult(m: MatchComposite): Result = {

    val timeDate = new DateTime(m.tournament.tournamentTime)
    val timeSlice =  if(timeDate.getWeekOfWeekyear()==53) 0 else timeDate.getWeekOfWeekyear() //53 - last week of the previous year
 
    val playerAName = m.matchFacts.playerAFacts.playerName
    val playerBName = m.matchFacts.playerBFacts.playerName
    val playerAWinner = m.matchFacts.winner.equals(m.matchFacts.playerAFacts.playerName)

    Result(playerAName, playerBName, playerAWinner, timeSlice)
  }
}