package dk.tennis.em

import org.junit._
import Assert._
import dk.atp.api.CSVATPMatchesLoader
import dk.atp.api.domain.MatchComposite
import dk.atp.api.domain.SurfaceEnum._
import EMTrain.Params
import scala.util.Random
import org.joda.time.DateTime
import org.joda.time.Duration
import dk.tennis.em.dbn.factorgraph.DbnTennis.Result
import dk.tennis.em.dbn.infer.clusterloopybp.ClusterLoopyBPInferDbnTennisFactory

class GenericEMTrainRealTennisResultsTest {

  val ratingSize = 5
  
  val rand = new Random(System.currentTimeMillis())
  val priorProb = (1 to ratingSize).map(i => 1d / i)
  val normPriorProb = priorProb.map(v => v / priorProb.sum)

  val emissionProb = for (i <- 1 to ratingSize; j <- 1 to ratingSize) yield {
    val winProb = i.toDouble / (i + j)
    List(winProb, 1 - winProb)
  }

  val transitionProb = (1 to ratingSize).flatMap { i =>
    val values = for (j <- 1 to ratingSize) yield rand.nextDouble()
    val normValues = values.map(v => values.sum)
    normValues
  }

  val parameters = Params(normPriorProb, emissionProb.flatten, transitionProb)

 // val emTrain = new GenericEMTrain(GrmmInferDbnTennisFactory())
    val emTrain = new GenericEMTrain(ClusterLoopyBPInferDbnTennisFactory())

  private var llh: List[Double] = Nil
  private def progress(currentIter: Int, logLikelihood: Double) = {
    llh = logLikelihood :: llh;
    println("Log likelihood for iteration %d = %f".format(currentIter, logLikelihood))
  }

  val iterNum = 10

  @Test def emTrain_for_tennis_results_2010_and_2011 {

    val atpMatchesLoader = CSVATPMatchesLoader.fromCSVFile("./src/test/resources/match_data_2006_2011.csv")
    val matches: Seq[MatchComposite] = (2006 to 2011).flatMap(year => atpMatchesLoader.loadMatches(year))
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

    val firstMatchTime = shuffledMatches.head.tournament.tournamentTime.getTime()
    val results = for (m <- shuffledMatches) yield {
      toResult(firstMatchTime, m)
    }
    println("Results size: " + results.size)
    val trainedParams = emTrain.train(parameters, results, iterNum, progress)

    println(trainedParams.priorProb.map(e => e.formatted("%.4f")).toList)
    println(trainedParams.emissionProb.map(e => e.formatted("%.4f")).toList)
    println(trainedParams.transitionProb.map(e => e.formatted("%.4f")).toList)
  }

  private def toResult(firstMatchTime: Long, m: MatchComposite): Result = {

    val timeDate = new DateTime(m.tournament.tournamentTime)
    val durationSinceFirstMatch = new Duration(timeDate.getMillis() - firstMatchTime).getStandardDays() / 30000
    val timeSlice = durationSinceFirstMatch.toInt

    val playerAName = m.matchFacts.playerAFacts.playerName
    val playerBName = m.matchFacts.playerBFacts.playerName
    val playerAWinner = m.matchFacts.winner.equals(m.matchFacts.playerAFacts.playerName)

    Result(playerAName, playerBName, playerAWinner, timeSlice)
  }
}