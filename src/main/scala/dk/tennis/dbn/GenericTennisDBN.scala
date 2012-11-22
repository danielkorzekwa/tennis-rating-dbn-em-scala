package dk.tennis.dbn

import scala.collection.mutable.ListBuffer
import scala.collection._
import dk.bayes.clustergraph._
import dk.bayes.factor.Factor._
import ClusterGraph._
import dk.bayes.factor._
import org.joda.time.DateTime
import org.joda.time.Duration
import dk.bayes.infer.LoopyBP
import dk.tennis.dbn.utils.TimeSliceCalc
import dk.tennis.dbn.utils.TimeSliceCalc
import dk.tennis.dbn.utils._
import dk.tennis.dbn.clustergraph.GenericTennisClusterGraph
import dk.tennis.dbn.clustergraph.TennisVar
import clustergraph.TennisClusterGraph

/**
 * @see DbnTennis
 * @author korzekwad
 */

/**
 * Input parameters example:
 * priorProb = 0.2 0.5 0.3 //three rating values: 0,1,2 with prior probabilities 0.2 0.5 0.3 respectively.
 *
 * Tennis match score (Emission probabilities, 18 values) specified in the following order:
 *
 * playerA_rating, playeB_rating, win/lose
 * 0,0,w - 0.5
 * 0,0,l - 0.5
 * 0,1,w - 1/3
 * 0,1,l - 2/3
 * 0,2,w - 0.25
 * 0,2,l - 0.75
 * 1,0,w - 2/3
 * 1,0,l - 1/3
 * ...
 *
 * Rating transition probabilities (9 values) specified in the following order:
 * player_old_rating,player_new_rating
 * 0,0 - 0.98
 * 0,1 - 0.01
 * 0,2 - 0.01
 * 1,0 - 0.01
 * 1,1 - 0.97
 * 1,2 - 0.02
 * ...
 *
 */
case class GenericTennisDBN(priorProb: Array[Double], emissionProb: Array[Double], transitionProb: Array[Double], timeSliceInDays: Int) extends TennisDBN {

  private val playerVariables = PlayerVars()
  private val matchVariables = MatchVars()

  private val tennisClusterGraph = GenericTennisClusterGraph(priorProb, emissionProb, transitionProb)
  private val loopyBP = LoopyBP(tennisClusterGraph.getClusterGraph())

  private var timeSliceCalc: Option[TimeSliceCalc] = None

  def getTennisClusterGraph(): TennisClusterGraph = tennisClusterGraph

  def addMatch(matchOutcome: MatchOutcome) {
    if (timeSliceCalc.isEmpty) timeSliceCalc = Some(TimeSliceCalc(matchOutcome.matchTime, timeSliceInDays: Int))

    val timeSlice = timeSliceCalc.get.toTimeSlice(matchOutcome.matchTime)

    val playerAVar = playerVariables.getPlayerVar(matchOutcome.playerA, timeSlice) match {
      case None => addPlayerFactor(matchOutcome.playerA, timeSlice)
      case Some(playerVar) => playerVar
    }
    val playerBVar = playerVariables.getPlayerVar(matchOutcome.playerB, timeSlice) match {
      case None => addPlayerFactor(matchOutcome.playerB, timeSlice)
      case Some(playerVar) => playerVar
    }

    val matchVar = tennisClusterGraph.addMatchCluster(playerAVar, playerBVar)
    matchVariables.add(matchOutcome, matchVar)
    if (matchOutcome.playerAWinner.isDefined) tennisClusterGraph.setEvidence(matchVar.id, matchOutcome.playerAWinner.get)

  }

  def updateMatch(outcome: MatchOutcome) {
    val matchVar = matchVariables.get(outcome)
    tennisClusterGraph.setEvidence(matchVar.id, outcome.playerAWinner.get)
  }

  private def addPlayerFactor(playerName: String, timeSlice: Int): TennisVar = {

    val prevTimeSlice = playerVariables.getLastPlayerTimeSlice(playerName)

    prevTimeSlice match {
      case None => {
        val playerVar = tennisClusterGraph.addPriorSkillCluster(timeSlice)
        playerVariables.addPlayerVariable(playerName, playerVar)
        playerVar
      }
      case Some(prevTimeSlice) => {
        val vars = tennisClusterGraph.addTransitionSkillCluster(playerVariables.getPlayerVar(playerName, prevTimeSlice).get, timeSlice)
        vars.foreach(v => playerVariables.addPlayerVariable(playerName, v))
        vars.last
      }
    }
  }

  def getMatchProbability(playerA: String, playerB: String, matchTime: Long): Double = {
    val variable = matchVariables.get(playerA, playerB, matchTime)
    val marginalFactor = loopyBP.marginal(variable.id)
    marginalFactor.getValues()(0)
  }

  def getPlayerRating(playerName: String, matchTime: Long): Array[Double] = {
    val timeSlice = timeSliceCalc.get.toTimeSlice(matchTime)
    val playerVariable = playerVariables.getPlayerVar(playerName, timeSlice).get
    val playerMarginal = loopyBP.marginal(playerVariable.id)
    val ratingProbabilities = playerMarginal.getValues()
    ratingProbabilities
  }

  def calibrate(iterNum: (Int) => Unit = (iterNum: Int) => {}) {
    tennisClusterGraph.getClusterGraph.getClusters().foreach(c => c.resetMessages())
    loopyBP.calibrate(iterNum)
  }
}

object GenericTennisDBN {
  def apply(matches: Seq[MatchOutcome], priorProb: Array[Double], emissionProb: Array[Double], transitionProb: Array[Double], timeSliceInDays: Int): TennisDBN = {
    val tennisDBN = GenericTennisDBN(priorProb, emissionProb, transitionProb, timeSliceInDays)
    matches.foreach(matchOutcome => tennisDBN.addMatch(matchOutcome))
    tennisDBN
  }
}