package dk.tennis.em.dbn.generic

import dk.tennis.em.EMTrain._
import scala.collection.mutable.ListBuffer
import scala.collection._
import dk.tennis.em.dbn.InferDbnTennis._
import dk.tennis.em.bn.Factor
import Factor._

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
class GenericDbnTennis(priorProb: Seq[Double], emissionProb: Seq[Double], transitionProb: Seq[Double]) extends DbnTennis {

  private val results: ListBuffer[Result] = ListBuffer()
  private val factors: ListBuffer[Factor] = ListBuffer()

  private val resultVariables: mutable.Map[Result, Var] = mutable.Map()

  /**All score variables must be uniquely identified. */
  private var scoreIndex = 0
  /**
   * Add tennis result between two tennis players to dynamic bayesian network.
   * @param result
   * @param timeSlice
   */
  def addResult(result: Result) {

    if (!results.isEmpty) {
      val maxTimeSlice = results.map(r => r.timeSlice).max
      require(result.timeSlice >= maxTimeSlice,
        "Adding result from the past is not permitted. Adding time slice: %d, the newest added time slice: %d.".format(result.timeSlice, maxTimeSlice))
    }

    if (!playerResultExists(result.playerA, result.timeSlice)) addPlayerFactor(result.playerA, result.timeSlice)
    if (!playerResultExists(result.playerB, result.timeSlice)) addPlayerFactor(result.playerB, result.timeSlice)

    addScoreFactor(result)

    results += result
  }

  /**
   * Gets all player ids in a bayesian network.
   *
   */
  def getPlayerIds(): List[String] = results.flatMap(r => r.playerA :: r.playerB :: Nil).distinct.toList

  /**Returns results.*/
  def getResults(): List[Result] = results.toList

  /**Returns underlying list of factors for dynamic bayesian network.*/
  def getFactors(): List[Factor] = factors.toList

  def getResultVariables(): immutable.Map[Result, Var] = resultVariables.toMap

  /**Add  player factor to factor list if not exist yet (either prior or transition factor)*/
  private def addPlayerFactor(playerName: String, timeSlice: Int) = {

    val prevTimeSlices = results.filter(r => r.timeSlice < timeSlice && (r.playerA.equals(playerName) || r.playerB.equals(playerName))).map(r => r.timeSlice).toList

    prevTimeSlices match {
      case Nil => addPlayerPriorFactor(playerName, timeSlice)
      case prevTimeSlices => addPlayerTransitionFactor(playerName, prevTimeSlices.max, timeSlice)
    }

  }

  private def addPlayerPriorFactor(playerName: String, timeSlice: Int) {

    val playerVar = createPlayerVariable(playerName, timeSlice)
    val factor = Factor(playerVar, priorProb: _*)
    factors += factor
  }

  private def addPlayerTransitionFactor(playerName: String, maxPrevTimeSlice: Int, timeSlice: Int) {

    for (i <- maxPrevTimeSlice until timeSlice) {
      val varCurr = createPlayerVariable(playerName, i)
      val varNext = createPlayerVariable(playerName, i + 1)
      val varValues = (1 to transitionProb.size).map(_.toString)
      val factor = Factor(varCurr, varNext, transitionProb: _*)
      factors += factor
    }
  }

  private def addScoreFactor(result: Result) = {

    val playerAVar = createPlayerVariable(result.playerA, result.timeSlice)
    val playerBVar = createPlayerVariable(result.playerB, result.timeSlice)

    val scoreVarName = "score_%s_%s_%d_%d".format(result.playerA, result.playerB, result.timeSlice, scoreIndex)
    scoreIndex += 1
    val scoreVar = Var(scoreVarName, ("w", "l"))

    val emissionFactor = Factor(
      playerAVar :: playerBVar :: scoreVar :: Nil,
      emissionProb)

    implicit def booleanToString(value: Boolean): String = if (value) "w" else "l"

    if (result.playerAWinner.isDefined) {
      val emissionFactorWithEvidence = emissionFactor.evidence((scoreVarName, booleanToString(result.playerAWinner.get)))
      factors += emissionFactorWithEvidence
    } else {
      factors += emissionFactor
    }

    resultVariables += result -> scoreVar

  }

  private def createPlayerVariable(playerName: String, timeSlice: Int): Var = {
    val priorFactorValues = (1 to priorProb.size).map(_.toString)
    Var("%s_rating_%d".format(playerName, timeSlice), priorFactorValues)
  }

  private def playerResultExists(playerName: String, timeSlice: Int): Boolean = results.find(r => r.timeSlice == timeSlice && (r.playerA.equals(playerName) || r.playerB.equals(playerName))).isDefined
}