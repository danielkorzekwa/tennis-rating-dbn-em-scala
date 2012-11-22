package dk.tennis.dbn.utils

import scala.collection._
import dk.tennis.dbn.MatchOutcome
import dk.tennis.dbn.clustergraph.TennisVar

case class MatchVars {

  private val resultVariables: mutable.ListBuffer[Tuple2[MatchOutcome, TennisVar]] = mutable.ListBuffer()

  private var lastMatchTime = -1L

  def isEmpty() = resultVariables.isEmpty

  def add(matchOutcome: MatchOutcome, matchVar: TennisVar) {
    require(matchOutcome.matchTime >= lastMatchTime,
      "Adding result from the past is not permitted. Adding time : %d, the newest added time : %d.".format(matchOutcome.matchTime, lastMatchTime))

    resultVariables += matchOutcome -> matchVar
    lastMatchTime = matchOutcome.matchTime
  }

  def get(outcome: MatchOutcome): TennisVar =
    resultVariables.find {
      case (r, v) =>
        r.playerA.equals(outcome.playerA) && r.playerB.equals(outcome.playerB) &&
          r.matchTime == outcome.matchTime
    }.get._2

  def get(playerA: String, playerB: String, matchTime: Long): TennisVar =
    resultVariables.find {
      case (r, v) =>
        r.playerA.equals(playerA) && r.playerB.equals(playerB) && r.matchTime == matchTime
    }.get._2

}