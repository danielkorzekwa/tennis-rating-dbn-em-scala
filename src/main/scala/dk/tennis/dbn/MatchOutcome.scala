package dk.tennis.dbn

case class MatchOutcome(playerA: String, playerB: String, playerAWinner: Option[Boolean], matchTime: Long)

object MatchOutcome{
  def apply(playerA: String, playerB: String, playerAWinner: Boolean, matchTime: Long): MatchOutcome =
    MatchOutcome(playerA, playerB, Option(playerAWinner), matchTime)
}