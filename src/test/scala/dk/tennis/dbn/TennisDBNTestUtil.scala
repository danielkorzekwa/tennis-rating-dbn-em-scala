package dk.tennis.dbn
import dk.atp.api.domain.MatchComposite

object TennisDBNTestUtil {

  def toMatchOutcome(m: MatchComposite): MatchOutcome = {

    val playerAName = m.matchFacts.playerAFacts.playerName
    val playerBName = m.matchFacts.playerBFacts.playerName
    val playerAWinner = m.matchFacts.winner.equals(m.matchFacts.playerAFacts.playerName)

    MatchOutcome(playerAName, playerBName, playerAWinner, m.tournament.tournamentTime.getTime)
  }
}