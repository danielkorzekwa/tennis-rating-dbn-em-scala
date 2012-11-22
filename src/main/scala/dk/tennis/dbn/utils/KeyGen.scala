package dk.tennis.dbn.utils

case class KeyGen {

  private var lastKey: Int = 0
  def newKey(): Int = {
    lastKey = lastKey + 1
    lastKey
  }
}