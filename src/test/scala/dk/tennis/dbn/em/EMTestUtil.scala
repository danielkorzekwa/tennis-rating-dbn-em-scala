package dk.tennis.dbn.em

object EMTestUtil {

  def getPriorProb(ratingSize: Int): Array[Double] = {
    val prob = (1 to ratingSize).map(i => 1d / i)
    val normProb = prob.map(v => v / prob.sum)
    normProb.toArray
  }

  def getEmissionProb(ratingSize: Int): Array[Double] = {
    val prob = for (i <- 1 to ratingSize; j <- 1 to ratingSize) yield {
      val winProb = i.toDouble / (i + j)
      List(winProb, 1 - winProb)
    }
    prob.flatten.toArray
  }

  def getTransitionProb(ratingSize: Int): Array[Double] = {
    val prob = (1 to ratingSize).flatMap { i =>
      val values = for (j <- 1 to ratingSize) yield (1 / (0.01 + Math.abs(i - j))).toDouble
      val normValues = values.map(v => v / values.sum)
      normValues
    }
    prob.toArray
  }
}