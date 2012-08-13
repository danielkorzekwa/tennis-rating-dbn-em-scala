package dk.tennis.em.util

import org.junit._
import Assert._

object VectorAssert {

  def vectorAssert(expected: Seq[Double], actual: Seq[Double], delta: Double) {
    assertEquals(expected.size, actual.size)
    for ((expected, actual) <- expected.zip(actual)) {
      assertEquals(expected, actual, delta)
    }
  }
}