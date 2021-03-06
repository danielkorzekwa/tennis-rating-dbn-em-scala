package dk.tennis.dbn.util

import org.junit._
import Assert._
import dk.bayes.factor.Factor

object AssertUtil {

  def assertVector(expected: Seq[Double], actual: Seq[Double], delta: Double) {
    assertEquals("Wrong size of vector.".format(expected.size, actual.size), expected.size, actual.size)
    var i = 0
    for ((expected, actual) <- expected.zip(actual)) {
      assertEquals("Element: " + i, expected, actual, delta)
      i = i + 1
    }
  }
  
  def assertFactor(expected: Factor, actual: Factor, delta: Double = 0) {
    assertEquals(expected.getVariables().toList, actual.getVariables().toList)
    assertVector(expected.getValues().toList, actual.getValues().toList, delta)
  }
}