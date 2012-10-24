package dk.tennis.em.loopybp

import org.junit._
import dk.tennis.em.loopybp._
import Assert._
import dk.tennis.em.bn.Factor
import dk.tennis.em.bn.Factor._
import ClusterGraph._
import GenericClusterGraph._
import dk.tennis.em.util.AssertUtil._
class GenericClusterGraphTest {

  def progress(iterNum: Int): Unit = println("Iter: " + iterNum)

  val var1 = Var(1, 2)
  val var2 = Var(2, 2)
  val var3 = Var(3, 2)

  /**This is a product of two factors: var1:[0.3,0.7], var2|var1:[0.2,0.8,0.1,0.9]*/
  val cluster1 = Cluster(1, Factor(Array(var1, var2), Array(0.06, 0.24, 0.07, 0.63)))
  /**Factor: var3|var2*/
  val cluster2 = Cluster(2, Factor(Array(var2, var3), Array(0.7, 0.3, 0.6, 0.4)))

  val edges = List((1, 2))

  val clusterGraph = GenericClusterGraph(List(cluster1, cluster2), edges)
  val calibratedClusterGraph = clusterGraph.calibrate(progress)

  /**
   * Tests for clusteBelief
   */
  @Test def clusterBelief_two_clusters {

    assertVector(List(0.06, 0.24, 0.07, 0.63), calibratedClusterGraph.clusterBelief(1).values, 0.0001)
    assertVector(List(0.091, 0.03899, 0.522, 0.348), calibratedClusterGraph.clusterBelief(2).values, 0.0001)
  }

  /**
   * Tests for logLikelihood
   *
   */

  @Test(expected = classOf[IllegalArgumentException]) def logLikelihood_empty_assignment {
    calibratedClusterGraph.logLikelihood(Nil)
  }

  @Test(expected = classOf[IllegalArgumentException]) def logLikelihood_partial_assignment {

    val assignment = List(Assignment(1, 0), Assignment(2, 0))

    calibratedClusterGraph.logLikelihood(assignment)
  }

  @Test(expected = classOf[IllegalArgumentException]) def logLikelihood_assignment_not_unique {

    val assignment = List(Assignment(1, 0), Assignment(2, 0), Assignment(1, 0), Assignment(3, 0))

    calibratedClusterGraph.logLikelihood(assignment)
  }

  @Test def logLikelihood {
    val assignment = List(Assignment(1, 0), Assignment(2, 1), Assignment(3, 0))

    val llh = calibratedClusterGraph.logLikelihood(assignment)

    assertEquals(-1.9379, llh, 0.0001)
  }

  /**
   * Tests for marginal
   */
  @Test(expected = classOf[NoSuchElementException]) def marginal_variable_not_found {
    calibratedClusterGraph.marginal(123)
  }

  @Test def marginal {
    assertFactor(Factor(Var(1, 2), Array(0.3, 0.7)), calibratedClusterGraph.marginal(var1.id))
    assertFactor(Factor(Var(2, 2), Array(0.13, 0.87)), calibratedClusterGraph.marginal(var2.id))
    assertFactor(Factor(Var(3, 2), Array(0.613, 0.387)), calibratedClusterGraph.marginal(var3.id))
  }

}