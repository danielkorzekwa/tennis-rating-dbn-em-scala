package dk.tennis.em.loopybp

import org.junit._
import dk.tennis.em.loopybp._
import Assert._
import dk.tennis.em.bn.Factor
import dk.tennis.em.bn.Factor._
import ClusterGraph._
import GenericClusterGraph._
import dk.tennis.em.util.VectorAssert._
class GenericClusterGraphTest {

  def progress(iterNum: Int): Unit = println("Iter: " + iterNum)

  @Test def two_clusters {
    val var1 = Var("1", ("T", "F"))
    val var2 = Var("2", ("T", "F"))
    val var3 = Var("3", ("T", "F"))

    /**This is a product of two factors: var1:[0.3,0.7], var2|var1:[0.2,0.8,0.1,0.9]*/
    val cluster1 = Cluster(1, Factor(List(var1, var2), List(0.06, 0.24, 0.07, 0.63)))
    /**Factor: var3|var2*/
    val cluster2 = Cluster(2, Factor(List(var2, var3), List(0.7, 0.3, 0.6, 0.4)))
    
    val edges = List((1, 2))
    
    val clusterGraph = GenericClusterGraph(List(cluster1, cluster2), edges)
    val calibratedClusterGraph = clusterGraph.calibrate(progress)

    vectorAssert(List(0.06, 0.24, 0.07, 0.63), calibratedClusterGraph.clusterBelief(1).values, 0.0001)
    vectorAssert(List(0.091, 0.03899, 0.522, 0.348), calibratedClusterGraph.clusterBelief(2).values, 0.0001)
  }
}