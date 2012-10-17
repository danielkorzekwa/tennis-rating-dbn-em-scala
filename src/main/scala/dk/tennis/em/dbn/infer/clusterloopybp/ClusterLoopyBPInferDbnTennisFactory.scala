package dk.tennis.em.dbn.infer.clusterloopybp
import dk.tennis.em.dbn.infer.InferDbnTennisFactory
import dk.tennis.em.dbn.infer.InferDbnTennis
import dk.tennis.em.dbn.factorgraph.DbnTennis.Result
import dk.tennis.em.loopybp._
import dk.tennis.em.dbn.factorgraph._
import dk.tennis.em.dbn.factorgraph.DbnTennis
import ClusterGraph._

/**
 * Factory which creates ClusterLoopyBPInferDbnTennis object.
 *
 * @author korzekwad
 */
case class ClusterLoopyBPInferDbnTennisFactory extends InferDbnTennisFactory {

  /**@see InferDbnTennisFactory*/
  def create(results: Seq[Result], priorProb: Seq[Double],
    emissionProb: Seq[Double], transitionProb: Seq[Double]): InferDbnTennis = {

    require(results.size > 0, "Results can't be empty")

    val dbnTennisWithEvidence = toDBNTennis(results, priorProb, emissionProb, transitionProb)
    val dbnTennisWithoutEvidence = toDBNTennis(results.map(r => r.copy(playerAWinner = None)), priorProb, emissionProb, transitionProb)

    val clusters = dbnTennisWithEvidence.getFactors.zipWithIndex.map { case (f, index) => Cluster(index, f) }
    val clustersNoEvidence = dbnTennisWithoutEvidence.getFactors.zipWithIndex.map { case (f, index) => Cluster(index, f) }

    val edges = calcEdges(clusters)

    val clusterGraph = GenericClusterGraph(clusters, edges)
    val calibratedClusterGraph = clusterGraph.calibrate(iter => { println("Loopy BP iter=" + iter) })

    val clusterGraphNoEvidence = GenericClusterGraph(clustersNoEvidence, edges)
    ClusterLoopyBPInferDbnTennis(calibratedClusterGraph, clusterGraphNoEvidence, dbnTennisWithEvidence.getResultVariables(), dbnTennisWithEvidence.getPlayerVariables())
  }

  private def calcEdges(clusters: Seq[Cluster]) = {
    val scoreEdges = clusters.filter(cluster => cluster.factor.variables.size == 3).flatMap { scoreCluster =>
      val playerClusters = clusters.filter(c => c.factor.variables.size < 3 && scoreCluster.factor.variables.map(_.name).contains(c.factor.variables.last.name))

      playerClusters.map(playerCluster => (playerCluster.id, scoreCluster.id))
    }

    val transitionEdges: Seq[Tuple2[Int, Int]] = clusters.filter(cluster => cluster.factor.variables.size == 2).map { transitionCluster =>

      val prevCluster = clusters.find(c => c.factor.variables.size == 2 && c.factor.variables.last.name == transitionCluster.factor.variables.head.name)
      val edge = prevCluster match {
        case Some(prevCluster) => (prevCluster.id, transitionCluster.id)
        case None => {
          val priorCluster = clusters.find(c => c.factor.variables.size == 1 && c.factor.variables.last.name == transitionCluster.factor.variables.head.name).get
          (priorCluster.id, transitionCluster.id)
        }
      }
      edge
    }

    scoreEdges.toList ::: transitionEdges.toList
  }

  private def toDBNTennis(results: Seq[Result], priorProb: Seq[Double], emissionProb: Seq[Double], transitionProb: Seq[Double]): DbnTennis = {
    val dbnTennis = new GenericDbnTennis(priorProb, emissionProb, transitionProb)
    results.foreach(r => dbnTennis.addResult(r))
    dbnTennis
  }

}