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

    val dbnTennisWithEvidence = toDBNTennis(results, priorProb.toArray, emissionProb.toArray, transitionProb.toArray)
    val dbnTennisWithoutEvidence = toDBNTennis(results.map(r => r.copy(playerAWinner = None)), priorProb.toArray, emissionProb.toArray, transitionProb.toArray)

    val clusters = dbnTennisWithEvidence.asInstanceOf[GenericDbnTennis2].clusters
    val clustersNoEvidence = dbnTennisWithoutEvidence.asInstanceOf[GenericDbnTennis2].clusters

    val edges = dbnTennisWithoutEvidence.asInstanceOf[GenericDbnTennis2].edges.toList

    val clusterGraph = GenericClusterGraph(clusters, edges)
    val calibratedClusterGraph = clusterGraph.calibrate(iter => { println("Loopy BP iter=" + iter) })

    val clusterGraphNoEvidence = GenericClusterGraph(clustersNoEvidence, edges)
    ClusterLoopyBPInferDbnTennis(calibratedClusterGraph, clusterGraphNoEvidence, dbnTennisWithEvidence.getResultVariables(), dbnTennisWithEvidence.getPlayerVariables())
  }

 

  private def toDBNTennis(results: Seq[Result], priorProb: Array[Double], emissionProb: Array[Double], transitionProb: Array[Double]): DbnTennis = {
    val dbnTennis = new GenericDbnTennis2(priorProb, emissionProb, transitionProb)
    results.foreach(r => dbnTennis.addResult(r))
    dbnTennis
  }

}