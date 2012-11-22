package dk.tennis.dbn.em

import TennisEM._
import scala.annotation.tailrec
import dk.bayes.factor.FactorUtil
import dk.bayes.em.GenericEMLearn
import dk.bayes.em.DataSet
import dk.bayes.clustergraph.ClusterGraph
import dk.bayes.em.EMLearn._
import dk.tennis.dbn.MatchOutcome
import dk.tennis.dbn._
import dk.bayes.clustergraph.Cluster
import clustergraph.TennisClusterGraph

/**
 * Default implementation of TennisEM.
 *
 * @author Daniel Korzekwa
 */
object GenericTennisEM extends TennisEM {

  def train(tennisClusterGraph: TennisClusterGraph, iterNum: Int, progress: (Int, Double) => Unit): Params = {

    val dataSet = toDataSet(tennisClusterGraph)
   
    def emLearnProgress(emProgress: Progress) = progress(emProgress.iterNum, emProgress.logLikelihood)

    GenericEMLearn.learn(tennisClusterGraph.getClusterGraph(), dataSet, iterNum, emLearnProgress)

    val priorParameter = findCluster(tennisClusterGraph.getClusterGraph(), tennisClusterGraph.getPriorParameter().size).get.getFactor().getValues()
    val emissionParameter = findCluster(tennisClusterGraph.getClusterGraph(), tennisClusterGraph.getEmissionParameter().size).get.getFactor().getValues()

    val transitionCluster = findCluster(tennisClusterGraph.getClusterGraph(), tennisClusterGraph.getTransitionParameter().size)
    val transitionParameter = if (transitionCluster.isDefined) transitionCluster.get.getFactor().getValues() else Array[Double]()

    Params(priorParameter, emissionParameter, transitionParameter)
  }

  private def findCluster(clusterGraph: ClusterGraph, parameterSize: Int): Option[Cluster] = {
    val cluster = clusterGraph.getClusters().find(c => c.getFactor().getValues().size == parameterSize)
    cluster
  }

  private def toDataSet(tennisClusterGraph: TennisClusterGraph): DataSet = {
    val variableIds = tennisClusterGraph.getClusterGraph().getVariables().map(v => v.id)
    val evidenceMap = tennisClusterGraph.getEvidenceMap()

    val samples = variableIds.map(varId => evidenceMap.getOrElse(varId, -1))

    val dataSet = DataSet(variableIds.toArray, Array(samples.toArray))
    dataSet
  }
}