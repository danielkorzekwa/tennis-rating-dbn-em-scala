package dk.tennis.em.dbn.infer.clusterloopybp

import dk.tennis.em.dbn.infer.InferDbnTennis
import dk.tennis.em.loopybp.ClusterGraph
import dk.tennis.em.loopybp.ClusterGraph.Assignment
import dk.tennis.em.dbn.factorgraph.DbnTennis.Result
import dk.tennis.em.bn.Factor.Var

/**
 * Dbn Tennis inference based on Loopy Belief Propagation algorithm
 * presented during a Probabilistic Graphical Models course (https://www.coursera.org/course/pgm)
 *
 * As opposed to Belief Propagation algorithm described on wikipedia (http://en.wikipedia.org/wiki/Belief_propagation),
 * which uses factor graph structure, this version of Belief Propagation operates on a cluster graph.
 *
 * @param clusterGraph Cluster graph with evidence applied
 * @param originalClusterGraph Cluster graph without evidence applied
 * @resultVariables Evidence representing results for tennis matches
 * @playerVariables Mapping between timeSlice, playerName and Variable Map[time slice, Map[playerName, playerVariable]]
 *
 * @author korzekwad
 */
case class ClusterLoopyBPInferDbnTennis(clusterGraph: ClusterGraph, originalClusterGraph: ClusterGraph,
  resultVariables: Seq[Tuple2[Result, Var]], playerVariables: Map[Int, Map[String, Var]]) extends InferDbnTennis {

  def getRatingPriorProbabilities(): Seq[Seq[Double]] = getClusterBeliefs(1)

  def getScoreEmissionProbabilities(): Seq[Seq[Double]] = getClusterBeliefs(3)

  def getRatingTransitionProbabilities(): Seq[Seq[Double]] = getClusterBeliefs(2)

  private def getClusterBeliefs(varNum: Int): Seq[Seq[Double]] = {
    val clusters = clusterGraph.getClusters().filter(c => c.factor.variables.size == varNum)
    val beliefs = clusters.map(c => clusterGraph.clusterBelief(c.id).normalize().values)
    beliefs
  }

  def logLikelihood(): Double = {

    val allVariables = clusterGraph.getClusters().flatMap(c => c.factor.variables).distinct

    val assignment = allVariables.map(v => Assignment(v.name, v.values.head))

    val varToResultMap = Map(resultVariables.map { case (r, v) => (v.name, r) }: _*)
    val resultToVarMap = Map(resultVariables.map { case (r, v) => (r, v) }: _*)

    val assignmentWithEvidence = assignment.map { a =>
      val result = varToResultMap.get(a.variableName)
      result match {
        case Some(result) => {
          if (result.playerAWinner.isDefined) {
            val variable = resultToVarMap(result)
            if (result.playerAWinner.get) Assignment(a.variableName, variable.values.head) else Assignment(a.variableName, variable.values.last)
          } else a
        }
        case None => a
      }
    }

    val posteriorLogLikelihood = clusterGraph.logLikelihood(assignmentWithEvidence)
    val productLogLikelihood = originalClusterGraph.logLikelihood(assignmentWithEvidence)
    val evidenceLogLikelihood = productLogLikelihood - posteriorLogLikelihood

    evidenceLogLikelihood
  }

  def getPlayerAWinningProb(playerA: String, playerB: String, t: Int): Double = {
    val (result, variable) = resultVariables.find { case (r, v) => r.playerA.equals(playerA) && r.playerB.equals(playerB) && r.timeSlice == t && r.playerAWinner.isEmpty }.get
    val marginalFactor = clusterGraph.marginal(variable.name)
    marginalFactor.values(0)
  }

  def getPlayerRating(playerName: String, timeSlice: Int): Seq[Double] = {
     val playerVariable = playerVariables(timeSlice)(playerName)
    val playerMarginal = clusterGraph.marginal(playerVariable.name)
    val ratingProbabilities = playerMarginal.values
    ratingProbabilities
  }

}