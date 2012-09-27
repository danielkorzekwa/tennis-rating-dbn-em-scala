package dk.tennis.em.dbn.infer.grmm

import edu.umass.cs.mallet.grmm.types.FactorGraph
import edu.umass.cs.mallet.grmm.inference.JunctionTreeInferencer
import scala.collection.JavaConversions._
import edu.umass.cs.mallet.grmm.types.TableFactor
import scala.Math._
import edu.umass.cs.mallet.grmm.types.Variable
import edu.umass.cs.mallet.grmm.types.Assignment
import edu.umass.cs.mallet.grmm.inference.JunctionTree
import edu.umass.cs.mallet.grmm.types.Factor
import edu.umass.cs.mallet.grmm.inference.JunctionTreePropagation
import edu.umass.cs.mallet.grmm.inference.TreeBP
import edu.umass.cs.mallet.grmm.inference.BruteForceInferencer
import edu.umass.cs.mallet.grmm.types.AbstractTableFactor
import edu.umass.cs.mallet.grmm.inference.LoopyBP
import dk.tennis.em.dbn.infer._
import dk.tennis.em.dbn.factorgraph.DbnTennis.Result

/**
 * Inference engine based on Mallet GRMM toolkit (http://mallet.cs.umass.edu/grmm/index.php)
 *
 * @author korzekwad
 *
 * @param factorGraph Factor graph with evidence applied
 * @param originalFactorGraph Factor graph without evidence applied
 * @resultVariables Evidence representing results for tennis matches
 * @playerVariables Mapping between timeSlice, playerName and Variable Map[time slice, Map[playerName, playerVariable]]
 *
 */
case class GrmmInferDbnTennis(factorGraph: FactorGraph, originalFactorGraph: FactorGraph,
  resultVariables: Seq[Tuple2[Result, Variable]], playerVariables: Map[Int, Map[String, Variable]]) extends InferDbnTennis {

  private val inferencer = new LoopyBP()

  def computeMarginals() { inferencer.computeMarginals(factorGraph) }

  /**@see InferDbnTennis.*/
  def getRatingPriorProbabilities(): Seq[Seq[Double]] = marginalizeFactors(1)

  /**@see InferDbnTennis.*/
  def getScoreEmissionProbabilities(): Seq[Seq[Double]] = marginalizeFactors(3)

  /**@see InferDbnTennis.*/
  def getRatingTransitionProbabilities(): Seq[Seq[Double]] = marginalizeFactors(2)

  /**@see InferDbnTennis.*/
  def logLikelihood(): Double = {
    val variables = factorGraph.variablesSet().map(v => v.asInstanceOf[Variable]).toArray
    val assignment = new Assignment(variables, variables.map(v => 0))

    /**Set assignment for evidence variables*/
    resultVariables.filter { case (r, v) => r.playerAWinner.isDefined }.foreach {
      case (r, v) =>
        val evidenceAssignment = new Assignment(v, if (r.playerAWinner.get) 0 else 1)
        assignment.setValues(evidenceAssignment)
    }

    val posteriorLogLikelihood = inferencer.lookupLogJoint(assignment)
    val productLogLikelihood = originalFactorGraph.logProduct(assignment)
    val evidenceLogLikelihood = productLogLikelihood - posteriorLogLikelihood

    evidenceLogLikelihood
  }

  /**
   * Find all factors with a given number of variables and marginalize them from the factor graph.
   *
   * @return Probabilities for all marginalized factors
   */
  private def marginalizeFactors(factorVarNum: Int): Seq[Seq[Double]] = {

    val factors: Seq[AbstractTableFactor] = factorGraph.factors().map(f => f.asInstanceOf[AbstractTableFactor]).filter(f => f.varSet().size == factorVarNum).toSeq
    val marginalFactors = factors.map(f => inferencer.lookupMarginal(f.varSet()))
    val factorProbabilities = marginalFactors.map { f =>
      val probs = f.asInstanceOf[AbstractTableFactor].getValues().toSeq
      probs
    }

    factorProbabilities
  }

  /** @see InferDbnTennis*/
  def getPlayerAWinningProb(playerA: String, playerB: String, t: Int): Double = {
    val (result, variable) = resultVariables.find { case (r, v) => r.playerA.equals(playerA) && r.playerB.equals(playerB) && r.timeSlice == t && r.playerAWinner.isEmpty}.get
    val marginalFactor = inferencer.lookupMarginal(variable).asInstanceOf[AbstractTableFactor]
    marginalFactor.value(0)
  }

  /** @see InferDbnTennis*/
  def getPlayerRating(playerName: String, timeSlice: Int): Seq[Double] = {
    val playerVariable = playerVariables(timeSlice)(playerName)
    val playerMarginal = inferencer.lookupMarginal(playerVariable).asInstanceOf[AbstractTableFactor]
    val ratingProbabilities = playerMarginal.getValues()
    ratingProbabilities
  }
}