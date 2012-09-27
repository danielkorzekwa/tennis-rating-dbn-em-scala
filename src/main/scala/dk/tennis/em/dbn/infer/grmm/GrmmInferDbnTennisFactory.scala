package dk.tennis.em.dbn.infer.grmm

import dk.tennis.em.dbn._
import factorgraph.GenericDbnTennis
import edu.umass.cs.mallet.grmm.types.FactorGraph
import edu.umass.cs.mallet.grmm.types.Variable
import edu.umass.cs.mallet.grmm.types.TableFactor
import dk.tennis.em.bn.Factor
import scala.collection._
import edu.umass.cs.mallet.grmm.types.LogTableFactor
import edu.umass.cs.mallet.grmm.types.Assignment
import dk.tennis.em.bn.Factor._
import dk.tennis.em.dbn.factorgraph.DbnTennis
import infer._
import dk.tennis.em.dbn.factorgraph.DbnTennis.Result

case class GrmmInferDbnTennisFactory extends InferDbnTennisFactory {

  /**@see InferDbnTennisFactory*/
  def create(results: Seq[Result], priorProb: Seq[Double], emissionProb: Seq[Double], transitionProb: Seq[Double]): InferDbnTennis = {
    require(results.size > 0, "Results can't be empty")

    val dbnTennisWithEvidence = toDBNTennis(results, priorProb, emissionProb, transitionProb)
    val dbnTennisWithoutEvidence = toDBNTennis(results.map(r => r.copy(playerAWinner = None)), priorProb, emissionProb, transitionProb)

    /**Map[variableId,variable]*/
    val grmmVariablesMap: Map[String, Variable] = toGrmmVariables(dbnTennisWithEvidence.getFactors())

    val resultsVariables = dbnTennisWithEvidence.getResultVariables().map{case(result,variable) => (result,grmmVariablesMap(variable.name))}

    val playerVariables: immutable.Map[Int,immutable.Map[String,Variable]] = dbnTennisWithEvidence.getPlayerVariables().mapValues{
      timeSliceVariables =>
        timeSliceVariables.mapValues(variable => grmmVariablesMap(variable.name))
    }
    
    val inferDbnTennis = GrmmInferDbnTennis(
      toGrmmFactorGraph(dbnTennisWithEvidence.getFactors(), grmmVariablesMap),
      toGrmmFactorGraph(dbnTennisWithoutEvidence.getFactors(), grmmVariablesMap),
      resultsVariables,
      playerVariables)

    inferDbnTennis.computeMarginals()
    inferDbnTennis
  }

  private def toGrmmFactor(factor: Factor, grmmVariablesMap: Map[String, Variable]): LogTableFactor = {
    val variables = factor.variables.map(v => grmmVariablesMap(v.name)).toArray
    LogTableFactor.makeFromValues(variables, factor.values.toArray)
  }

  private def toGrmmVariables(factors: Seq[Factor]): Map[String, Variable] = Map(factors.flatMap(_.variables).map(v => v.name -> new Variable(v.values.size)): _*)

  private def toGrmmFactorGraph(factors: Seq[Factor], grmmVariablesMap: Map[String, Variable]): FactorGraph = {
    val factorGraph = new FactorGraph(grmmVariablesMap.values.toArray)
    factors.map(f => toGrmmFactor(f, grmmVariablesMap)).foreach(f => factorGraph.addFactor(f))
    factorGraph
  }

  private def toDBNTennis(results: Seq[Result], priorProb: Seq[Double], emissionProb: Seq[Double], transitionProb: Seq[Double]): DbnTennis = {
    val dbnTennis = new GenericDbnTennis(priorProb, emissionProb, transitionProb)
    results.foreach(r => dbnTennis.addResult(r))
    dbnTennis
  }
}