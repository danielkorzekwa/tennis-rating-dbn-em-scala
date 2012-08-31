package dk.tennis.em.dbn.grmm

import dk.tennis.em.dbn._
import dk.tennis.em.dbn.InferDbnTennis._
import dk.tennis.em.dbn.InferDbnTennisFactory
import generic.GenericDbnTennis
import edu.umass.cs.mallet.grmm.types.FactorGraph
import edu.umass.cs.mallet.grmm.types.Variable
import edu.umass.cs.mallet.grmm.types.TableFactor
import dk.tennis.em.bn.Factor
import scala.collection._
import edu.umass.cs.mallet.grmm.types.LogTableFactor

case class GrmmInferDbnTennisFactory extends InferDbnTennisFactory {

  /**@see InferDbnTennisFactory*/
  def create(results: Seq[Result], priorProb: Seq[Double], emissionProb: Seq[Double], transitionProb: Seq[Double]): InferDbnTennis = {
    require(results.size>0,"Results can't be empty")
    val factors = toFactors(results, priorProb, emissionProb, transitionProb)

    /**Map[variableId,variable]*/
    val grmmVariablesMap: Map[String, Variable] = Map(factors.flatMap(_.variables).map(v => v.name -> new Variable(v.values.size)): _*)

    val factorGraph = new FactorGraph(grmmVariablesMap.values.toArray)
    factors.map(f => toGrmmFactor(f, grmmVariablesMap)).foreach(f => factorGraph.addFactor(f))

    GrmmInferDbnTennis(factorGraph)
  }

  private def toFactors(results: Seq[Result], priorProb: Seq[Double], emissionProb: Seq[Double], transitionProb: Seq[Double]): Seq[Factor] = {
    val dbnTennis = new GenericDbnTennis(priorProb, emissionProb, transitionProb)
    results.foreach(r => dbnTennis.addResult(r))
    dbnTennis.getFactors()
  }

  private def toGrmmFactor(factor: Factor, grmmVariablesMap: Map[String, Variable]): LogTableFactor = {
    val variables = factor.variables.map(v => grmmVariablesMap(v.name)).toArray
     LogTableFactor.makeFromValues(variables, factor.values.toArray)
  }

}