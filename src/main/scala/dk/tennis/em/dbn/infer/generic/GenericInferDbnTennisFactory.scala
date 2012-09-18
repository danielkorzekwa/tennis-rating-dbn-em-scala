package dk.tennis.em.dbn.infer.generic

import dk.tennis.em.dbn._
import factorgraph.GenericDbnTennis
import infer._
import dk.tennis.em.dbn.factorgraph.DbnTennis.Result

case class GenericInferDbnTennisFactory extends InferDbnTennisFactory {

  /**@see InferDbnTennisFactory*/
  def create(results: Seq[Result], priorProb: Seq[Double], emissionProb: Seq[Double], transitionProb: Seq[Double]): InferDbnTennis = {
    val dbnTennis = new GenericDbnTennis(priorProb, emissionProb, transitionProb)
    results.foreach(r => dbnTennis.addResult(r))
    val inferDbnTennis = GenericInferDbnTennis(dbnTennis.getFactors())
    inferDbnTennis
  }

}