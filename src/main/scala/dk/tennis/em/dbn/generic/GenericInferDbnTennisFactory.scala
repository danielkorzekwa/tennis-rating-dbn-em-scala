package dk.tennis.em.dbn.generic

import dk.tennis.em.dbn._
import dk.tennis.em.dbn.InferDbnTennis._
import dk.tennis.em.dbn.InferDbnTennisFactory

case class GenericInferDbnTennisFactory extends InferDbnTennisFactory {

  /**@see InferDbnTennisFactory*/
  def create(results: Seq[Result], priorProb: Seq[Double], emissionProb: Seq[Double], transitionProb: Seq[Double]): InferDbnTennis = {
    val dbnTennis = new GenericDbnTennis(priorProb, emissionProb, transitionProb)
    results.foreach(r => dbnTennis.addResult(r))
    val inferDbnTennis = GenericInferDbnTennis(dbnTennis.getFactors())
    inferDbnTennis
  }

}