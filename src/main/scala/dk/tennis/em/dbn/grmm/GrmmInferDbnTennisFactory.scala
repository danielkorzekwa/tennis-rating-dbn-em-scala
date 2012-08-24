package dk.tennis.em.dbn.grmm

import dk.tennis.em.dbn._
import dk.tennis.em.dbn.InferDbnTennis._
import dk.tennis.em.dbn.InferDbnTennisFactory

class GrmmInferDbnTennisFactory extends InferDbnTennisFactory {

  /**@see InferDbnTennisFactory*/
  def create(result: Seq[Result], priorProb: Seq[Double], emissionProb: Seq[Double], transitionProb: Seq[Double]): InferDbnTennis =
    throw new UnsupportedOperationException("Not implemented yet.")
}