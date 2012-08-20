package dk.tennis.em

import EMTrain._
import scala.annotation.tailrec
import dbn._
import dbn.DbnTennis._
import dk.tennis.em.dbn.GenericInferDbnTennis
import GenericEMTrain._

/**
 * @see EMTrain
 */

object GenericEMTrain {

  /**Adds up corresponding elements in two sequences (Adding up two vectors).*/
  implicit def toRichSeq(orig: Seq[Double]) = new {
    def +(seq: Seq[Double]): Seq[Double] = orig.zip(seq).map(e => e._1 + e._2)
  }

  /**Adding up corresponding elements in multiple sequences.*/
  private def sum(vectors: Seq[Seq[Double]]): Seq[Double] = {
    vectors match {
      case Nil => Nil
      case x :: xs => vectors.reduceLeft { (reduced, rating) => (reduced + rating) }
    }

  }
}
class GenericEMTrain extends EMTrain {

  /** @see EMTrain */
  def train(parameters: Params, results: List[Result], iterNum: Int, progress: (Int, Double) => Unit): Params = {

    @tailrec
    def trainIteration(parameters: Params, iter: Int,prevLlh:Double=Double.MinValue): Params = {

      val sufficientStats = expectationStep(parameters, results)
      val newParameters = maximizationStep(sufficientStats)
      val llh =  sufficientStats.loglikelihood

      progress(iter,llh)
      
      /**For a better stopping criteria, please look here: em_converged.m (BNT tool) or Numerical Recipes in C p423
       * (http://astronu.jinr.ru/wiki/upload/d/d6/NumericalRecipesinC.pdf)
       */
      if (iter < iterNum && llh> prevLlh) trainIteration(newParameters, iter + 1,llh) else newParameters
    }

    val newParameters = trainIteration(parameters, 1)
    newParameters
  }

  /** @see EMTrain */
  def expectationStep(parameters: Params, results: List[Result]): SufficientStats = {

    val dbnTennis = new GenericDbnTennis(parameters.priorProb, parameters.emissionProb, parameters.transitionProb)
    results.foreach(r => dbnTennis.addResult(r))

    val inferDbnTennis = GenericInferDbnTennis(dbnTennis.getFactors())

    val priorRatingProbs = inferDbnTennis.getRatingPriorProbabilities()
    val scoreEmissionProbs = inferDbnTennis.getScoreEmissionProbabilities()
    val ratingTransitionProbs = inferDbnTennis.getRatingTransitionProbabilities()

    val priorStats = sum(priorRatingProbs)
    val emissionStats = sum(scoreEmissionProbs)
    val transitionStats = sum(ratingTransitionProbs)

    SufficientStats(priorStats, priorRatingProbs.size, 
    		emissionStats, scoreEmissionProbs.size, 
    		transitionStats, ratingTransitionProbs.size, 
    		inferDbnTennis.logLikelihood)
  }

  /** @see EMTrain */
  def maximizationStep(sufficientStats: SufficientStats): Params = {
    val priorProbs = toCPD(sufficientStats.priorStats, sufficientStats.priorStats.size)
    val emissionProbs = toCPD(sufficientStats.emissionStats, 2) //2 - number of tennis match outcomes (WIN,LOSE))
    val transitionProbs = toCPD(sufficientStats.transitionStats, sufficientStats.priorStats.size)
    Params(priorProbs, emissionProbs, transitionProbs)
  }

  /**Normalize all factor values so they form a conditional probability table.*/
  def toCPD(values: Seq[Double], sliceSize: Int): Seq[Double] = {

    val normValues = values.isEmpty match {
      case true => Nil
      case false => values.grouped(sliceSize).flatMap { slice =>
        slice.map(elem => elem / slice.sum)
      }
    }

    normValues.toSeq
  }

}