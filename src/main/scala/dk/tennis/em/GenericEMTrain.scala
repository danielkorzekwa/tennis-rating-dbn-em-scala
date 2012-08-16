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
    def trainIteration(parameters: Params, iter: Int): Params = {

      val sufficientStats = expectationStep(parameters, results)
      val newParameters = maximizationStep(sufficientStats)
      progress(iter, 0d)
      if (iter < iterNum) trainIteration(newParameters, iter + 1) else newParameters
    }

    val newParameters = trainIteration(parameters, 1)
    newParameters
  }

  /** @see EMTrain */
  def expectationStep(parameters: Params, results: List[Result]): SufficientStats = {

    val dbnTennis = new GenericDbnTennis(parameters.priorProb, parameters.emissionProb, parameters.transitionProb)
    results.foreach(r => dbnTennis.addResult(r))

    val priorRatingProbs = GenericInferDbnTennis.getRatingPriorProbabilities(dbnTennis.getFactors())
    val scoreEmissionProbs = GenericInferDbnTennis.getScoreEmissionProbabilities(dbnTennis.getFactors())
    val ratingTransitionProbs = GenericInferDbnTennis.getRatingTransitionProbabilities(dbnTennis.getFactors())

    val priorStats = sum(priorRatingProbs)
    val emissionStats = sum(scoreEmissionProbs)
    val transitionStats = sum(ratingTransitionProbs)

    SufficientStats(priorStats, priorRatingProbs.size, emissionStats, scoreEmissionProbs.size, transitionStats, ratingTransitionProbs.size)
  }

  /** @see EMTrain */
  def maximizationStep(sufficientStats: SufficientStats): Params = {
    val priorProbs = toCPD(sufficientStats.priorStats,3)
    val emissionProbs = toCPD(sufficientStats.emissionStats,2)
    val transitionProbs = toCPD(sufficientStats.transitionStats,3)
    Params(priorProbs, emissionProbs, transitionProbs)
  }

  /**Normalize all factor values so they form a conditional probability table.*/
  def toCPD(values:Seq[Double],sliceSize:Int): Seq[Double] = {

    val normValues = values.grouped(sliceSize).map { slice =>
      slice.map(elem => elem / slice.sum)
    }.flatten.toSeq

   normValues
  }
  
}