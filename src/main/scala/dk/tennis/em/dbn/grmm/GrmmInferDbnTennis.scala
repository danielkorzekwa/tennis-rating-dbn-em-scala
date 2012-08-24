package dk.tennis.em.dbn.grmm
import dk.tennis.em.dbn.InferDbnTennis

/**
 * Inference engine based on Mallet GRMM toolkit (http://mallet.cs.umass.edu/grmm/index.php)
 *
 * @author korzekwad
 *
 */
case class GrmmInferDbnTennis extends InferDbnTennis {

  /**@see InferDbnTennis.*/
  def getRatingPriorProbabilities(): Seq[Seq[Double]] = throw new UnsupportedOperationException("Not implemented yet")

  /**@see InferDbnTennis.*/
  def getScoreEmissionProbabilities(): Seq[Seq[Double]] = throw new UnsupportedOperationException("Not implemented yet")

  /**@see InferDbnTennis.*/
  def getRatingTransitionProbabilities(): Seq[Seq[Double]] = throw new UnsupportedOperationException("Not implemented yet")

  /**@see InferDbnTennis.*/
  def logLikelihood(): Double = throw new UnsupportedOperationException("Not implemented yet")
}