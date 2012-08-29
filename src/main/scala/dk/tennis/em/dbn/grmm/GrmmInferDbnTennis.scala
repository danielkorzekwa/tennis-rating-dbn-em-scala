package dk.tennis.em.dbn.grmm
import dk.tennis.em.dbn.InferDbnTennis
import edu.umass.cs.mallet.grmm.types.FactorGraph
import edu.umass.cs.mallet.grmm.inference.JunctionTreeInferencer
import scala.collection.JavaConversions._
import edu.umass.cs.mallet.grmm.types.TableFactor
import scala.Math._
import edu.umass.cs.mallet.grmm.types.Variable
import edu.umass.cs.mallet.grmm.types.Assignment

/**
 * Inference engine based on Mallet GRMM toolkit (http://mallet.cs.umass.edu/grmm/index.php)
 *
 * @author korzekwad
 *
 */
case class GrmmInferDbnTennis(factorGraph: FactorGraph) extends InferDbnTennis {

  private val inferencer = new JunctionTreeInferencer()
  inferencer.computeMarginals(factorGraph)

  /**@see InferDbnTennis.*/
  def getRatingPriorProbabilities(): Seq[Seq[Double]] = marginalizeFactors(1)

  /**@see InferDbnTennis.*/
  def getScoreEmissionProbabilities(): Seq[Seq[Double]] = marginalizeFactors(3)

  /**@see InferDbnTennis.*/
  def getRatingTransitionProbabilities(): Seq[Seq[Double]] = marginalizeFactors(2)

  /**
   * Find all factors with a given number of variables and marginalize them from the factor graph.
   *
   * @return Probabilities for all marginalized factors
   */
  private def marginalizeFactors(factorVarNum: Int): Seq[Seq[Double]] = {

    val factors: Seq[TableFactor] = factorGraph.factors().map(f => f.asInstanceOf[TableFactor]).filter(f => f.varSet().size == factorVarNum).toSeq
    val marginalFactors = factors.map(f => inferencer.lookupMarginal(f.varSet()))
    val factorProbabilities = marginalFactors.map(f => f.asInstanceOf[TableFactor].getValues().toSeq)

    factorProbabilities

  }

  /**@see InferDbnTennis.*/
  def logLikelihood(): Double = {

  //
   
0
  }
}