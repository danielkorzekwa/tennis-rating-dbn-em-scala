package dk.tennis.em.dbn.grmm
import dk.tennis.em.dbn.InferDbnTennis
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
import edu.umass.cs.mallet.grmm.inference.JunctionTreeUnnormalizedPropagation
import JunctionTreeUnnormalizedPropagation._

/**
 * Inference engine based on Mallet GRMM toolkit (http://mallet.cs.umass.edu/grmm/index.php)
 *
 * @author korzekwad
 *
 */
case class GrmmInferDbnTennis(factorGraph: FactorGraph) extends InferDbnTennis {

  private val junctionTreePropagation = new JunctionTreeUnnormalizedPropagation(new SumProductMessageStrategy())
  private val inferencer = new JunctionTreeInferencer(junctionTreePropagation)
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
    val factorProbabilities = marginalFactors.map { f =>
      val probs = f.asInstanceOf[TableFactor].getValues().toSeq
      val normalizationConstant = probs.sum
      val normalizedProbs = probs.map(p => p / normalizationConstant)
      normalizedProbs
    }

    factorProbabilities
  }

  /**@see InferDbnTennis.*/
  def logLikelihood(): Double = {
    
    val marginal = inferencer.lookupMarginal(factorGraph.factors().toList(0).asInstanceOf[TableFactor].varSet()).asInstanceOf[TableFactor]
    val likelihood = marginal.getValues().sum
    log(likelihood)
  }
}