package edu.umass.cs.mallet.grmm.inference

import JunctionTreePropagation._
import edu.umass.cs.mallet.grmm.types.VarSet
import scala.collection.JavaConversions._
import edu.umass.cs.mallet.grmm.types.Factor
import java.util.Collection
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import edu.umass.cs.mallet.grmm.types.Variable
import edu.umass.cs.mallet.grmm.types.HashVarSet

/**
 * This is a modified version of edu.umass.cs.mallet.grmm.inference.JunctionTreePropagation with no normalization applied to factors in a factor graph.
 *  Having access to not normalized marginals allows for computing the log likelihood of evidence, which is set in a factor graph. Code example for calculating log likelihood of evidence:
 * 
 * In this example, we obtain marginal of the first factor in a factor graph and then we take a logarithm of sum of factor marginal values, 
 * but actually we could take any of factor graph's factors and compute the log likelihood of evidence.
 * 
 * val marginal = inferencer.lookupMarginal(factorGraph.factors().toList(0).asInstanceOf[TableFactor].varSet()).asInstanceOf[TableFactor]
 * val likelihood = marginal.getValues().sum
 * log(likelihood)
 * 
 * @author korzekwad
 */

object JunctionTreeUnnormalizedPropagation {

  class SumProductMessageStrategy extends MessageStrategy with Serializable {

    /**
     * This sends a sum-product message, normalized to avoid
     * underflow.
     */
    def sendMessage(jt: JunctionTree, from: VarSet, to: VarSet) {
      val sepset: Collection[_ <: Any] = jt.getSepset(from, to)
      val fromCpf: Factor = jt.getCPF(from)
      val toCpf: Factor = jt.getCPF(to)
      val oldSepsetPot: Factor = jt.getSepsetPot(from, to)
      val lambda: Factor = fromCpf.marginalize(sepset)

      //   lambda.normalize ();

      jt.setSepsetPot(lambda, from, to)
      toCpf.multiplyBy(lambda)
      toCpf.divideBy(oldSepsetPot)
      //   toCpf.normalize ();
    }

    def extractBelief(cpf: Factor, varSet: VarSet): Factor =
      {
        cpf.marginalize(varSet)
      }

    // Serialization
    private val serialVersionUID: Long = 1
    private val CUURENT_SERIAL_VERSION: Int = 1

    private def writeObject(out: ObjectOutputStream) {
      out.defaultWriteObject()
      out.writeInt(CUURENT_SERIAL_VERSION)
    }

    private def readObject(in: ObjectInputStream) {
      in.defaultReadObject()
      in.readInt() // version
    }
  }

}
class JunctionTreeUnnormalizedPropagation(strategy: MessageStrategy) extends JunctionTreePropagation(strategy) {

  private var totalMessagesSent = 0;

  override def computeMarginals(jt: JunctionTree) {

    propagate(jt)
    //  jt.normalizeAll ();      // Necessary if jt originally unnormalized
  }

  /* Hugin-style propagation for junction trees */

  // bottom-up pass
  private def collectEvidence(jt: JunctionTree, parent: VarSet, child: VarSet) {

    jt.getChildren(child).foreach(gchild => collectEvidence(jt, child, gchild.asInstanceOf[VarSet]))
    if (parent != null) {
      totalMessagesSent += 1
      strategy.sendMessage(jt, child, parent)
    }
  }

  // top-down pass
  private def distributeEvidence(jt: JunctionTree, parent: VarSet) {

    jt.getChildren(parent).foreach { child =>
      totalMessagesSent += 1
      strategy.sendMessage(jt, parent, child.asInstanceOf[VarSet])
      distributeEvidence(jt, child.asInstanceOf[VarSet])
    }
  }

  private def propagate(jt: JunctionTree) {
    val root: VarSet = jt.getRoot().asInstanceOf[VarSet]
    collectEvidence(jt, null, root)
    distributeEvidence(jt, root)
  }

  override def lookupMarginal(jt: JunctionTree, varSet: VarSet): Factor =
    {
      val parent: VarSet = jt.findParentCluster(varSet)
      if (parent == null) {
        throw new UnsupportedOperationException
        ("No parent cluster in " + jt + " for clique " + varSet)
      }

      val cpf: Factor = jt.getCPF(parent);

      val marginal: Factor = strategy.extractBelief(cpf, varSet)

      //  marginal.normalize ();

      marginal
    }

  override def lookupMarginal(jt: JunctionTree, variable: Variable): Factor =
    {
      val parent: VarSet = jt.findParentCluster(variable)
      val cpf: Factor = jt.getCPF(parent)

      val marginal: Factor = strategy.extractBelief(cpf, new HashVarSet(Array(variable)))
      //  marginal.normalize ();

      marginal
    }

}