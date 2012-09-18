package dk.tennis.em.dbn.infer.grmm

import org.junit._
import Assert._
import edu.umass.cs.mallet.grmm.types.FactorGraph
import edu.umass.cs.mallet.grmm.types.TableFactor
import edu.umass.cs.mallet.grmm.types.Variable
import edu.umass.cs.mallet.grmm.inference.TreeBP
import edu.umass.cs.mallet.grmm.inference.JunctionTreeInferencer
import dk.tennis.em.util.VectorAssert._

/**
 * Test for inference in Bayesian Network using Mallet GRMM toolkit (http://mallet.cs.umass.edu/grmm/index.php
 *
 * @author korzekwad
 */
class GrmmTest {

  val factorGraph = new FactorGraph()

  val factors: Seq[TableFactor] = (1 to 500).flatMap { i =>
    val varR1 = new Variable(3)
    val varR2 = new Variable(3)
    val varScore = new Variable(2)

    val factorR1 = new TableFactor(Array(varR1), Array(1d / 6, 2d / 6, 3d / 6))
    val factorR2 = new TableFactor(Array(varR2), Array(1d / 3, 1d / 3, 1d / 3))
    val factorScore = new TableFactor(Array(varR1, varR2, varScore), Array(0.5, 0.5, 1d / 3, 2d / 3, 0.25, 0.75, 2d / 3, 1d / 3, 0.5, 0.5, 0.4, 0.6, 0.75, 0.25, 0.6, 0.4, 0.5, 0.5))
    List(factorR1, factorR2, factorScore)
  }

  factors.foreach(f => factorGraph.addFactor(f))
  
  @Test def inference {

    val inferencer = new JunctionTreeInferencer()
    inferencer.computeMarginals(factorGraph)

    val varMarginal = inferencer.lookupMarginal(factors(2).varSet())

    val expected = List(0.0278, 0.0278, 0.0185, 0.0370, 0.0139, 0.0417, 0.0741, 0.0370, 0.0556, 0.0556, 0.0444, 0.0667, 0.1250, 0.0417, 0.1000, 0.0667, 0.0833, 0.0833)
    val actual = varMarginal.asInstanceOf[TableFactor].getValues().toList
    vectorAssert(expected, actual, 0.0001)
  }
}