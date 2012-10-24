package dk.tennis.em.dbn.infer.grmm

import org.junit._
import Assert._
import dk.tennis.em.dbn.infer.InferDbnTennis
import dk.tennis.em.util.AssertUtil._
import dk.tennis.em.dbn.infer.InferDbnTennis
import dk.tennis.em.dbn.factorgraph.DbnTennis.Result
import edu.umass.cs.mallet.grmm.types.Variable
import edu.umass.cs.mallet.grmm.types.TableFactor
import edu.umass.cs.mallet.grmm.types.FactorGraph
import edu.umass.cs.mallet.grmm.types.LogTableFactor

class GrmmInferDbnTennisTest {

  @Test def getPLayerAWinningProb_Federer_Nadal_infer {

    val varR1 = new Variable(3)
    val varR2 = new Variable(3)
    val varScore = new Variable(2)

    val factorR1 = LogTableFactor.makeFromValues(Array(varR1), Array(5.647370307995246E-17, 2.4441886487929025E-5, 0.9999755581135146))
    val factorR2 = LogTableFactor.makeFromValues(Array(varR2), Array(1.0326870013899315E-11, 0.0014485133712921617, 0.9985514866183759))
    val factorScore = LogTableFactor.makeFromValues(Array(varR1, varR2, varScore), Array(0.5, 0.5, 1d / 3, 2d / 3, 0.25, 0.75, 2d / 3, 1d / 3, 0.5, 0.5, 0.4, 0.6, 0.75, 0.25, 0.6, 0.4, 0.5, 0.5))

    val factors = List(factorR1, factorR2, factorScore)

    val factorGraph = new FactorGraph()
    factors.foreach(f => factorGraph.addFactor(f))

    val resultVariables = List((Result("Roger Federer", "Rafael Nadal", None, 0) -> varScore))
    val playerVariables = Map[Int, Map[String, Variable]]()
    val inferTennis = GrmmInferDbnTennis(factorGraph, factorGraph, resultVariables, playerVariables)
    inferTennis.computeMarginals()

    val winningProb = inferTennis.getPlayerAWinningProb("Roger Federer", "Rafael Nadal", 0)
    assertEquals(0.5001, winningProb, 0.0001)

  }
  
    @Test def getPLayerAWinningProb_Federer_Andy_Roddick {

    val varR1 = new Variable(3)
    val varR2 = new Variable(3)
    val varScore = new Variable(2)

    val factorR1 = LogTableFactor.makeFromValues(Array(varR1), Array(7.129974785262082E-13, 4.924618919176454E-7, 0.9999995075373893))
    val factorR2 = LogTableFactor.makeFromValues(Array(varR2), Array(2.588131869674688E-4, 0.07082927266844272, 0.9289119141445964))
    val factorScore = LogTableFactor.makeFromValues(Array(varR1, varR2, varScore), 
        Array(0.5772, 0.4228, 0.2799, 0.7201, 0.1077, 0.8923, 0.7260, 0.2740, 0.4666, 0.5334, 0.1876, 0.8124, 0.9916, 0.0084, 0.8272, 0.1728, 0.4742, 0.5258))

    val factors = List(factorR1, factorR2, factorScore)

    val factorGraph = new FactorGraph()
    factors.foreach(f => factorGraph.addFactor(f))

    val resultVariables = List((Result("Roger Federer", "Rafael Nadal", None, 0) -> varScore))
    val playerVariables = Map[Int, Map[String, Variable]]()
    val inferTennis = GrmmInferDbnTennis(factorGraph, factorGraph, resultVariables, playerVariables)
    inferTennis.computeMarginals()

    val winningProb = inferTennis.getPlayerAWinningProb("Roger Federer", "Rafael Nadal", 0)
    assertEquals(0.4993, winningProb, 0.0001)

  }

}