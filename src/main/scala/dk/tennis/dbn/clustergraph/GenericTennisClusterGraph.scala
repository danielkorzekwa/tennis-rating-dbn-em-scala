package dk.tennis.dbn.clustergraph
import dk.bayes.factor.Var
import dk.tennis.dbn.utils._
import dk.bayes.clustergraph.GenericClusterGraph
import dk.bayes.factor.Factor
import dk.bayes.factor.MultiFactor
import dk.bayes.clustergraph.ClusterGraph
import scala.collection._
import dk.bayes.infer.LoopyBP

case class GenericTennisClusterGraph(priorProb: Array[Double], emissionProb: Array[Double], transitionProb: Array[Double]) extends TennisClusterGraph {

  private val keyGen = KeyGen()
  private val clusterGraph = GenericClusterGraph()
  private val evidenceMap: mutable.Map[Int, Int] = mutable.Map()

  def getClusterGraph(): ClusterGraph = clusterGraph

  def addPriorSkillCluster(timeSlice: Int): TennisVar = {

    val playerVar = TennisVar(timeSlice, keyGen.newKey(), priorProb.size)
    val factor = Factor(playerVar, priorProb)

    clusterGraph.addCluster(playerVar.id, factor, Option(1))

    playerVar
  }

  def addTransitionSkillCluster(varFrom: TennisVar, timeSliceTo: Int): Seq[TennisVar] = {

    var currVar = varFrom

    val variables = for (i <- varFrom.timeSlice until timeSliceTo) yield {
      val varCurr = currVar
      val nextTimeSlice = i + 1
      val varNext = TennisVar(nextTimeSlice, keyGen.newKey(), priorProb.size)

      val factor = MultiFactor(varCurr, varNext, transitionProb)
      clusterGraph.addCluster(varNext.id, factor, Option(2))
      clusterGraph.addEdge(varCurr.id, varNext.id)

      currVar = varNext
      varNext
    }
    variables
  }

  def addMatchCluster(playerVar1: TennisVar, playerVar2: TennisVar): TennisVar = {
    require(playerVar1.timeSlice == playerVar2.timeSlice, "Adding match between tennis players in two different time slices")

    val matchVar = TennisVar(playerVar1.timeSlice, keyGen.newKey(), 2)

    val emissionFactor = MultiFactor(playerVar1, playerVar2, matchVar, emissionProb)

    clusterGraph.addCluster(matchVar.id, emissionFactor, Option(3))

    clusterGraph.addEdge(playerVar1.id, matchVar.id)
    clusterGraph.addEdge(playerVar2.id, matchVar.id)

    matchVar
  }

  def setEvidence(varId: Int, won: Boolean) {

    def booleanToInt(value: Boolean): Int = if (value) 0 else 1

    val outcomeValue = booleanToInt(won)
    val evidence = (varId, outcomeValue)
    LoopyBP(clusterGraph).setEvidence(evidence)
    evidenceMap += evidence._1 -> evidence._2
  }
  def getEvidenceMap(): immutable.Map[Int, Int] = evidenceMap.toMap

  def getPriorParameter(): Array[Double] = priorProb
  def getEmissionParameter(): Array[Double] = emissionProb
  def getTransitionParameter(): Array[Double] = transitionProb
}