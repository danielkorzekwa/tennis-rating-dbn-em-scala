package dk.tennis.dbn.clustergraph
import dk.bayes.factor.Var
import dk.bayes.clustergraph.ClusterGraph

trait TennisClusterGraph {

  def getEvidenceMap(): Map[Int, Int]
  def setEvidence(varId: Int, won: Boolean)

  def addPriorSkillCluster(timeSlice: Int): TennisVar

  def addTransitionSkillCluster(varFrom: TennisVar, timeSliceTo: Int): Seq[TennisVar]

  def addMatchCluster(playerVar1: TennisVar, playerVar2: TennisVar): TennisVar

  def getClusterGraph(): ClusterGraph

  def getPriorParameter(): Array[Double]
  def getEmissionParameter(): Array[Double]
  def getTransitionParameter(): Array[Double]
}