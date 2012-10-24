package dk.tennis.em.loopybp
import scala.annotation.tailrec
import scala.util.Random
import GenericClusterGraph._
import dk.tennis.em.bn.Factor
import ClusterGraph._
import Factor._
import scala.Math._

/**
 * Reference implementation of ClusterGraph.
 *
 * @author korzekwad
 *
 * @constructor Creates new cluster graph
 *
 * @param clusters Clusters to be added to cluster graph
 * @param messages Messages to be passed between clusters
 * @param threashold Stopping criteria for calibration process.
 * The difference between all pairs of old and new messages in a cluster graph must be lower than threshold value.
 */
case class GenericClusterGraph(clusters: Seq[Cluster], messages: Seq[Message], threashold: Double = 0.00001) extends ClusterGraph {

  private val msgByDestClusterId: Map[Int, Seq[Message]] = messages.groupBy(m => m.destClusterId)
  private val msgBySrcClusterId: Map[Int, Seq[Message]] = messages.groupBy(m => m.srcClusterId)

  def calibrate(iterNum: (Int) => Unit): ClusterGraph = {

    @tailrec
    def calibrateUntilConverge(oldGraph: GenericClusterGraph, currentIter: Int): GenericClusterGraph = {
      iterNum(currentIter)

      oldGraph.calibrateIteration()

      if (oldGraph.isCalibrated()) oldGraph
      else calibrateUntilConverge(oldGraph, currentIter + 1)
    }

    calibrateUntilConverge(this, 1)
  }

  val rand = new Random(System.currentTimeMillis())
  /**Computes messages passed between clusters and returns new factor graph.*/
  private def calibrateIteration() {

    val rand = new Random(System.currentTimeMillis())
    val shuffledClusters = rand.shuffle(clusters)

    shuffledClusters.foreach { c => updateClusterMessages(c) }

  }

  /**
   * Computes and returns new messages sent by cluster to other clusters.
   *
   * @param cluster Cluster, which new messages are computed for
   *
   * @return New set of messages computed for a cluster
   */
  private def updateClusterMessages(cluster: Cluster) {
    val messagesOut = msgBySrcClusterId(cluster.id)
    messagesOut.foreach(m => updateMessage(cluster, m))
  }

  /**
   * Computes new message
   *
   * @param cluster Cluster, which sends this message
   * @param message Message, which has to be recomputed
   *
   * @return New computed message
   *
   */
  private def updateMessage(cluster: Cluster, message: Message) {
    val messagesInButOne = msgByDestClusterId(cluster.id).filter(m => m.srcClusterId != message.destClusterId)

    var factors = List[Factor]()

    for (m <- messagesInButOne) { factors = m.newFactor :: factors }
    val newMessageOutFactor = cluster.factor.product(factors, Option(variableMapping))

    var varNames = List[Int]()

    for (v <- message.newFactor.variables) { varNames = v.id :: varNames }
    val newMessageOutMarginal = newMessageOutFactor.marginal(varNames).normalize()

    message.oldFactor = message.newFactor
    message.newFactor = newMessageOutMarginal
  }

  def variableMapping(factorA: Factor, factorB: Factor): Array[VariableMapping] = {
    val variableMappingArray = new Array[VariableMapping](factorA.variables.size)

    var i = 0
    while (i < factorA.variables.size) {
      val factorBMapping = if (factorA.variables(i).id == factorB.variables.head.id) Some(0) else None
      val variableMapping = VariableMapping(factorA.variables(i), Some(i), factorBMapping)
      variableMappingArray(i) = variableMapping
      i += 1
    }
    variableMappingArray
  }

  def clusterBelief(clusterId: Int): Factor = {
    val cluster = clusters.find(c => c.id == clusterId).get
    val messagesIn = messages.filter(m => m.destClusterId == cluster.id)
    val factorsIn = messagesIn.map(m => m.newFactor)

    val clusterBelief = cluster.factor.product(factorsIn)
    clusterBelief.normalize()
  }

  def logLikelihood(assignment: Seq[Assignment]): Double = {
    val allVariables = clusters.flatMap(c => c.factor.variables)
    val allVariableNames = allVariables.map(v => v.id).distinct

    val assignmentDiff = allVariableNames.diff(assignment.map(a => a.variableId))
    require(assignmentDiff.size == 0, "Assignment of all variables in a cluster is required.")
    require(assignment.size == assignment.distinct.size, "Assignment is not unique.")

    val clustersLoglikelihood = clusters.map(c => log(likelihood(clusterBelief(c.id), assignment))).sum

    val sepsetBeliefs: Seq[Factor] = messages.filter(m => m.srcClusterId > m.destClusterId).map { m =>
      val linkedMessage = messages.find(msg => msg.srcClusterId == m.destClusterId && msg.destClusterId == m.srcClusterId).get
      m.newFactor.productSingle(linkedMessage.newFactor).normalize()
    }
    val sepsetLoglikelihood = sepsetBeliefs.map(b => log(likelihood(b, assignment))).sum

    clustersLoglikelihood - sepsetLoglikelihood
  }

  /**Returns likelihood of factor assignment*/
  private def likelihood(factor: Factor, assignment: Seq[Assignment]): Double = {

    def assignmentValue(varId: Int): Int = assignment.find(a => a.variableId == varId).get.valueIndex

    //Tuple2[varName,varValue]
    val factorAssignment: Seq[Tuple2[Int, Int]] = factor.variables.map(v => (v.id, assignmentValue(v.id)))

    val evidenceFactor = factor.evidence(factorAssignment: _*)
    evidenceFactor.values.sum
  }

  def getClusters(): Seq[Cluster] = clusters

  /**Returns marginal factor for a variable in a cluster graph.*/
  def marginal(varId: Int): Factor = {
    val varCluster = clusters.find(c => c.factor.variables.map(v => v.id).contains(varId)).get
    val varMarginal = clusterBelief(varCluster.id).marginal(varId)
    varMarginal
  }

  /**Returns true if this cluster is calibrated with that cluster.*/
  private def isCalibrated(): Boolean = {

    val notCalibratedCluster = msgBySrcClusterId.find {
      case (clusterId, messages) =>

        val notCalibratedMsg = messages.find { msg => !messagesCalibrated(msg) }

        !notCalibratedMsg.isEmpty
    }

    notCalibratedCluster.isEmpty
  }

  /**Returns true if two messages are calibrated, otherwise false is returned.*/
  private def messagesCalibrated(msg: Message): Boolean = {
    val notCalibratedValue = msg.oldFactor.values.zip(msg.newFactor.values).
      find { case (oldVal, newVal) => abs(oldVal - newVal) > threashold }

    notCalibratedValue.isEmpty
  }
}

/**Companion object of [[dk.tennis.em.loopybp.GenericClusterGraph]]*/
object GenericClusterGraph {

  /**
   * Message sent between clusters.
   *
   * @constructor Creates message, which is sent between clusters
   *
   * @param srcClusterId Cluster sending this message
   * @param destClusterId Cluster receiving this message
   * @param factor Factor to be sent between clusters
   */
  case class Message(srcClusterId: Int, destClusterId: Int, var oldFactor: Factor, var newFactor: Factor)

  /**
   * Creates cluster graph.
   *
   * @param clusters Clusters to be added to cluster graph
   * @param edges Bidirectional edges between clusters. Tuple2 [clusterId1,clusterId2]
   *
   * @return Cluster graph
   */
  def apply(clusters: Seq[Cluster], edges: Seq[Tuple2[Int, Int]]): ClusterGraph = {

    val messages = edges.flatMap {
      case (clusterId1, clusterId2) =>
        val cluster1 = clusters.find(c => c.id == clusterId1).get
        val cluster2 = clusters.find(c => c.id == clusterId2).get
        val sepset = calcSepset(cluster1, cluster2)

        List(Message(clusterId1, clusterId2, sepset, sepset), Message(clusterId2, clusterId1, sepset, sepset))
    }

    GenericClusterGraph(clusters, messages)
  }

  /**
   * Returns sepset between two clusters.
   *
   * @param clusterA Cluster included in a sepset
   * @param clusterB Cluster included in a sepset
   *
   * @return Factor representing sepset between two cluster
   */
  private def calcSepset(clusterA: Cluster, clusterB: Cluster): Factor = {
    val intersectVariables = clusterA.factor.variables.intersect(clusterB.factor.variables)
    val dimension = intersectVariables.map(v => v.dim).product
    val uniformValues = (0 until dimension).map(i => 1d).toArray
    Factor(intersectVariables, uniformValues)
  }
}