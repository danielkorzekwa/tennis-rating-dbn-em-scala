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

  def calibrate(iterNum: (Int) => Unit): ClusterGraph = {

    @tailrec
    def calibrateUntilConverge(oldGraph: GenericClusterGraph, currentIter: Int): GenericClusterGraph = {
      iterNum(currentIter)

      val newGraph = oldGraph.calibrateIteration()

      if (oldGraph.isCalibrated(newGraph)) newGraph
      else calibrateUntilConverge(newGraph, currentIter + 1)
    }

    calibrateUntilConverge(this, 1)
  }

  /**Computes messages passed between clusters and returns new factor graph.*/
  private def calibrateIteration(): GenericClusterGraph = {
    val rand = new Random(System.currentTimeMillis())

    val shuffledClusters = rand.shuffle(clusters)

    val newMessages = shuffledClusters.flatMap(c => calNewClusterMessages(c))

    val newClusterGraph = GenericClusterGraph(clusters, newMessages)
    newClusterGraph
  }

  /**
   * Computes and returns new messages sent by cluster to other clusters.
   *
   * @param cluster Cluster, which new messages are computed for
   *
   * @return New set of messages computed for a cluster
   */
  private def calNewClusterMessages(cluster: Cluster): Seq[Message] = {
    val messagesOut = messages.filter(m => m.srcClusterId == cluster.id)
    messagesOut.map(m => calNewMessage(cluster, m))
  }

  /**
   * Compute new message
   *
   * @param cluster Cluster, which sends this message
   * @param message Message, which has to be recomputed
   *
   * @return New computed message
   *
   */
  private def calNewMessage(cluster: Cluster, message: Message): Message = {
    val messagesInButOne = messages.filter(m => m.destClusterId == cluster.id && m.srcClusterId != message.destClusterId)

    val newMessageOutFactor = cluster.factor.product(messagesInButOne.map(m => m.factor): _*)
    val newMessageOutMarginal = newMessageOutFactor.marginal(message.factor.variables.map(_.name): _*).normalize()
    val newMessageOut = Message(cluster.id, message.destClusterId, newMessageOutMarginal)
    newMessageOut
  }

  def clusterBelief(clusterId: Int): Factor = {
    val cluster = clusters.find(c => c.id == clusterId).get
    val messagesIn = messages.filter(m => m.destClusterId == cluster.id)
    val factorsIn = messagesIn.map(m => m.factor)

    val clusterBelief = cluster.factor.product(factorsIn: _*)
    clusterBelief.normalize()
  }

  def getClusters(): Seq[Cluster] = clusters

  /**Returns true if this cluster is calibrated with that cluster.*/
  private def isCalibrated(clusterGraph: GenericClusterGraph): Boolean = {

    val oldMessages = messages
    val newMessages = clusterGraph.messages

    val notCalibratedMsg = oldMessages.find { oldMsg =>
      val newMsg = newMessages.find(newMsg => newMsg.srcClusterId == oldMsg.srcClusterId && newMsg.destClusterId == oldMsg.destClusterId).get
      !messagesCalibrated(oldMsg, newMsg)
    }
    notCalibratedMsg.isEmpty
  }

  /**Returns true if two messages are calibrated, otherwise false is returned.*/
  private def messagesCalibrated(oldMsg: Message, newMsg: Message): Boolean = {
    val notCalibratedValue = oldMsg.factor.values.zip(newMsg.factor.values).find { case (oldVal, newVal) => abs(oldVal - newVal) > threashold }
    notCalibratedValue.isEmpty
  }
}

/**Companion object of [[dk.tennis.em.loopybp.GenericClusterGraph]]*/
object GenericClusterGraph {

  /**
   * Message sent between clusters.
   *
   * @constructor Creates message, which is sent between cluster
   *
   * @param srcClusterId Cluster sending this message
   * @param destClusterId Cluster receiving this message
   * @param factor Factor to be sent between clusters
   */
  case class Message(srcClusterId: Int, destClusterId: Int, factor: Factor)

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

        List(Message(clusterId1, clusterId2, sepset), Message(clusterId2, clusterId1, sepset))
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
    val dimension = intersectVariables.map(v => v.values.size).product
    val uniformValues = (0 until dimension).map(i => 1d)
    Factor(intersectVariables, uniformValues)
  }
}