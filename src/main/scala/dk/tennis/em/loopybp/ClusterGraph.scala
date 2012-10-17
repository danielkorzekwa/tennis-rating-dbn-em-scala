package dk.tennis.em.loopybp
import dk.tennis.em.bn.Factor
import Factor._
import ClusterGraph._

/**
 * Performs belief propagation on a cluster graph, as presented at the
 * Probabilistic Graphical Models course (https://www.coursera.org/course/pgm), Spring term 2012.
 *
 * @author korzekwad
 */
trait ClusterGraph {

  /**
   * Calibrates cluster graph.
   *
   * @param iterNum It is called by this method at the beginning of every iteration
   *
   * @return Calibrated cluster graph
   */
  def calibrate(iterNum: (Int) => Unit): ClusterGraph

  /**Returns all clusters in a cluster graph.*/
  def getClusters(): Seq[Cluster]

  /**
   * Calculates cluster belief.
   *
   * @param clusterId Unique cluster id
   *
   * @return Factor representing cluster belief
   */
  def clusterBelief(clusterId: Int): Factor

  /**Returns log likelihood of assignment for all variables in a factor graph.*/
  def logLikelihood(assignment: Seq[Assignment]): Double

}

object ClusterGraph {

  /**
   * Cluster in a cluster graph.
   *
   * @constructor Creates new cluster
   * @param id Unique cluster id
   * @param factor Initial cluster potential
   */
  case class Cluster(id: Int, factor: Factor)

  /**
   * Assignment of a value to a variable.
   *
   * @constructor Creates new assignment
   *
   * @param variableName Unique identifier of variable
   * @param variableValue Value of variable
   */
  case class Assignment(variableName: String, variableValue: String)
}