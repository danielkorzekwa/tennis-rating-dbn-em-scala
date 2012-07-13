package dk.tennis.em.bn

import scala.annotation.tailrec
import scala.Math._

object AssignmentUtil {

  /**
   * Compute all assignments for a list of dimensions. E.g. if dimensions are List(2,3),
   * then the following assignments are produced:
   * 0 0
   * 0 1
   * 0 2
   * 1 0
   * 1 1
   * 1 2
   *
   */
  def computeAllAssignments(dimensions: Seq[Int]): Seq[Seq[Int]] = {
    require(dimensions.find(_ <= 0).isEmpty, "Dimension can't be less or equal than 0")

    @tailrec
    def product(currValues: Seq[List[Int]], dimensions: Seq[Int], dimIndex: Int): Seq[Seq[Int]] = {

      val newValues = (0 until dimensions(dimIndex)).flatMap(dimValue =>
        currValues.map(v => dimValue :: v))

      if (dimIndex == 0) newValues else product(newValues, dimensions, dimIndex - 1)
    }

    val dimensionNum = dimensions.size
    val initialValues = for (dimValue <- 0 until dimensions(dimensionNum - 1)) yield List(dimValue)

    if (dimensionNum == 1) initialValues else product(initialValues, dimensions, dimensionNum - 2)

  }

  /**
   * Convert assignment to index.
   *  For example, the following table presents mappings from assignments to indices for dimensions [3 2]
   *  0 0 -> 0
   *  0 1 -> 1
   *  1 0 -> 2
   *  1 1 -> 3
   *  2 0 -> 4
   *  2 1 -> 5
   */
  def assignmentToIndex(assignment: Seq[Int], dimensions: Seq[Int]): Int = {
    require(dimensions.find(_ <= 0).isEmpty, "Dimension can't be less or equal than 0")
    require(assignment.size == dimensions.size, "The length of assignment must be equal to the number of dimensions.")

    val dimValues = assignment.zipWithIndex.map {
      case (a, index) =>
        a * dimensions.drop(index + 1).product
    }
    dimValues.sum
  }

  /**
   * Convert value index to assignment.
   *  For example, the following table presents mappings from value indexes to corresponding assignments for dimensions [3 2]
   *  0 -> 0 0
   *  1 -> 0 1
   *  2 -> 1 0
   *  3 -> 1 1
   *  4 -> 2 0
   *  5 -> 2 1
   *
   */
  def indexToAssignment(index: Int, dimensions: Seq[Int]): Seq[Int] = {
    require(dimensions.find(_ <= 0).isEmpty, "Dimension can't be less or equal than 0")
    require(index >= 0, "Index of assignment must be greater or equal to 0")
    
    val assignment = List.make(dimensions.size, index)

    val cumProdDimensions = (dimensions.tail :+ 1)
    val cumProd = cumProdDimensions.zipWithIndex.map { case (d, index) => cumProdDimensions.slice(index, cumProdDimensions.size).product }

    val floorValue = assignment.zip(cumProd).map { case (a, cumProd) => floor(a.toDouble / cumProd) }
    val assignmentFinal = floorValue.zip(dimensions).map { case (floorValue, a) => (floorValue % a).toInt }

    assignmentFinal

  }

}