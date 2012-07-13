package dk.tennis.em.bn

import org.junit._
import Assert._

class AssignmentUtilTest {

  /**
   *  Tests for computeAllAssignments.
   */

  @Test(expected = classOf[IllegalArgumentException]) def computeAllAssignments_dimensions_must_be_positive {
    AssignmentUtil.computeAllAssignments(List(2, 3, -7))
  }

  @Test(expected = classOf[IllegalArgumentException]) def computeAllAssignments_dimensions_cant_be_zero {
    AssignmentUtil.computeAllAssignments(List(2, 3, 0, 2))
  }

  @Test def computeAllAssignments_dim_3 {
    val assignments = AssignmentUtil.computeAllAssignments(List(3))

    assertEquals(3, assignments.size)

    assertEquals(List(0), assignments(0))
    assertEquals(List(1), assignments(1))
    assertEquals(List(2), assignments(2))
  }

  @Test def computeAllAssignments_dim_2_3 {
    val assignments = AssignmentUtil.computeAllAssignments(List(2, 3))

    assertEquals(6, assignments.size)

    assertEquals(List(0, 0), assignments(0))
    assertEquals(List(0, 1), assignments(1))
    assertEquals(List(0, 2), assignments(2))
    assertEquals(List(1, 0), assignments(3))
    assertEquals(List(1, 1), assignments(4))
    assertEquals(List(1, 2), assignments(5))
  }

  @Test def computeAllAssignments_dim_3_3_2 {
    val assignments = AssignmentUtil.computeAllAssignments(List(3, 3, 2))

    assertEquals(18, assignments.size)

    assertEquals(List(0, 0, 0), assignments(0))
    assertEquals(List(0, 0, 1), assignments(1))
    assertEquals(List(0, 1, 0), assignments(2))
    assertEquals(List(0, 1, 1), assignments(3))
    assertEquals(List(0, 2, 0), assignments(4))
    assertEquals(List(0, 2, 1), assignments(5))
    assertEquals(List(1, 0, 0), assignments(6))
    assertEquals(List(1, 0, 1), assignments(7))
    assertEquals(List(1, 1, 0), assignments(8))
    assertEquals(List(1, 1, 1), assignments(9))
    assertEquals(List(1, 2, 0), assignments(10))
    assertEquals(List(1, 2, 1), assignments(11))
    assertEquals(List(2, 0, 0), assignments(12))
    assertEquals(List(2, 0, 1), assignments(13))
    assertEquals(List(2, 1, 0), assignments(14))
    assertEquals(List(2, 1, 1), assignments(15))
    assertEquals(List(2, 2, 0), assignments(16))
    assertEquals(List(2, 2, 1), assignments(17))

  }

  @Test def computeAllAssignments_dim_3_times_14 {
    val assignments = AssignmentUtil.computeAllAssignments(List.make(14, 3))

    assertEquals(4782969, assignments.size)
    assertEquals(List.make(14, 2), assignments(4782969 - 1))
  }

  /**
   * Tests for assignmentToIndex.
   * *
   */
  @Test(expected = classOf[IllegalArgumentException]) def assignmentToIndex_dimensions_must_be_positive {
    AssignmentUtil.assignmentToIndex(List(2, 3), List(5, -2))
  }
  @Test(expected = classOf[IllegalArgumentException]) def assignmentToIndex_dimensions_cant_be_zero {
    AssignmentUtil.assignmentToIndex(List(2, 3, 6), List(5, 0, 7))
  }
  @Test(expected = classOf[IllegalArgumentException]) def assignmentToIndex_length_of_assignment_must_be_equals_to_number_of_dimensions {
    AssignmentUtil.assignmentToIndex(List(2, 3, 6), List(5, 3))
  }

  @Test def assignmentToIndex_2 {
    assertEquals(2, AssignmentUtil.assignmentToIndex(List(2), List(5)))
  }

  @Test def assignmentToIndex_21 {
    assertEquals(5, AssignmentUtil.assignmentToIndex(List(2, 1), List(3, 2)))
  }

  @Test def assignmentToIndex_201 {
    assertEquals(13, AssignmentUtil.assignmentToIndex(List(2, 0, 1), List(3, 3, 2)))
  }

  /**
   * Tests for indexToAssignment.
   * *
   */
   @Test(expected = classOf[IllegalArgumentException]) def indexToAssignment_dimensions_must_be_positive {
   AssignmentUtil.indexToAssignment(5, List(3,-5 ,6,2))
  }
  @Test(expected = classOf[IllegalArgumentException]) def indexToAssignment_dimensions_cant_be_zero {
    AssignmentUtil.indexToAssignment(5, List(6,0))
  }
    @Test(expected = classOf[IllegalArgumentException]) def indexToAssignment_index_cant_be_negative {
   AssignmentUtil.indexToAssignment(-5, List(3,4 ,6,2))
  }
  
  @Test def indexToAssignment_2 {
    assertEquals(List(2), AssignmentUtil.indexToAssignment(2, List(5)))
  }

  @Test def indexToAssignment_5 {
    assertEquals(List(2, 1), AssignmentUtil.indexToAssignment(5, List(3, 2)))
  }

  @Test def indexToAssignment_13 {
    assertEquals(List(2, 0, 1), AssignmentUtil.indexToAssignment(13, List(3, 3, 2)))
  }

  @Test def indexToAssignment_repeat {
    for (i <- 0 until 500000) AssignmentUtil.indexToAssignment(13, List(3, 3, 2))
  }
}