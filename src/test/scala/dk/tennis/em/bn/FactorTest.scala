package dk.tennis.em.bn

import org.junit._
import Assert._
import Factor._

class FactorTest {

  /**Tests for var constructor.*/
  @Test(expected = classOf[IllegalArgumentException]) def create_var_list_of_value_cant_be_empty {
    Var("factorName", Nil)
  }

  /**Tests for factor constructor.*/
  @Test(expected = classOf[IllegalArgumentException]) def create_factor_number_of_factor_values_must_be_equal_to_product_of_variable_dimensions {
    Factor(Var("A", ("F", "T")), 1)
  }

  /**Tests for product() function.*/
  @Test def product_two_factors {
    val factorA = Factor(Var("A", ("F", "T")), 3d / 7, 4d / 7)

    val factorB = Factor(Var("A", ("F", "T")), Var("B", ("F", "T")), 1d / 3, 2d / 3, 3d / 4, 1d / 4)

    val factorProduct = factorA.product(factorB)

    assertEquals(2, factorProduct.variables.size)
    assertEquals(Var("A", ("F", "T")), factorProduct.variables(0))
    assertEquals(Var("B", ("F", "T")), factorProduct.variables(1))

    assertEquals(4, factorProduct.values.size)
    assertEquals(List(3d / 21, 6d / 21, 12d / 28, 4d / 28), factorProduct.values)
  }

  @Test def product_three_factors {
    val factorA = Factor(Var("Rating1", ("S", "M", "L")), 2d / 6, 3d / 6, 1d / 6)

    val factorB = Factor(Var("Rating1", ("S", "M", "L")), Var("Rating2", ("S", "M", "L")), Var("Score", ("W", "L")),
      1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18)

    val factorC = Factor(Var("Rating2", ("S", "M", "L")), 1d / 6, 1d / 6, 4d / 6)
    val factorProduct = factorA.product(factorB).product(factorC)

    assertEquals(3, factorProduct.variables.size)
    assertEquals(Var("Rating1", ("S", "M", "L")), factorProduct.variables(0))
    assertEquals(Var("Rating2", ("S", "M", "L")), factorProduct.variables(1))
    assertEquals(Var("Score", ("W", "L")), factorProduct.variables(2))

    assertEquals(18, factorProduct.values.size)
    assertEquals(0.055, factorProduct.values(0), 0.001)
    assertEquals(0.111, factorProduct.values(1), 0.001)
    assertEquals(0.166, factorProduct.values(2), 0.001)
    assertEquals(0.222, factorProduct.values(3), 0.001)
    assertEquals(1.111, factorProduct.values(4), 0.001)
    assertEquals(1.333, factorProduct.values(5), 0.001)
    assertEquals(0.583, factorProduct.values(6), 0.001)
    assertEquals(0.666, factorProduct.values(7), 0.001)
    assertEquals(0.75, factorProduct.values(8), 0.001)
    assertEquals(0.833, factorProduct.values(9), 0.001)
    assertEquals(3.666, factorProduct.values(10), 0.001)
    assertEquals(4.0, factorProduct.values(11), 0.001)
    assertEquals(0.361, factorProduct.values(12), 0.001)
    assertEquals(0.388, factorProduct.values(13), 0.001)
    assertEquals(0.416, factorProduct.values(14), 0.001)
    assertEquals(0.444, factorProduct.values(15), 0.001)
    assertEquals(1.888, factorProduct.values(16), 0.001)
    assertEquals(2, factorProduct.values(17), 0.001)
  }

  @Test def product_three_factors_overloaded {
    val factorA = Factor(Var("Rating1", ("S", "M", "L")), 2d / 6, 3d / 6, 1d / 6)

    val factorB = Factor(Var("Rating1", ("S", "M", "L")), Var("Rating2", ("S", "M", "L")), Var("Score", ("W", "L")),
      1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18)

    val factorC = Factor(Var("Rating2", ("S", "M", "L")), 1d / 6, 1d / 6, 4d / 6)
    val factorProduct = factorA.product(factorC :: factorB :: Nil)

    assertEquals(3, factorProduct.variables.size)
    assertEquals(Var("Rating1", ("S", "M", "L")), factorProduct.variables(0))
    assertEquals(Var("Rating2", ("S", "M", "L")), factorProduct.variables(1))
    assertEquals(Var("Score", ("W", "L")), factorProduct.variables(2))

    assertEquals(18, factorProduct.values.size)
    assertEquals(0.055, factorProduct.values(0), 0.001)
    assertEquals(0.111, factorProduct.values(1), 0.001)
    assertEquals(0.166, factorProduct.values(2), 0.001)
    assertEquals(0.222, factorProduct.values(3), 0.001)
    assertEquals(1.111, factorProduct.values(4), 0.001)
    assertEquals(1.333, factorProduct.values(5), 0.001)
    assertEquals(0.583, factorProduct.values(6), 0.001)
    assertEquals(0.666, factorProduct.values(7), 0.001)
    assertEquals(0.75, factorProduct.values(8), 0.001)
    assertEquals(0.833, factorProduct.values(9), 0.001)
    assertEquals(3.666, factorProduct.values(10), 0.001)
    assertEquals(4.0, factorProduct.values(11), 0.001)
    assertEquals(0.361, factorProduct.values(12), 0.001)
    assertEquals(0.388, factorProduct.values(13), 0.001)
    assertEquals(0.416, factorProduct.values(14), 0.001)
    assertEquals(0.444, factorProduct.values(15), 0.001)
    assertEquals(1.888, factorProduct.values(16), 0.001)
    assertEquals(2, factorProduct.values(17), 0.001)
  }

  @Test def product_fourteen_factors {
    val factors = (1 to 12).map(i => Factor(Var("Rating" + i, ("S", "M", "L")), 1, 2, 3))

    val factorProduct = factors.head.product(factors.tail)

    assertEquals(12, factorProduct.variables.size)
    assertEquals(Var("Rating1", ("S", "M", "L")), factorProduct.variables(0))
    assertEquals(Var("Rating2", ("S", "M", "L")), factorProduct.variables(1))
    assertEquals(Var("Rating12", ("S", "M", "L")), factorProduct.variables(11))

    assertEquals(531441, factorProduct.values.size)
    assertEquals(1, factorProduct.values(0), 0.0001)
    assertEquals(2, factorProduct.values(1), 0.001)
    assertEquals(3, factorProduct.values(2), 0.001)
    assertEquals(2, factorProduct.values(3), 0.001)
    assertEquals(531441, factorProduct.values(531441 - 1), 0.001)
  }

  /**Tests for evidence() function.*/
  @Test def evidence_factor_with_single_variable {
    val factorA = Factor(Var("Rating1", ("S", "M", "L")), 2d / 6, 3d / 6, 1d / 6)
    val factorWithEvidence = factorA.evidence(("Rating1", "M") :: Nil)

    assertEquals(0, factorWithEvidence.values(0), 0)
    assertEquals(3d / 6, factorWithEvidence.values(1), 0)
    assertEquals(0, factorWithEvidence.values(2), 0)
  }

  @Test def evidence_factor_with_two_variables {
    val factorA = Factor(Var("Rating1", ("S", "M", "L")), Var("Score", ("W", "L")), 1, 2, 3, 4, 5, 6)
    val factorWithEvidence = factorA.evidence(("Rating1", "M") :: Nil)

    assertEquals(0, factorWithEvidence.values(0), 0)
    assertEquals(0, factorWithEvidence.values(1), 0)
    assertEquals(3, factorWithEvidence.values(2), 0)
    assertEquals(4, factorWithEvidence.values(3), 0)
    assertEquals(0, factorWithEvidence.values(4), 0)
    assertEquals(0, factorWithEvidence.values(5), 0)
  }

  @Test def evidence_factor_with_two_variables_double_evidence {
    val factorA = Factor(Var("Rating1", ("S", "M", "L")), Var("Score", ("W", "L")), 1, 2, 3, 4, 5, 6)
    val factorWithEvidence = factorA.evidence(("Rating1", "M") :: ("Score", "L") :: Nil)

    assertEquals(0, factorWithEvidence.values(0), 0)
    assertEquals(0, factorWithEvidence.values(1), 0)
    assertEquals(0, factorWithEvidence.values(2), 0)
    assertEquals(4, factorWithEvidence.values(3), 0)
    assertEquals(0, factorWithEvidence.values(4), 0)
    assertEquals(0, factorWithEvidence.values(5), 0)
  }

  /**Tests for marginal() function.*/

  @Test(expected = classOf[IllegalArgumentException]) def marginal_zero_variables {
    Factor(Var("R1", ("1", "2", "3")), 1d / 3, 1d / 3, 1d / 3).marginal(Nil)
  }

  @Test def marginal_single_variable_no_evidence {
    val factorR1 = Factor(Var("R1", ("1", "2", "3")), 1d / 3, 1d / 3, 1d / 3)
    var factorR2 = Factor(Var("R2", ("1", "2", "3")), 1d / 3, 1d / 3, 1d / 3)
    val factorScore = Factor(Var("R1", ("1", "2", "3")), Var("R2", ("1", "2", "3")), Var("Score", ("W", "L")), 0.5, 0.5, 1d / 3, 2d / 3, 0.25, 0.75, 2d / 3, 1d / 3, 0.5, 0.5, 0.4, 0.6, 0.75, 0.25, 0.6, 0.4, 0.5, 0.5)
    val fullJoinFactor = factorR1.product(factorR2).product(factorScore)

    val marginal = fullJoinFactor.marginal(List("R2"))

    assertEquals(List(Var("R2", ("1", "2", "3"))), marginal.variables)
    assertEquals(3, marginal.values.size)
    assertEquals(1d / 3, marginal.values(0), 0.001)
    assertEquals(1d / 3, marginal.values(1), 0.001)
    assertEquals(1d / 3, marginal.values(2), 0.001)
  }

  @Test def marginal_single_variable_with_single_evidence {
    val factorR1 = Factor(Var("R1", ("1", "2", "3")), 1d / 3, 1d / 3, 1d / 3)
    var factorR2 = Factor(Var("R2", ("1", "2", "3")), 1d / 3, 1d / 3, 1d / 3)
    val factorScore = Factor(Var("R1", ("1", "2", "3")), Var("R2", ("1", "2", "3")), Var("Score", ("W", "L")), 0.5, 0.5, 1d / 3, 2d / 3, 0.25, 0.75, 2d / 3, 1d / 3, 0.5, 0.5, 0.4, 0.6, 0.75, 0.25, 0.6, 0.4, 0.5, 0.5)
    val fullJoinFactor = factorR1.product(factorR2).product(factorScore).evidence(("Score", "W") :: Nil)

    val marginal = fullJoinFactor.marginal(List("R2")).normalize

    assertEquals(List(Var("R2", ("1", "2", "3"))), marginal.variables)
    assertEquals(3, marginal.values.size)
    assertEquals(0.4259, marginal.values(0), 0.001)
    assertEquals(0.3185, marginal.values(1), 0.001)
    assertEquals(0.2555, marginal.values(2), 0.001)
  }

  @Test def marginal_single_variable_with_double_evidence {
    val factorR1 = Factor(Var("R1", ("1", "2", "3")), 1d / 3, 1d / 3, 1d / 3)
    var factorR2 = Factor(Var("R2", ("1", "2", "3")), 1d / 3, 1d / 3, 1d / 3)
    val factorScore = Factor(Var("R1", ("1", "2", "3")), Var("R2", ("1", "2", "3")), Var("Score", ("W", "L")), 0.5, 0.5, 1d / 3, 2d / 3, 0.25, 0.75, 2d / 3, 1d / 3, 0.5, 0.5, 0.4, 0.6, 0.75, 0.25, 0.6, 0.4, 0.5, 0.5)
    val fullJoinFactor = factorR1.product(factorR2).product(factorScore).evidence(("Score", "W") :: ("R2", "2") :: Nil)

    val marginal = fullJoinFactor.marginal(List("R1")).normalize

    assertEquals(List(Var("R1", ("1", "2", "3"))), marginal.variables)
    assertEquals(3, marginal.values.size)
    assertEquals(0.2325, marginal.values(0), 0.001)
    assertEquals(0.3488, marginal.values(1), 0.001)
    assertEquals(0.4186, marginal.values(2), 0.001)
  }

  @Test def marginal_on_observed_variable {
    val factorR1 = Factor(Var("R1", ("1", "2", "3")), 1d / 3, 1d / 3, 1d / 3)
    var factorR2 = Factor(Var("R2", ("1", "2", "3")), 1d / 3, 1d / 3, 1d / 3)
    val factorScore = Factor(Var("R1", ("1", "2", "3")), Var("R2", ("1", "2", "3")), Var("Score", ("W", "L")), 0.5, 0.5, 1d / 3, 2d / 3, 0.25, 0.75, 2d / 3, 1d / 3, 0.5, 0.5, 0.4, 0.6, 0.75, 0.25, 0.6, 0.4, 0.5, 0.5)
    val fullJoinFactor = factorR1.product(factorR2).product(factorScore).evidence(("Score", "W") :: ("R1", "2") :: Nil)

    val marginal = fullJoinFactor.marginal(List("R1")).normalize

    assertEquals(List(Var("R1", ("1", "2", "3"))), marginal.variables)
    assertEquals(3, marginal.values.size)
    assertEquals(0, marginal.values(0), 0.001)
    assertEquals(1, marginal.values(1), 0.001)
    assertEquals(0, marginal.values(2), 0.001)
  }

  @Test def marginal_two_variables_with_no_evidence {
    val factorR1 = Factor(Var("R1", ("1", "2", "3")), 1d / 6, 2d / 6, 3d / 6)
    var factorR2 = Factor(Var("R2", ("1", "2", "3")), 1d / 3, 1d / 3, 1d / 3)
    val factorScore = Factor(Var("R1", ("1", "2", "3")), Var("R2", ("1", "2", "3")), Var("Score", ("W", "L")), 0.5, 0.5, 1d / 3, 2d / 3, 0.25, 0.75, 2d / 3, 1d / 3, 0.5, 0.5, 0.4, 0.6, 0.75, 0.25, 0.6, 0.4, 0.5, 0.5)
    val fullJoinFactor = factorR1.product(factorR2).product(factorScore)

    val marginalFactor = fullJoinFactor.marginal(List("R1", "R2")).normalize

    assertEquals(Var("R1", ("1", "2", "3")) :: Var("R2", ("1", "2", "3")) :: Nil, marginalFactor.variables)
    assertEquals(9, marginalFactor.values.size)
    assertEquals(0.055, marginalFactor.values(0), 0.001)
    assertEquals(0.055, marginalFactor.values(1), 0.001)
    assertEquals(0.055, marginalFactor.values(2), 0.001)
    assertEquals(0.111, marginalFactor.values(3), 0.001)
    assertEquals(0.111, marginalFactor.values(4), 0.001)
    assertEquals(0.111, marginalFactor.values(5), 0.001)
    assertEquals(0.166, marginalFactor.values(6), 0.001)
    assertEquals(0.166, marginalFactor.values(7), 0.001)
    assertEquals(0.166, marginalFactor.values(8), 0.001)

    val singleMarginal = marginalFactor.marginal("R1" :: Nil).normalize
    assertEquals(List(Var("R1", ("1", "2", "3"))), singleMarginal.variables)
    assertEquals(3, singleMarginal.values.size)
    assertEquals(0.1666, singleMarginal.values(0), 0.001)
    assertEquals(0.333, singleMarginal.values(1), 0.001)
    assertEquals(0.5, singleMarginal.values(2), 0.001)
  }

  @Test def marginal_two_variables_with_evidence {
    val factorR1 = Factor(Var("R1", ("1", "2", "3")), 1d / 3, 1d / 3, 1d / 3)
    var factorR2 = Factor(Var("R2", ("1", "2", "3")), 1d / 3, 1d / 3, 1d / 3)
    val factorScore = Factor(Var("R1", ("1", "2", "3")), Var("R2", ("1", "2", "3")), Var("Score", ("W", "L")), 0.5, 0.5, 1d / 3, 2d / 3, 0.25, 0.75, 2d / 3, 1d / 3, 0.5, 0.5, 0.4, 0.6, 0.75, 0.25, 0.6, 0.4, 0.5, 0.5)
    val fullJoinFactor = factorR1.product(factorR2).product(factorScore).evidence(("Score", "W") :: Nil)

    val marginalFactor = fullJoinFactor.marginal(List("R1", "R2")).normalize

    assertEquals(Var("R1", ("1", "2", "3")) :: Var("R2", ("1", "2", "3")) :: Nil, marginalFactor.variables)
    assertEquals(9, marginalFactor.values.size)
    assertEquals(0.111, marginalFactor.values(0), 0.001)
    assertEquals(0.074, marginalFactor.values(1), 0.001)
    assertEquals(0.055, marginalFactor.values(2), 0.001)
    assertEquals(0.148, marginalFactor.values(3), 0.001)
    assertEquals(0.111, marginalFactor.values(4), 0.001)
    assertEquals(0.088, marginalFactor.values(5), 0.001)
    assertEquals(0.166, marginalFactor.values(6), 0.001)
    assertEquals(0.133, marginalFactor.values(7), 0.001)
    assertEquals(0.111, marginalFactor.values(8), 0.001)

    val singleMarginal = marginalFactor.marginal("R1" :: Nil).normalize
    assertEquals(List(Var("R1", ("1", "2", "3"))), singleMarginal.variables)
    assertEquals(3, singleMarginal.values.size)
    assertEquals(0.2407, singleMarginal.values(0), 0.001)
    assertEquals(0.348, singleMarginal.values(1), 0.001)
    assertEquals(0.411, singleMarginal.values(2), 0.001)
  }

  @Test def marginal_three_variables_with_evidence {
    val factorR1 = Factor(Var("R1", ("1", "2", "3")), 1d / 3, 1d / 3, 1d / 3)
    var factorR2 = Factor(Var("R2", ("1", "2", "3")), 1d / 3, 1d / 3, 1d / 3)
    val factorScore = Factor(Var("R1", ("1", "2", "3")), Var("R2", ("1", "2", "3")), Var("Score", ("W", "L")), 0.5, 0.5, 1d / 3, 2d / 3, 0.25, 0.75, 2d / 3, 1d / 3, 0.5, 0.5, 0.4, 0.6, 0.75, 0.25, 0.6, 0.4, 0.5, 0.5)
    val fullJoinFactor = factorR1.product(factorR2).product(factorScore).evidence(("Score", "W") :: Nil)

    val marginalFactor = fullJoinFactor.marginal(List("R1", "R2", "Score")).normalize

    //  println(marginalFactor.evidence(("R1", "1") :: ("R2", "2") :: Nil))
    // assertEquals(0, marginalFactor.evidence(("R1", "1") :: ("R2", "2") :: Nil).values(0), 0.001)
    // assertEquals(1, marginalFactor.evidence(("R1", "1") :: ("R2", "2") :: Nil).values(1), 0.001)

  }

  /**Tests for normalize() function.*/

  @Test def normalize_even_probababilities {
    val factor = Factor(Var("R1", ("1", "2", "3")), 1d / 5, 1d / 5, 1d / 5)
    val normFactor = factor.normalize()

    assertEquals(3, normFactor.values.size)
    assertEquals(1d / 3, normFactor.values(0), 0.001)
    assertEquals(1d / 3, normFactor.values(1), 0.001)
    assertEquals(1d / 3, normFactor.values(2), 0.001)

  }

  @Test def normalize_not_even_probabilities {
    val factor = Factor(Var("R1", ("1", "2", "3")), 1d / 3, 2d / 3, 3d / 3)
    val normFactor = factor.normalize()

    assertEquals(3, normFactor.values.size)
    assertEquals(1d / 6, normFactor.values(0), 0.001)
    assertEquals(2d / 6, normFactor.values(1), 0.001)
    assertEquals(3d / 6, normFactor.values(2), 0.001)

  }
}