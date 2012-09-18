package dk.tennis.em.dbn.infer

import dk.tennis.em.dbn.factorgraph.DbnTennis.Result

/**
 * Creates inference engine for tennis dbn network.
 *
 * @author korzekwad
 */
trait InferDbnTennisFactory {

  /**
   *
   * @param results Results of tennis matches.
   *
   * @param  priorProb For example 0.2 0.5 0.3 //three rating values: 0,1,2 with prior probabilities 0.2 0.5 0.3 respectively.
   *
   * @param emissionProb Tennis match score (Emission probabilities, 18 values) specified in the following order:
   *
   * playerA_rating, playeB_rating, win/lose
   * 0,0,w - 0.5
   * 0,0,l - 0.5
   * 0,1,w - 1/3
   * 0,1,l - 2/3
   * 0,2,w - 0.25
   * 0,2,l - 0.75
   * 1,0,w - 2/3
   * 1,0,l - 1/3
   * ...
   *
   * @param transitionProb  * Rating transition probabilities (9 values) specified in the following order:
   * player_old_rating,player_new_rating
   * 0,0 - 0.98
   * 0,1 - 0.01
   * 0,2 - 0.01
   * 1,0 - 0.01
   * 1,1 - 0.97
   * 1,2 - 0.02
   * ...
   */
  def create(results: Seq[Result], priorProb: Seq[Double], emissionProb: Seq[Double], transitionProb: Seq[Double]): InferDbnTennis
}