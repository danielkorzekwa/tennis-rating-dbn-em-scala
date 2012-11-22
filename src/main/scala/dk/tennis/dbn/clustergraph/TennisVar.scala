package dk.tennis.dbn.clustergraph
import dk.bayes.factor.Var

case class TennisVar(timeSlice:Int,override val id: Int, override val dim: Int) extends Var(id,dim)
