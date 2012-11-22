package dk.tennis.dbn.utils

import scala.collection._
import dk.tennis.dbn.clustergraph.TennisVar

case class PlayerVars {

  /**Map[time slice,Map[playerName,player variable]]*/
  private val playerVariables: mutable.Map[Int, mutable.Map[String, TennisVar]] = mutable.Map()

   /**Map[playerName,lastTimeSlice]*/
  private val lastPlayerTimeSliceMap: mutable.Map[String, Int] = mutable.Map()
  
  def getPlayerVar(playerName: String, timeSlice: Int): Option[TennisVar] =
    {
      playerVariables.get(timeSlice) match {
        case None => None
        case Some(timeSliceVars) => timeSliceVars.get(playerName)
      }
    }
  
   def addPlayerVariable(playerName: String, variable: TennisVar) {
    val timeSlicePlayerVariables = playerVariables.getOrElseUpdate(variable.timeSlice, mutable.Map[String, TennisVar]())
    timeSlicePlayerVariables += (playerName -> variable)
       
    lastPlayerTimeSliceMap += playerName -> variable.timeSlice
  }
   
   def getLastPlayerTimeSlice(playerName:String):Option[Int] = lastPlayerTimeSliceMap.get(playerName)
}