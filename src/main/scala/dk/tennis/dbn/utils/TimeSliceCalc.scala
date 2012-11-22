package dk.tennis.dbn.utils
import org.joda.time.DateTime
import org.joda.time.Duration

case class TimeSliceCalc(startTime: Long, timeSliceInDays: Int) {

  def toTimeSlice(time: Long): Int = {
    val timeDate = new DateTime(time)
    val durationSinceFirstMatch = new Duration(timeDate.getMillis() - startTime).getStandardDays() / timeSliceInDays

    val timeSlice = durationSinceFirstMatch.toInt

    timeSlice
  }
}