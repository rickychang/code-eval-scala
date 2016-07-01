import java.text.SimpleDateFormat
import java.time.Duration
import java.util.{Calendar, Date}

/**
  * Created by Ricky Chang on 7/1/16.
  */

object Main extends App {

  def parseDates(experience: String) = {
    val dateFormat = new SimpleDateFormat("MMM yyyy")
    val (start, end) = experience.split("-") match {
      case Array(s, e) =>
        val end = dateFormat.parse(e)
        val cal = Calendar.getInstance()
        cal.setTime(end)
        cal.add(Calendar.MONTH, 1)
        (dateFormat.parse(s), cal.getTime)
    }
    (start, end)
  }

  def getDaysBetweenDates(d1: Date, d2: Date): Long = {
    Duration.between(d1.toInstant, d2.toInstant).toDays
  }

  def getMaxDate(d1: Date, d2: Date): Date = {
    if (d1.after(d2)) d1 else d2
  }

  def mergeDatePairs(datePairs: Seq[(Date, Date)]): Seq[(Date, Date)] = {
    datePairs.foldLeft(List[(Date, Date)]()) { (mergedList, range) => mergedList match {
      case Nil => List(range)
      case head::tail => if (head._2.after(range._1)) {
        (head._1, getMaxDate(head._2, range._2)) :: tail
      } else {
        range :: mergedList
      }
    }}.reverse

  }

  def computeYears(experiences: Seq[String]) = {
    val datePairs = experiences.map(e => parseDates(e)).sortBy(_._1)
//    println(datePairs)
    val mergedDatePairs = mergeDatePairs(datePairs)
//    println(mergedDatePairs)
    val dayDurations = mergedDatePairs.map(e => getDaysBetweenDates(e._1, e._2))
//    println(dayDurations)
//    println(dayDurations.sum)
    dayDurations.sum / 365
  }

  val source = scala.io.Source.fromFile(args(0))
  val lines = source.getLines.filter(_.length > 0)
  for (l <- lines) {
//    println(l.split(";").map(_.trim))
    println(computeYears(l.split(";").map(_.trim)))
  }
}

