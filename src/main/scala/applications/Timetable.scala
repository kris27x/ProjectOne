package applications

import lib.picture.Picture
import scala.io.Source

class Timetable(
                 firstDay: Int = 1, // 1 is Monday
                 lastDay: Int = 5, // 5 is Friday
                 firstTime: Int = 9, // 09:00
                 lastTime: Int = 17, // 17:00
                 slotWidth: Int = 9, // Minimum column width (< 9 will truncate headers)
                 alsoBeside: Boolean = false // Show clashes vertically (true) horizontally (false)
               ) {
  private val nbrOfDays = lastDay - firstDay + 1
  private val days = (firstDay to lastDay).toList
  private val dayNames = List("MONDAY", "TUESDAY", "WEDNESDAY", "THURSDAY", "FRIDAY", "SATURDAY", "SUNDAY")
  private val dayLabels = dayNames.slice((firstDay - 1), (firstDay - 1) + nbrOfDays)
  private val header = dayLabels map (Picture(_) fixWidth slotWidth)
  private val times = (firstTime to lastTime).toList
  private val timeLabels = "Time " :: (times map (t => s"$t:00"))
  private val timeWidth = 5
  private val margin = timeLabels map (Picture(_) fixWidth(timeWidth))

  // A slot is a day and a time
  private case class Slot(day: Int, time: Int)

  // slots represents all the day/time combinations that are required in the timetable
  private val slots = for (d <- days; t <- times) yield Slot(d, t)

  // alsoP is a picture that is used to separate events that occur on the same day at
  // the same time.
  private val alsoP: Picture =
    if (alsoBeside)
      Picture("ALSO").border(' ')
    else
      Picture("ALSO").borderT(' ').borderB(' ').fixWidth(slotWidth)

  // A method that combines two pictures by placing the alsoP picture between them.
  private def also(p1: Picture, p2: Picture): Picture =
    if (alsoBeside)
      p1 + alsoP + p2
    else
      p1 ^ alsoP ^ p2

  // An activity (to be presented) is a (day/time) slot together with the
  // now-formatted picture representing the description of that activity.
  private case class Activity(slot: Slot, picture: Picture)

  // A method to process each line of input from the external text file.
  private def parseLine(line: String): Activity = {
    val day: Int = line.take(1).toInt
    val hour: Int = line.slice(2, 4).toInt
    val xs = line.drop(5)
    Activity(Slot(day, hour), Picture(xs, ' ').fixWidth(slotWidth))
  }

  // The method that builds the timetable as a picture.
  def makeTimetable(dataFilename: String): Picture = {

    /*
     First of all, read the data from the text file
     */
    val fileHandle = Source.fromFile(dataFilename)
    val lines: Seq[String] = fileHandle.getLines.toList
    fileHandle.close()

    /*
     * Parse each line from the text file and build a sequence of activities.
     */
    val activities: Seq[Activity] = lines map parseLine

    /*
     * Gather all the activities that share the same slot
     */
    val groupedActivities: Map[Slot, Seq[Activity]] = activities.groupBy(_.slot)

    /*
     * Extract the pictures from the slots. The key of the map IS the slot so
     * there is no need to keep this information within the values as well
     */
    val groupedActivityPictures: Map[Slot, Seq[Picture]] =
      groupedActivities map ((slot, acts) => (slot, acts map (_.picture)))

    /*
     * Reduce the sequence of pictures within each slot to a single picture. If
     * there are two or more pictures in a slot then these need to be combined
     * with the alsoP separator.
     */
    val groupedAndJoinedActivityPictures: Map[Slot, Picture] =
      groupedActivityPictures map ((slot, acts) => (slot, acts reduceLeft also))

    /*
     * Create the array of pictures needed to create the timetable. This requires
     * iterating through the days/times.  The result is a sequence of rows where
     * each row contains the information that takes place at the given time.
     * If there is no activity at a given day/time then a blank picture is
     * constructed (Picture(' ')) because there must be a picture in every cell.
     */
    val tableData: Seq[Seq[Picture]] =
      for time <- times
        yield
          for day <- days
            yield
              groupedAndJoinedActivityPictures.getOrElse(Slot(day, time), Picture(' '))
    /*
     * Augment the table data by adding the header informatoin to the top (the header
     * contains the day names) and the margin to the sides (the margin contains the
     * hours). This gives a completed table of pictures representing the timetable.
     */
    val timetable: Seq[Seq[Picture]] = margin zip (header +: tableData) map (_ +: _)
    timetable.formatAsTable()
  }
}

object Timetable {
  @main def runTT(): Unit = {
    /*
     * Create a timetable instance configured with the required presentation options
     */
    val tt1 = new Timetable(slotWidth = 14, alsoBeside = false, lastDay = 5)
    val tt2 = new Timetable(slotWidth = 14, alsoBeside = true)

    /*
     * Use the timetable instance to build a Picture representation and display it.
     * Try using tt2 instead and see the difference.
     * Try some other Timetable instances with the same input data.
     */
    println(tt1.makeTimetable("dat/tt-input.txt"))
  }
}