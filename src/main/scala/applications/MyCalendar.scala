package applications

import lib.picture.Picture
import java.time.{YearMonth, LocalDate, DayOfWeek}
import java.time.format.TextStyle
import java.util.Locale
import lib.picture.Picture._
import scala.collection.mutable.ListBuffer

object MyCalendar {

  /*
  A calendar stores, and displays, events that are scheduled on specific
  days. Each event has a date, a time, and a description.
  */

  case class Date(year: Int, month: Int, day: Int)

  case class Time(hour: Int, minute: Int) {
    def toPicture: Picture = Picture(f"$hour%02d:$minute%02d")
  }

  case class Event(date: Date, time: Time, desc: String)

  // Mutable ListBuffer to store events
  private val events: ListBuffer[Event] = ListBuffer.empty

  /**
   * The columns of the calendar are set to a given width.
   */
  val EventWidth: Int = 25

  /**
   * The place where the events are stored.
   */
  val EventsPath: String = "dat/events.txt"

  /**
   * Returns the number of days in a month. Deals with leap years.
   *
   * @return The number of days in a given month for a given year.
   */
  def daysInMonth(year: Int, month: Int): Int = YearMonth.of(year, month).lengthOfMonth()

  /**
   * The names of the months are stored in a Map relating month numbers (1..) to names.
   * To get the picture representing "JANUARY", write nameOfMonth(1)
   */
  val nameOfMonth: Map[Int, Picture] = Map(
    1 -> Picture("JANUARY"),
    2 -> Picture("FEBRUARY"),
    3 -> Picture("MARCH"),
    4 -> Picture("APRIL"),
    5 -> Picture("MAY"),
    6 -> Picture("JUNE"),
    7 -> Picture("JULY"),
    8 -> Picture("AUGUST"),
    9 -> Picture("SEPTEMBER"),
    10 -> Picture("OCTOBER"),
    11 -> Picture("NOVEMBER"),
    12 -> Picture("DECEMBER")
  )

  /**
   * Days of the week are numbered
   * 0 = Monday
   * 1 = Tuesday
   * 2 = Wednesday
   * 3 = Thursday
   * 4 = Friday
   * 5 = Saturday
   * 6 = Sunday
   *
   * Uses an algorithm published by Tomohiko Sakamoto in 1993
   * See http://www.faqs.org/faqs/sci-math-faq/dayWeek/
   * The algorithm assumes the first day (0) is Sunday. This
   * method adjusts the result so that the first day (0) is
   * Monday.
   *
   * @return the day of the week for a given year/month/day
   *         where 0=Monday, 1=Tuesday,..., 6=Sunday
   */
  def getDayOfWeek(year: Int, month: Int, day: Int): Int = {
    val t = List(0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4)
    val d = day
    val m = month
    val y = if (month < 3) year - 1 else year
    val answerSundayEq0 = (y + y / 4 - y / 100 + y / 400 + t(m - 1) + d) % 7
    (answerSundayEq0 + 6) % 7
  }

  /**
   * The names of the days are stored as a sequence of pictures. To ensure that all the
   * headers in the calendar are the same width, the day names are fixed to the given
   * constant EVENT_WIDTH.
   */
  val namesOfDays: Seq[Picture] =
    Seq("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
      .map(Picture(_).fixWidth(EventWidth))

  /**
   * Simple method to read the events from a text file. Absolutely no validation is
   * included. The text file is assumed to be formatted correctly. It reads events
   * by line in the format:
   * year month day hour minute description
   * E.g.
   * 2023 2  22  09  00  CTEC3904 lecture in MS1.02
   *
   * Blank lines are ignored. Multiple spaces are ignored.
   *
   * @param filename The pathname of the file of event data. In IntelliJ this is relative
   *                 to the project root. Thus, "/dat/events.txt" would refer to
   * {{{
   *                 v ProjectName
   *                     > dat
   *                         events.txt
   * }}}
   * @return A sequence of events.
   */
  def readEventsFromFile(filename: String): Seq[Event] = {
    import java.io.File
    import scala.io.{BufferedSource, Source}
    val fileHandle: BufferedSource = Source.fromFile(filename)
    val lines: Seq[String] = fileHandle.getLines().toList
    fileHandle.close()
    lines.filter(_.nonEmpty).map { line =>
      val (datetime, description) = line.split("\\s+").toList.splitAt(5)
      val (List(year, month, day), List(hour, minute)) = datetime.map(_.toInt).splitAt(3)
      Event(Date(year, month, day), Time(hour, minute), description.mkString(" "))
    }
  }

  /**
   * A method to make up random diary events. Useful for testing.
   * N.B. It over-writes the file at EventsPath.
   */
  def makeUpEvents(numberOfEvents: Int, filename: String): Unit = {
    import java.io.{BufferedWriter, File, FileWriter}
    val file = new File(filename)
    val fileHandle = new BufferedWriter(new FileWriter(file))
    val r = scala.util.Random
    for (_ <- 1 to numberOfEvents) {
      val yy = r.nextInt(2) + 2023 // Uses dates in 2023 and 2024
      val mm = r.nextInt(12) + 1
      val dd = YearMonth.of(yy, mm).lengthOfMonth()
      val hrs = r.nextInt(24)
      val mins = if (r.nextInt(2) == 0) 0 else 30
      val desc = f"Event-auto-gen ($yy/$mm/$dd @ $hrs%02d:$mins%02d)"
      fileHandle.write(f"$yy%4d $mm%4d $dd%4d $hrs%02d $mins%02d $desc\n")
    }
    fileHandle.close()
  }

  /** THE COURSEWORK METHOD
   * **********************************************************************************
   * Produces a picture (ready for display on the output console) of the given month in
   * the calendar. The events are filtered so that only those relevant for the given
   * month are displayed.
   * ***********************************************************************************
   *
   * @param year   The calendar year. (e.g. 2023)
   * @param month  The calendar month (1 = JANUARY, etc.)
   * @param events The sequence of (all) events. It will need to be filtered to obtain
   *               those events relevant for this month and this year.
   * @return A nicely formatted calendar page that contains a view of the given
   *         month in the given year.
   */
  // Create a list of day names for each day of the week
  private val dayNames = List("MONDAY", "TUESDAY", "WEDNESDAY", "THURSDAY", "FRIDAY", "SATURDAY", "SUNDAY")
  // Create a list of abbreviated day labels by taking the first 3 characters of each day name
  private val dayLabels = dayNames.map(_.substring(0, 3))
  // Define the width for each day slot in the calendar
  private val slotWidth = 40
  // Create a list of Picture objects for each day label with a fixed width
  private val header = dayLabels.map(Picture(_).fixWidth(slotWidth))

  // Method to add events
  def addEvent(year: Int, month: Int, day: Int, hour: Int, minute: Int, desc: String): Unit = {
    events += Event(Date(year, month, day), Time(hour, minute), desc)
  }

  def displayMonth(year: Int, month: Int): Unit = {
    // Create an object for the given year and month
    val yearMonth = YearMonth.of(year, month)
    // Calculate the number of days
    val daysInMonth = yearMonth.lengthOfMonth()
    // Get the first day of the week for the first day of the month
    val firstDayOfWeek = yearMonth.atDay(1).getDayOfWeek.getValue

    // Create a list of blank days to align the first day of the month with the correct day of the week
    val blankDays = List.fill(firstDayOfWeek - 1)(Picture(" ").fixWidth(slotWidth))
    // Create a list of Picture objects for each day of the month
    val monthDays = (1 to daysInMonth).map { day =>
      // Filter events for the current day and sort them by time
      val eventTitles = events.filter(e => e.date.year == year && e.date.month == month && e.date.day == day)
        .sortBy(e => (e.time.hour, e.time.minute)) // Sort events by hour and minute
        .map(e => s"${e.time.toPicture} ${e.desc}")
        .mkString("\n")
      // Create a string with the day number and events
      val dayWithEvents = if (eventTitles.nonEmpty) s"$day\n$eventTitles" else day.toString
      // Create a Picture object for the current day with a fixed width
      Picture(dayWithEvents).fixWidth(slotWidth)
    }

    // Combine the blank days with the month days
    val daysWithBlanks = blankDays ++ monthDays
    // Split the days into calendar rows
    val calendarRows = daysWithBlanks.grouped(7).toList

    // Create the calendar timetable with the header and calendar rows
    val timetable: Seq[Seq[Picture]] = header +: calendarRows.map(row => row.padTo(7, Picture(" ").fixWidth(slotWidth)))
    // Format the timetable as a table
    val calendar = timetable.formatAsTable()

    // Get the month name and create the year and month header string
    val monthName = nameOfMonth(month)
    val yearAndMonthHeader = s"$monthName $year"

    // Function to center the header string within the total available width
    def centerHeader(header: String, totalWidth: Int): String = {
      val padding = (totalWidth - header.length) / 2
      " " * padding + header + " " * padding
    }

    // Center the year and month header
    val centeredHeader = centerHeader(yearAndMonthHeader, slotWidth * 7)

    // Print the centered header and the calendar
    println(centeredHeader + "\n" + calendar)
  }

  /**
   * A method to create a set of random events and write them to the
   * default text file, EventsPath. To change the number of events
   * simply adjust the number in the parameter list.
   */
  @main def constructRandomEventFile(): Unit = makeUpEvents(1000, EventsPath)

  @main def coursework(): Unit = {
    // Read the events from the file
    val eventsFromFile = readEventsFromFile(EventsPath)

    // Add the events to the calendar
    eventsFromFile.foreach { event =>
      addEvent(event.date.year, event.date.month, event.date.day, event.time.hour, event.time.minute, event.desc)
    }

    // Display the current month
    val now = LocalDate.now()
    displayMonth(now.getYear, now.getMonthValue)
  }
}
