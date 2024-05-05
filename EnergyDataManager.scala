import java.io.{File, PrintWriter} // Import File and PrintWriter for file operations
import java.text.SimpleDateFormat // Import SimpleDateFormat for formatting dates
import java.util.Scanner // Import Scanner for reading input from the user
import scala.io.Source // Import Source to read from files
import scala.util.{Try, Success, Failure} // Import Try, Success, and Failure for handling exceptions

// Define a class to manage energy data from different sources
class EnergyDataManager(hydroFilename: String, windFilename: String, solarFilename: String) {
  val dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'") // Define a date format matching the expected input

  // Method to get the date range of data in a file
  def getDateRange(filename: String): (Option[String], Option[String]) = {
    println(s"Attempting to open file at: $filename") // Debug output to indicate file processing
    val file = new File(filename)
    if (!file.exists()) {
      println(s"File does not exist: $filename")
      return (None, None) // Return None if file doesn't exist
    }
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
    val src = Source.fromFile(file)
    try {
      val lines = src.getLines().drop(1) // Skip the header line
      var dateCount = 0
      val dates = lines.flatMap { line =>
        println(s"Processing line: $line") // Debug output for each line processed
        val parts = line.trim.split(",")
        if (parts.length > 1) {
          val dateStr = parts(1) // Assume date is in the second column
          Try(dateFormat.parse(dateStr)) match {
            case Success(date) =>
              dateCount += 1
              Some(dateFormat.format(date)) // Return formatted date if parsing is successful
            case Failure(e) =>
              println(s"Failed to parse date: $dateStr with error: ${e.getMessage}") // Log parsing error
              None
          }
        } else None
      }.toList.distinct.sorted

      println(s"Total valid dates parsed: $dateCount") // Debug output for total dates parsed

      if (dates.isEmpty) {
        println("No valid dates found.") // Log if no valid dates were found
        (None, None)
      } else (Some(dates.head), Some(dates.last)) // Return the earliest and latest date
    } finally {
      src.close() // Ensure file is always closed after processing
    }
  }

  // Method to find data by type based on timestamp
  def findDataByType(filename: String, timestamp: Long): Option[Double] = {
    withFileSource(filename) { lines =>
      lines.map { line =>
        val parts = line.trim.split(",").map(_.trim.replaceAll("\"", "")) // Clean up the line and remove quotes
        parts
      }.find { parts =>
        parts.length > 2 && Try(dateFormat.parse(parts(1)).getTime == timestamp).getOrElse(false) // Check if timestamp matches
      }.flatMap { matchedParts =>
        Try(matchedParts(3).toDouble).toOption // Return energy value as Double if present
      }
    }
  }

  // Helper method to manage file resource
  private def withFileSource[T](filename: String)(f: Iterator[String] => T): T = {
    val src = Source.fromFile(filename)
    try {
      f(src.getLines()) // Process lines from file
    } finally {
      src.close() // Ensure the file is always closed
    }
  }

  // Method to collect and store data from all sources
  def collectAndStoreData(): Unit = {
    val outputFile = new File("collected_data.txt")
    val pw = new PrintWriter(outputFile)
    try {
      pw.println("Collected data on renewable energy sources:")
      // Process each file
      val fileNames = List(hydroFilename, windFilename, solarFilename)
      fileNames.foreach { file =>
        val source = Source.fromFile(file)
        try {
          val lines = source.getLines().drop(1) // Assume first line is the header
          lines.foreach(line => {
            val data = line.split(",").map(_.trim)
            val datasetid = data(0)
            val time = data(1)  // Assume time is the second column
            val time2 = data(2)
            val energy = data(3) // Assume energy data is in the third column
            pw.println(s"Datasetid: $datasetid, Time: $time-$time2 Energy: $energy kWh from $file")
          })
        } finally {
          source.close() // Ensure the file is always closed
        }
      }
    } finally {
      pw.close()
    }
    println("Data has been collected and stored in " + outputFile.getPath)
  }

  // Method to analyze energy data from a file
  def analyzeData(filename: String): Unit = {
    val source = Source.fromFile(filename)
    try {
      val data = source.getLines().drop(1) // Assume first line is the header
        .map(_.split(",").last.trim.replaceAll("\"", "")) // Remove quotes and trim spaces
        .filter(_.matches("-?\\d+(\\.\\d+)?")) // Ensure it is a numeric value
        .map(_.toDouble) // Convert to Double safely
        .toList

      if (data.isEmpty) {
        println("No data available for analysis.")
        return
      }

      val mean = data.sum / data.size
      val sorted = data.sorted
      val median = if (sorted.size % 2 == 1) sorted(sorted.size / 2) else (sorted(sorted.size / 2 - 1) + sorted(sorted.size / 2)) / 2.0
      val mode = data.groupBy(identity).maxBy(_._2.size)._1
      val range = sorted.last - sorted.head
      val midrange = (sorted.head + sorted.last) / 2.0

      println(s"Mean: $mean")
      println(s"Median: $median")
      println(s"Mode: $mode")
      println(s"Range: $range")
      println(s"Midrange: $midrange")
    } finally {
      source.close() // Ensure the file is always closed
    }
  }

  // Method to fetch data within a specified range
  def fetchDataInRange(filename: String, startTime: String, endTime: String): List[Array[String]] = {
    val src = Source.fromFile(filename)
    try {
      val data = src.getLines().drop(1).map(_.split(",")).filter { parts =>
        val timestamp = dateFormat.parse(parts(1)).getTime
        timestamp >= dateFormat.parse(startTime).getTime && timestamp <= dateFormat.parse(endTime).getTime
      }.toList
      data
    } finally {
      src.close() // Ensure the file is always closed
    }
  }

  // Method to print sorted or unsorted data
  def printData(data: List[Array[String]], shouldSort: Boolean): Unit = {
    val sortedData = if (shouldSort) {
      data.sortBy(parts => parts.last.toDouble) // Assume last column can be converted to Double
    } else data

    sortedData.foreach { parts =>
      println(parts.mkString(", "))
    }
  }

}
