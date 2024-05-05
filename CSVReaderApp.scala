import java.util.Scanner // Import Scanner class for reading user input
import scala.util.{Failure, Success, Try} // Import Try, Success, and Failure for handling exceptions gracefully
import java.net.{HttpURLConnection, URL} // Import HttpURLConnection and URL for HTTP requests
import java.io.{File, PrintWriter} // Import File and PrintWriter for file operations
import scala.io.Source // Import Source for reading files and streams
import play.api.libs.json._ // Import Play's JSON library for JSON handling
import java.time.ZonedDateTime // Import ZonedDateTime for handling timezone-specific dates and times
import java.time.format.DateTimeFormatter // Import DateTimeFormatter for formatting and parsing date-time objects
import java.time.ZoneId // Import ZoneId to represent a time zone identifier
import java.time.format.DateTimeParseException // Import DateTimeParseException for errors during parsing dates and times
import java.time.Instant // Import Instant for an instantaneous point on the time-line

// Define an object to store the constant file path
object GlobalPathway{
  val globalPathway: String = "E:\\LUT\\fourth period\\DMM2\\project\\Project Documents-20240502\\project\\"
}

// Main application for CSV reading
object CSVReaderApp extends App {

  // Initialize user input scanner and define constants
  val scanner = new Scanner(System.in)
  val apiKey = "4d6d23aded204adcbfc6db5a456a6c62"
  val datasetIds = Map(181 -> "wind3min.csv", 188 -> "solar3min.csv", 191 -> "hydro3min.csv")

  // Define file paths using a global pathway
  val hydroFilename = GlobalPathway+"hydro3min.csv"
  val windFilename = GlobalPathway+"wind3min.csv"
  val solarFilename = GlobalPathway+"solar3min.csv"
  val dataManager = new EnergyDataManager(hydroFilename, windFilename, solarFilename)

  // Method to handle data queries
  def handleQuery(dataType: String, filename: String): Unit = {
    println(s"\n$dataType Data Query")
    val range = dataManager.getDateRange(filename)
    println(s"Data available from ${range._1.getOrElse("unknown")} to ${range._2.getOrElse("unknown")}")
    println("Enter the date and time in the format yyyy-MM-dd'T'HH:mm:ss.SSS'Z':")
    val scanner = new Scanner(System.in)
    val userInput = scanner.nextLine()
    Try(dataManager.dateFormat.parse(userInput).getTime) match {
      case Success(timestamp) =>
        dataManager.findDataByType(filename, timestamp) match {
          case Some(value) => println(s"$dataType energy at specified time: $value")
          case None => println(s"No $dataType data found at specified time.")
        }
      case Failure(_) =>
        println("Invalid date format. Please use the format yyyy-MM-dd'T'HH:mm:ss.SSS'Z'.")
    }
  }

  // Display the main menu and prompt for user choice
  def mainMenu(): Unit = {
    println("\nSelect the type of data to query or action to perform:")
    println("1. Solar Data")
    println("2. Hydro Data")
    println("3. Wind Data")
    println("4. Collect and Store Data")
    println("5. Analyze Solar Data")
    println("6. Analyze Hydro Data")
    println("7. Analyze Wind Data")
    println("8. Fetch Solar Data for a Range and Sort")
    println("9. Fetch Hydro Data for a Range and Sort")
    println("10. Fetch Wind Data for a Range and Sort")
    println("11. Generate Pie Chart of Total Energy")
    println("12. Generate Pie Chart of Total Energy")
    println("0. Exit")
    println("Enter your choice (0-12):")
  }

  // Method to handle data collection
  def handleDataCollection(): Unit = {
    println("\nCollecting and storing energy data...")
    dataManager.collectAndStoreData()
  }
  // Method to analyze data based on type
  def handleAnalysis(dataType: String, filename: String): Unit = {
    println(s"\nAnalyzing $dataType Data...")
    dataManager.analyzeData(filename)
  }

  // Method to handle range queries for data
  def handleRangeQuery(dataType: String, filename: String): Unit = {
    println(s"\nEnter the start time (yyyy-MM-dd'T'HH:mm:ss.SSS'Z'):")
    val startTime = scanner.nextLine()
    println("Enter the end time (yyyy-MM-dd'T'HH:mm:ss.SSS'Z'):")
    val endTime = scanner.nextLine()
    val data = dataManager.fetchDataInRange(filename, startTime, endTime)
    println(s"\nDo you want to sort the data by the value. (Y/N)?")
    val shouldSort = scanner.nextLine().toUpperCase
    dataManager.printData(data, shouldSort == "Y")
  }

//  def handleRangeQuery(dataType: String, filename: String): Unit = {
//    println(s"\nQuerying range for $dataType Data...")
//    val startTimePrompt = "Enter the start time (yyyy-MM-dd'T'HH:mm:ss.SSS'Z') or enter 0 to cancel:"
//    val endTimePrompt = "Enter the end time (yyyy-MM-dd'T'HH:mm:ss.SSS'Z') or enter 0 to cancel:"
//
//    val startTime = getUserInputTime(startTimePrompt)
//    val endTime = getUserInputTime(endTimePrompt)
//
//    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'").withZone(ZoneId.of("UTC"))
//
//    (startTime, endTime) match {
//      case (Some(start), Some(end)) =>
//        val formattedStart = formatter.format(Instant.ofEpochMilli(start))
//        val formattedEnd = formatter.format(Instant.ofEpochMilli(end))
//        val data = dataManager.fetchDataInRange(filename, formattedStart, formattedEnd)
//        println(s"\nDo you want to sort the data by the value. (Y/N)?")
//        val shouldSort = scanner.nextLine().toUpperCase
//        dataManager.printData(data, shouldSort == "Y")
//      case _ =>
//        println("Data query cancelled or invalid input.")
//    }
//  }


  // Method to manage file resources using a functional approach
  def withFileSource[T](filename: String)(f: Iterator[String] => T): T = {
    val src = Source.fromFile(filename)
    try {
      f(src.getLines())
    } finally {
      src.close()
    }
  }

  // Display all collected energy data
  def showAllData(): Unit = {
    val fileNames = List(hydroFilename, windFilename, solarFilename)
    var totalEnergy = 0.0
    val energies = scala.collection.mutable.Map[String, Double]()

    fileNames.foreach { filename =>
      withFileSource(filename) { lines =>
        val header = lines.next()
        val startTimeIndex = header.split(",").indexWhere(_.trim == "startTime")
        val endTimeIndex = header.split(",").indexWhere(_.trim == "endTime")
        val valueIndex = header.split(",").indexWhere(_.trim == "value")
        val energyData = lines.map { line =>
          val parts = line.split(",")
          (parts(startTimeIndex), parts(endTimeIndex), parts(valueIndex).toDouble)
        }.toList

        val dates = energyData.map(_._1) ++ energyData.map(_._2)
        let minDate = dates.min
        let maxDate = dates.max
        let total = energyData.map(_._3).sum

        println(s"${filename.split("\\\\").last}:")
        println(s"Data range: $minDate to $maxDate")
        println(s"Total Energy: $total kWh")

        energies(filename.split("\\\\").last) = total
        totalEnergy += total
      }
    }

    println(s"\nTotal Energy from all sources: $totalEnergy kWh")
    energies.foreach { case (source, energy) =>
      val percentage = (energy / totalEnergy) * 100
      println(s"Percentage of $source: $percentage%")
    }
  }


  // Check for faults in the energy data
  def findFaults(): Unit = {
    val fileNames = Map("Hydro" -> hydroFilename, "Wind" -> windFilename, "Solar" -> solarFilename)
    val dateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSX").withZone(ZoneId.of("UTC"))
    var faultFound = false

    fileNames.foreach { case (energyType, filename) =>
      withFileSource(filename) { lines =>
        let header = lines.next()
        let startTimeIndex = header.split(",").indexWhere(_.trim == "startTime")
        let valueIndex = header.split(",").indexWhere(_.trim == "value")
        let values = lines.map { line =>
          let parts = line.split(",")
          (ZonedDateTime.parse(parts(startTimeIndex), dateTimeFormatter), parts(valueIndex).toDouble)
        }.toList

        // Filtering out night time data for Solar energy only
        let filteredValues = if (energyType == "Solar") {
          values.filter { case (date, _) =>
            let hour = date.getHour
            !(hour >= 20 || hour < 8) // Exclude 20:00 to 08:00 for solar data
          }
        } else values

        let totalCount = filteredValues.length
        let lowValuesCount = filteredValues.count(_._2 < 100)
        let veryLowValuesCount = filteredValues.count(_._2 < 500)

        // Checking for faults based on thresholds
        if (lowValuesCount.toDouble / totalCount >= 0.2) {
          println(s"$energyType energy may have data faults. $lowValuesCount out of $totalCount readings are below 100.")
          faultFound = true
        }
        if (veryLowValuesCount.toDouble / totalCount >= 0.5) {
          println(s"$energyType energy may have low energy output. $veryLowValuesCount out of $totalCount readings are below 500.")
          faultFound = true
        }
      }
    }

    if (!faultFound) {
      println("No faults found.")
    }
  }


  // Helper method to get user input with validation
  def getUserInputTime(prompt: String): Option[Long] = {
    var continue = true
    var timestamp: Option[Long] = None

    while (continue) {
      println(prompt)
      val userInput = scanner.nextLine()
      if (userInput == "0") {
        continue = false // Exit if user inputs '0'
      } else {
        Try(dataManager.dateFormat.parse(userInput).getTime) match {
          case Success(time) =>
            timestamp = Some(time)
            continue = false // Exit loop if date format is correct
          case Failure(_) =>
            println("Invalid date format. Please use the format yyyy-MM-dd'T'HH:mm:ss.SSS'Z' or enter 0 to cancel.")
        }
      }
    }

    timestamp
  }
  // Run the main logic of the application
  def run(): Unit = {


    var continue = true
    val scanner = new Scanner(System.in)

//    ---
    println("Enter the start time (yyyy-MM-dd'T'HH:mm:ss.SSS'Z'):")
    val startTimeString = scanner.nextLine()
    println("Enter the end time (yyyy-MM-dd'T'HH:mm:ss.SSS'Z'):")
    val endTimeString = scanner.nextLine()
//    ---

//    val startTimePrompt  = "Enter the start time (yyyy-MM-dd'T'HH:mm:ss.SSS'Z') or enter 0 to cancel:"
//    val endTimePrompt  = "Enter the end time (yyyy-MM-dd'T'HH:mm:ss.SSS'Z') or enter 0 to cancel:"
//
//    val startTimeString = getUserInputTime(startTimePrompt)
//    val endTimeString = getUserInputTime(endTimePrompt)


    //    ---
  def fetchData(url: String, apiKey: String): String = {
    val connection = new URL(url).openConnection().asInstanceOf[HttpURLConnection]
    connection.setRequestMethod("GET")
    connection.setRequestProperty("Accept", "application/json")
    connection.setRequestProperty("x-api-key", apiKey)

    val responseCode = connection.getResponseCode
    if (responseCode == HttpURLConnection.HTTP_OK) {
      val inputStream = connection.getInputStream
      val content = Source.fromInputStream(inputStream).mkString
      inputStream.close()
      content
    } else {
      throw new RuntimeException(s"HTTP error code: $responseCode")
    }
  }

    def extractCsvFromJson(jsonData: String): String = {
      val json = Json.parse(jsonData)
      (json \ "data").as[String] // Directly extract CSV data
    }

    def saveDataToFile(data: String, fileName: String): Unit = {
      val path = "E:\\LUT\\fourth period\\DMM2\\project\\Project Documents-20240502\\project\\" + fileName // 设定文件的存储路径
      val file = new File(path)
      val writer = new PrintWriter(file)
      writer.write(data)
      writer.close()
      println(s"Data saved to $path")
    }

    datasetIds.foreach { case (datasetId, fileName) =>
      val url = s"https://data.fingrid.fi/api/datasets/$datasetId/data?startTime=$startTimeString&endTime=$endTimeString&format=csv"
      try {
        println(s"Fetching data for dataset ID $datasetId")
        val rawData = fetchData(url, apiKey)
        println("Data fetched successfully:")
        val csvData = extractCsvFromJson(rawData)
        saveDataToFile(csvData, fileName)
      } catch {
        case e: Exception => println(s"An error occurred for dataset ID $datasetId: ${e.getMessage}")
      }
    }
//    ---

    while (continue) {
      mainMenu()
      val choice = scanner.nextLine()
      choice match {
        case "1" => handleQuery("Solar", solarFilename)
        case "2" => handleQuery("Hydro", hydroFilename)
        case "3" => handleQuery("Wind", windFilename)
        case "4" => handleDataCollection()
        case "5" => handleAnalysis("Solar", solarFilename)
        case "6" => handleAnalysis("Hydro", hydroFilename)
        case "7" => handleAnalysis("Wind", windFilename)
        case "8" => handleRangeQuery("Solar", solarFilename)
        case "9" => handleRangeQuery("Hydro", hydroFilename)
        case "10" => handleRangeQuery("Wind", windFilename)
        case "11" => showAllData()
        case "12" => findFaults()
        case "0" => continue = false
        case _ => println("Invalid choice. Please enter a number between 0 and 12.")
      }
    }
    println("Exiting the program.")
  }

  run()// Start the program
}

