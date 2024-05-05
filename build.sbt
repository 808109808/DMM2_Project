ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.14"

lazy val root = (project in file("."))
  .settings(
    name := "project_test3",
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.client3" %% "core" % "3.5.2",  // sttp核心库，用于HTTP请求
      "com.lihaoyi" %% "os-lib" % "0.8.0",                  // os-lib库，用于文件操作
      "com.github.tototoshi" %% "scala-csv" % "1.3.10"      // scala-csv库，用于处理CSV文件
    )
  )
libraryDependencies += "com.typesafe.play" %% "play-json" % "2.9.2"
libraryDependencies += "org.jfree" % "jfreechart" % "1.5.3"
