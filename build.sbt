import sbtwelcome.*

ThisBuild / organization := "com.github.simy4"
ThisBuild / version      := "0.1.0"
ThisBuild / scalaVersion := "3.6.4"
Global / cancelable      := true

lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0",
      "org.scalameta"          %% "munit"                      % "1.1.1" % Test
    ),
    scalacOptions ++= Seq(
      "-release",
      "21",
      "-encoding",
      "UTF-8",
      "-explain",
      "-explain-types",
      "-feature",
      "-unchecked",
      "-deprecation",
      "-source:future-migration",
      "-language:experimental.betterFors"
    )
  )

addCommandAlias("fmt", ";scalafmtAll;scalafmtSbt")

logo :=
  raw"""           _                 _            __                 _                      _
       |  __ _  __| |_   _____ _ __ | |_    ___  / _|   ___ ___   __| | ___   ___  ___ __ _| | __ _
       | / _` |/ _` \ \ / / _ \ '_ \| __|  / _ \| |_   / __/ _ \ / _` |/ _ \ / __|/ __/ _` | |/ _` |
       || (_| | (_| |\ V /  __/ | | | |_  | (_) |  _| | (_| (_) | (_| |  __/ \__ \ (_| (_| | | (_| |
       | \__,_|\__,_| \_/ \___|_| |_|\__|  \___/|_|    \___\___/ \__,_|\___| |___/\___\__,_|_|\__,_|
       |
       |""".stripMargin

usefulTasks := Seq(
  UsefulTask("~compile", "Compile with file-watch enabled"),
  UsefulTask("~console", "Run REPL with file-watch enabled"),
  UsefulTask("fmt", "Run scalafmt on the entire project")
)

logoColor := scala.Console.MAGENTA
