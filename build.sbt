ThisBuild / organization := "com.github.simy4"
ThisBuild / version := "0.1.0"
ThisBuild / scalaVersion := "3.0.0-M3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" % "scala-parallel-collections_2.13" % "1.0.0", // TDDO make me resolve suffix automatically
      "org.scalacheck"         %% "scalacheck" % "1.15.2" % Test
    ),
    scalacOptions ++= Seq(
      "-encoding",
      "UTF-8",
      "-feature",
      "-unchecked",
      "-Xignore-scala2-macros",
      "-deprecation"
    )
  )

initialCommands in console := "import aoc.y2015.Day19._"

addCommandAlias("fmt", "; compile:scalafmt; test:scalafmt; scalafmtSbt")
