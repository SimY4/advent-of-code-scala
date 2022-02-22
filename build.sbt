ThisBuild / organization := "com.github.simy4"
ThisBuild / version      := "0.1.0"
ThisBuild / scalaVersion := "3.1.1"
Global / cancelable      := true

lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
      "org.scalacheck"         %% "scalacheck"                 % "1.15.4" % Test
    ),
    scalacOptions ++= Seq(
      "-release", 
      "11",
      "-encoding",
      "UTF-8",
      "-explain",
      "-explain-types",
      "-feature",
      "-unchecked",
      "-deprecation",
      "-rewrite",
      "-new-syntax",
      "-source:future-migration"
    )
  )

console / initialCommands := "import aoc.y2020.Day20.*"

addCommandAlias("fmt", "; Compile / scalafmt; Test / scalafmt; scalafmtSbt")
