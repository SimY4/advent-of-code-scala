ThisBuild / organization := "com.github.simy4"
ThisBuild / version      := "0.1.0"
ThisBuild / scalaVersion := "3.2.0"
Global / cancelable      := true

lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
      "org.scalameta"          %% "munit"                      % "0.7.29" % Test
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

console / initialCommands := "import aoc.y2016.Day23.*"

testFrameworks += new TestFramework("munit.Framework")

addCommandAlias("fmt", ";scalafmtAll;scalafmtSbt")
