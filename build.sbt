ThisBuild / organization := "com.github.simy4"
ThisBuild / version := "0.1.0"
ThisBuild / scalaVersion := "0.19.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0",
      "org.parboiled"          %% "parboiled"  % "2.1.8",
      "org.scalacheck"         %% "scalacheck" % "1.14.2" % Test
    ).map(_.withDottyCompat(scalaVersion.value)),
    scalacOptions ++= Seq(
      "-encoding",
      "UTF-8",
      "-feature",
      "-unchecked",
      "-Xignore-scala2-macros",
      "-deprecation"
    )
  )

initialCommands in console := "import aoc.y2019.Day1._"

addCommandAlias("fmt", "; compile:scalafmt; test:scalafmt; scalafmtSbt")
