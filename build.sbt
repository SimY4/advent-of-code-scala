name := "advent-code"
version := "0.1"
scalaVersion := "2.12.7"

libraryDependencies ++= Seq(
  "org.parboiled" %% "parboiled" % "2.1.5"
)

scalacOptions ++= Seq(
  "-encoding",
  "UTF-8",
  "-feature",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-unchecked",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture",
  "-Yno-adapted-args",
  "-deprecation"
)

initialCommands in console := "import AdventOfCode.y2018.Day10._; val i = scala.io.Source.fromFile(\"src/main/scala/input.text\").getLines.mkString(\"\\n\");"

addCommandAlias("fmt", "; compile:scalafmt; test:scalafmt; scalafmtSbt")
