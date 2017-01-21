name := "plug"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "joda-time"                    % "joda-time"                    % "2.9.7",
  "org.joda"                     % "joda-convert"                 % "1.8.1",
  "org.scala-lang.modules"      %% "scala-parser-combinators"     % "1.0.5",
  "org.scalatest"               %% "scalatest"                    % "2.2.1"     % "test"
)
