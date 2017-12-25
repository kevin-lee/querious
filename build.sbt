
//scalaVersion := "2.12.1"
scalaVersion := "2.11.11"

name := "sql-parser-scala"
organization := "io.kevinlee"
version := "0.0.1"

scalacOptions += "-feature"

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.13.4" % Test,
  "org.scalatest"  %% "scalatest"  % "3.0.1"  % Test,
//  "org.scalacheck" %% "scalacheck" % "1.12.6" % Test,
//  "org.scalatest"  %% "scalatest"  % "2.2.6"  % Test,
  "com.lihaoyi"    %% "fastparse"  % "1.0.0"
)
