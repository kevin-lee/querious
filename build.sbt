
ThisBuild / scalaVersion := "2.12.13"

ThisBuild / name := "sql-parser-scala"
ThisBuild / organization := "io.kevinlee"
ThisBuild / version := "0.0.1"

ThisBuild / crossScalaVersions := List("2.11.12", "2.12.13")

val hedgehogVersion = "0.7.0"

val sqlParserScala = (project in file("."))
  .settings(
    scalacOptions ++= Seq(
//      "-feature",
//      "-language:higherKinds"
    ),
//
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full),
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
    reporterConfig := reporterConfig
      .value
      .withColumnNumbers(true)
      .withSourcePathColor(scala.Console.MAGENTA + scala.Console.UNDERLINED),
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.13.4" % Test,
      "org.scalatest"  %% "scalatest"  % "3.0.1"  % Test,
    //  "org.scalacheck" %% "scalacheck" % "1.12.6" % Test,
    //  "org.scalatest"  %% "scalatest"  % "2.2.6"  % Test,
      "io.estatico" %% "newtype" % "0.4.4",
      "eu.timepit" %% "refined" % "0.9.12",
      "com.lihaoyi"    %% "fastparse"  % "1.0.0"
    ) ++ Seq(
      "qa.hedgehog" %% "hedgehog-core" % hedgehogVersion,
      "qa.hedgehog" %% "hedgehog-runner" % hedgehogVersion,
      "qa.hedgehog" %% "hedgehog-sbt" % hedgehogVersion
    ).map(_ % Test),

    testFrameworks ++= Seq(TestFramework("hedgehog.sbt.Framework")),
  )

