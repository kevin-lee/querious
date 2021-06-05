ThisBuild / scalaVersion := "2.12.13"

ThisBuild / name := props.ProjectName
ThisBuild / organization := props.Org
ThisBuild / version := props.Version

ThisBuild / crossScalaVersions := props.CrossScalaVersions

lazy val querious = (project in file("."))
  .settings(
    scalacOptions ++= Seq(
//      "-feature",
//      "-language:higherKinds"
    ),
//
    addCompilerPlugin("org.scalamacros" % "paradise"           % "2.1.1" cross CrossVersion.full),
    addCompilerPlugin("com.olegpy"     %% "better-monadic-for" % "0.3.1"),
    reporterConfig := reporterConfig
      .value
      .withColumnNumbers(true)
      .withSourcePathColor(scala.Console.MAGENTA + scala.Console.UNDERLINED),
    libraryDependencies ++= libs.all,
    testFrameworks ++= Seq(TestFramework("hedgehog.sbt.Framework"))
  )

lazy val props =
  new {
    final val Org                = "io.kevinlee"
    final val ProjectName        = "querious"
    final val Version            = "0.1.0"
    final val CrossScalaVersions = List("2.11.12", "2.12.13")

    final val HedgehogVersion  = "0.7.0"
    final val NewtypeVersion   = "0.4.4"
    final val RefinedVersion   = "0.9.12"
    final val FastparseVersion = "1.0.0"
  }

lazy val libs =
  new {
    lazy val newtype   = "io.estatico" %% "newtype"   % props.NewtypeVersion
    lazy val refined   = "eu.timepit"  %% "refined"   % props.RefinedVersion
    lazy val fastparse = "com.lihaoyi" %% "fastparse" % props.FastparseVersion
    lazy val hedgehog  = Seq(
      "qa.hedgehog" %% "hedgehog-core"   % props.HedgehogVersion,
      "qa.hedgehog" %% "hedgehog-runner" % props.HedgehogVersion,
      "qa.hedgehog" %% "hedgehog-sbt"    % props.HedgehogVersion
    ).map(_ % Test)

    final val all      =
      Seq(
        newtype,
        refined,
        fastparse
      ) ++ hedgehog

  }
