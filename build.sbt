ThisBuild / organization := "it.unibo.scalabluff"
ThisBuild / scalaVersion := "3.6.4"
ThisBuild / version      := "0.1.0-SNAPSHOT"

lazy val root = (project in file("."))
  .settings(
    name := "ScalaBluff",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.18" % Test,
      "org.scalatestplus" %% "scalacheck-1-17" % "3.2.18.0" % Test,
      "org.scalacheck" %% "scalacheck" % "1.17.0" % Test,
      "org.scalafx" %% "scalafx" % "20.0.0-R31"

    ),
    scalacOptions ++= Seq("-deprecation", "-feature")
  )
