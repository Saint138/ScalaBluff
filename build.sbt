ThisBuild / organization := "it.unibo.scalabluff"
ThisBuild / scalaVersion := "3.6.4"
ThisBuild / version      := "0.1.0-SNAPSHOT"

lazy val javafxV  = "21.0.3"
lazy val scalafxV = "21.0.0-R32"

lazy val root = (project in file("."))
  .settings(
    name := "ScalaBluff",

    libraryDependencies ++= Seq(
      "org.scalafx" %% "scalafx" % scalafxV,
      "org.openjfx" % "javafx-controls" % javafxV,
      "org.openjfx" % "javafx-fxml"     % javafxV,
      "org.openjfx" % "javafx-graphics" % javafxV classifier "win",

      // Test
      "org.scalatest"     %% "scalatest"       % "3.2.18"   % Test,
      "org.scalatestplus" %% "scalacheck-1-17" % "3.2.18.0" % Test,
      "org.scalacheck"    %% "scalacheck"      % "1.17.0"   % Test
    ),

    // Opzioni compilatore Scala
    scalacOptions ++= Seq("-deprecation", "-feature"),
    Compile / run / fork := true,
    Compile / run / javaOptions += {
      val f = (baseDirectory.value / "logging.properties")
      s"-Djava.util.logging.config.file=${f.getAbsolutePath}"
    },

    // (opzionale) anche i test in fork
    Test / fork := true
  )
