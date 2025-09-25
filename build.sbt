// ====== Project settings ======
ThisBuild / organization := "it.unibo.scalabluff"
ThisBuild / scalaVersion := "3.6.4"
ThisBuild / version      := "0.1.0-SNAPSHOT"

lazy val javafxV  = "21.0.3"
lazy val scalafxV = "21.0.0-R32"

lazy val root = (project in file("."))
  .settings(
    name := "ScalaBluff",

    // ====== Dependencies ======
    libraryDependencies ++= Seq(
      "org.scalafx" %% "scalafx" % scalafxV,
      "org.openjfx" %  "javafx-controls" % javafxV,
      "org.openjfx" %  "javafx-fxml"     % javafxV,
      // Usa il classifier "win" per Windows. Se vuoi un JAR portabile per altri OS,
      // aggiungi anche i classifier "linux"/"mac" oppure rimuovi il classifier e fornisci il JRE modulare a parte.
      "org.openjfx" %  "javafx-graphics" % javafxV classifier "win",

      // Test
      "org.scalatest"     %% "scalatest"       % "3.2.18"   % Test,
      "org.scalatestplus" %% "scalacheck-1-17" % "3.2.18.0" % Test,
      "org.scalacheck"    %% "scalacheck"      % "1.17.0"   % Test
    ),

    // ====== Scala compiler options ======
    scalacOptions ++= Seq("-deprecation", "-feature"),

    // ====== Run/Test options ======
    Compile / run / fork := true,
    Compile / run / connectInput := true,
    Compile / run / javaOptions += {
      val f = (baseDirectory.value / "logging.properties")
      s"-Djava.util.logging.config.file=${f.getAbsolutePath}"
    },
    Test / fork := true
  )

// ====== sbt-assembly (fat JAR) ======
import sbtassembly.AssemblyPlugin.autoImport._
import sbtassembly.PathList
import sbtassembly.MergeStrategy

// Scegli quale main avviare nel JAR:
val AppMain = "it.unibo.bluff.view.gui.MainGUI"     // GUI
// Per la CLI usa, in alternativa:
// val AppMain = "it.unibo.bluff.Main"

// Imposta la Main-Class per run/package/assembly
Compile / run / mainClass         := Some(AppMain)
Compile / packageBin / mainClass  := Some(AppMain)
assembly / mainClass              := Some(AppMain)

// Nome del JAR prodotto da assembly
assembly / assemblyJarName := s"${name.value}-assembly-${version.value}.jar"

// Strategia di merge per evitare conflitti nelle dipendenze
assembly / assemblyMergeStrategy := {
  case PathList("META-INF", xs @ _*)                 => MergeStrategy.discard
  case "module-info.class"                           => MergeStrategy.discard
  case "reference.conf"                              => MergeStrategy.concat
  case PathList("META-INF", "services", xs @ _*)     => MergeStrategy.concat
  case x                                             => (assembly / assemblyMergeStrategy).value(x)
}

// Non eseguire i test durante l'assembly (più veloce)
assembly / test := {}
