import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._

object DrScala extends Build {
  lazy val sharedSettings = Defaults.defaultSettings ++ Seq(
    scalaVersion := "2.10.3",
    organization := "com.github.aloiscochard",
    version := "0.1.0-SNAPSHOT",
    description := "A doctor for your code"
  )

  lazy val plugin = Project(
    id   = "drscala",
    base = file("."),
    settings = sharedSettings ++ assemblySettings) settings (
      libraryDependencies ++= Seq(
        "org.brianmckenna" %% "wartremover" % "0.7",
        "net.databinder.dispatch" %% "dispatch-core" % "0.11.0",
        "io.argonaut" %% "argonaut" % "6.0.3"
      ),
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _),
      publishArtifact in Compile := false,
      artifact in (Compile, assembly) ~= { art =>
        art.copy(`classifier` = Some("assembly"))
      },
      mergeStrategy in assembly <<= (mergeStrategy in assembly) { (f) =>
        {
          case "scalac-plugin.xml"     => MergeStrategy.first
          case x => f(x)
        }
      }
    )
}
