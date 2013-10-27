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

  lazy val root = Project(
    id = "root",
    base = file("."),
    aggregate = Seq(common, plugin)
  )

  lazy val common = Project(
    id   = "drscala-common",
    base = file("common"),
    settings = sharedSettings
  ) settings (
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _)
  )

  lazy val plugin = Project(
    id   = "drscala-plugin",
    base = file("plugin"),
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
      },
      scalacOptions += "-Xplugin:/home/alois/oss/drscala/plugin/target/scala-2.10/drscala-plugin-assembly-0.1.0-SNAPSHOT.jar",
      scalacOptions ++= Seq(
        "warn",
        "debug"
      ).map("-P:drscala:" + _)
    ) dependsOn(common)
}
