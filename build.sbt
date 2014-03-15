import AssemblyKeys._

organization := "com.github.aloiscochard"

name := "drscala"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.10.3"

description := "A doctor for your code"

libraryDependencies ++= Seq(
  "org.brianmckenna" %% "wartremover" % "0.7",
  "net.databinder.dispatch" %% "dispatch-core" % "0.11.0",
  "io.argonaut" %% "argonaut" % "6.0.3"
)

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _)

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots"),
  "spray repo" at "http://repo.spray.io"
)

assemblySettings

artifact in (Compile, assembly) ~= { art =>
  art.copy(`classifier` = Some("assembly"))
}

addArtifact(artifact in (Compile, assembly), assembly)

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (f) =>
  {
    case "scalac-plugin.xml"     => MergeStrategy.first
    case x => f(x)
  }
}
