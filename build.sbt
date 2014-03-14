import AssemblyKeys._

organization := "com.github.aloiscochard"

name := "drscala"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.10.3"

description := "A doctor for your code"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.3.0",
  "io.spray" % "spray-client" % "1.3.0",
  "io.spray" %% "spray-json" % "1.2.5"
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
