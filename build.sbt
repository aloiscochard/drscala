import AssemblyKeys._

organization := "com.github.aloiscochard"

name := "drscala"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.10.3"

description := "A doctor for your code"

libraryDependencies ++= Seq("org.kohsuke" % "github-api" % "1.44")

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _)

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots") 
)

assemblySettings

artifact in (Compile, assembly) ~= { art =>
  art.copy(`classifier` = Some("assembly"))
}

addArtifact(artifact in (Compile, assembly), assembly)
