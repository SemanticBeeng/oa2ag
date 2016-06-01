import sbt._
import Keys._

object BuildSettings {

  val buildScalaVersion = "2.10.3"

  val buildSettings = Defaults.defaultSettings ++ Seq(
    scalaVersion := buildScalaVersion,
    scalacOptions ++= Seq( "-encoding", "UTF-8"), // "-deprecation", "-feature", "-unchecked",
    resolvers := Seq(
      Resolver.sonatypeRepo("snapshots"),
      Resolver.sonatypeRepo("releases")
    ),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.0-M3" cross CrossVersion.full)
  )
}


object Dependencies {
  val scalaLibrary = "org.scala-lang" % "scala-library" % BuildSettings.buildScalaVersion % "provided"
  val scalaReflect = "org.scala-lang" % "scala-reflect" % BuildSettings.buildScalaVersion
}

object MyBuild extends Build {

  import BuildSettings._
  import Dependencies._

  import generator.CodeToGenerate._

  val deps = buildSettings ++ generator.CodeToGenerate.settings ++ Seq(
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
      libraryDependencies += "org.scalamacros" % "quasiquotes" % "2.0.0-M3" cross CrossVersion.full,
      libraryDependencies += "org.scalatest" %% "scalatest" % "1.9" % "test" exclude("org.scala-lang", "scala-reflect"),
      sourceGenerators in (root, Compile) <+= generateLcCode,
      sourceGenerators in (root, Compile) <+= generateLetCode,
      sourceGenerators in (root, Compile) <+= generateStlcCode,
      sourceGenerators in (root, Compile) <+= generateTypeReCode,
      sourceGenerators in (root, Compile) <+= generatePaperCode,
      sourceGenerators in (root, Compile) <+= generateCZeroCode
    )

  lazy val macros = Project("macros", file("macros"), settings = deps)

  lazy val root: Project = Project(
    "root",
    file("."),
    settings = deps
  ) dependsOn macros

}
