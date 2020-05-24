ThisBuild / scalaVersion := "2.12.8"
ThisBuild / organization := "org.zubtsov"

lazy val wiktionary = (project in file("."))
  .settings(
    name := "wiktionary_parser",
    libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.5",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test
  )