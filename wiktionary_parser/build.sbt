ThisBuild / scalaVersion := "2.12.8"
ThisBuild / organization := "org.zubtsov"

lazy val wiktionary = (project in file("."))
  .settings(
    name := "wiktionary_parser",
    libraryDependencies += "com.google.code.gson" % "gson" % "2.8.6",
    libraryDependencies += "org.jsoup" % "jsoup" % "1.13.1",
    libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.5",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test
  )