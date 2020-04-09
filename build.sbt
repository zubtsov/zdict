ThisBuild / scalaVersion := "2.12.8"
ThisBuild / organization := "org.zubtsov"

lazy val zdict = (project in file("."))
  .settings(
    name := "zdict",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test
  )