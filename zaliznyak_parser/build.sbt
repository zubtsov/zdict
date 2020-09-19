ThisBuild / scalaVersion := s"2.12.8"
ThisBuild / organization := "org.zubtsov"

lazy val zdict = (project in file("."))
  .settings(
    name := "zaliznyak_parser",
    libraryDependencies += "org.scalatest" %% s"scalatest" % "3.0.5" % Test,
    libraryDependencies += "org.clapper" %% s"grizzled-slf4j" % "1.3.4",
    libraryDependencies += "org.slf4j" % "slf4j-log4j12" % "1.7.30"
  )