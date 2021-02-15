ThisBuild / scalaVersion := "2.12.12"
ThisBuild / organization := "scala-advent-utils"

lazy val hello = (project in file("."))
  .settings(
    name := "ScalaAdventUtils",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.2",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % Test,
  )

