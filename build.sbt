ThisBuild / scalaVersion := "2.13.4"
ThisBuild / organization := "scala-advent-utils"

lazy val hello = (project in file("."))
  .settings(
    name := "ScalaAdventUtils",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.2",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % Test,
    scalacOptions ++= Seq("-deprecation", "-feature")
  )

