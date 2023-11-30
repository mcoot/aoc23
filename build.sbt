ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "aoc23",

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies += "com.softwaremill.sttp.client3" %% "core" % "3.8.13",
    libraryDependencies += "org.typelevel" %% "cats-parse" % "0.3.9",
  )
