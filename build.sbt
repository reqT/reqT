ThisBuild / version      := "4.0.0-M1"

ThisBuild / scalaVersion := "3.3.0-RC2"

ThisBuild / organization := "io.github.reqt"

console / initialCommands := """import reqt.*"""

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val `reqT` = (project in file("."))
  .settings(
    name := "reqT",
    scalacOptions := List("-encoding", "utf8", "-Werror", "-deprecation", "-unchecked"),
    assembly / assemblyJarName := "reqT.jar",

  )