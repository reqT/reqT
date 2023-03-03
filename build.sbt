ThisBuild / version      := "4.0.0-M1"

ThisBuild / scalaVersion := "3.3.0-RC3"

ThisBuild / organization := "io.github.reqt"

console / initialCommands := """import reqt.*"""

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val reqtLangVer = "4.0.0-M2"

lazy val `reqT` = (project in file("."))
  .settings(
    name := "reqT",
    scalacOptions := List("-encoding", "utf8", "-Werror", "-deprecation", "-unchecked"),
    assembly / assemblyJarName := "reqT.jar",

    libraryDependencies += "reqt-lang" % "reqt-lang" % reqtLangVer from 
      s"https://github.com/reqT/reqT-lang/releases/download/$reqtLangVer/reqt-lang_3-$reqtLangVer.jar",
  )