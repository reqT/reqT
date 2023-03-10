lazy val reqTVer     = "4.0.0-M1"
lazy val reqTJarName = "reqT.jar"
lazy val reqtLangVer = "4.0.0-M3"
lazy val scalaVer    = "3.3.0-RC3"
lazy val RSTAVer     = "3.3.2"
lazy val AutoCompVer = "3.3.1"

ThisBuild / version       := reqTVer
ThisBuild / scalaVersion  := scalaVer
ThisBuild / organization  := "io.github.reqt"

console / initialCommands := """import reqt.*"""
Global / onChangedBuildSource := ReloadOnSourceChanges

fork := true
outputStrategy := Some(StdoutOutput)
run / javaOptions += "-Xmx8G"
run / connectInput := true

lazy val `reqT` = (project in file("."))
  .settings(
    name := "reqT",
    scalacOptions := List("-encoding", "utf8", "-Werror", "-deprecation", "-unchecked"),
    assembly / assemblyJarName := reqTJarName,
    assembly / mainClass := Some("reqt.Main"),

    libraryDependencies += "reqt-lang" % "reqt-lang" % reqtLangVer from 
      s"https://github.com/reqT/reqT-lang/releases/download/$reqtLangVer/reqt-lang_3-$reqtLangVer.jar",

    libraryDependencies += "com.fifesoft" % "rsyntaxtextarea" % RSTAVer,
    libraryDependencies += "com.fifesoft" % "autocomplete" % AutoCompVer,
  )