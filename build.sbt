lazy val reqTLangVer  = "4.2.1-M2"
lazy val reqTVer      = "4.2.1-M2"  // should be in sync with reqTLangVer from 4.2.1
lazy val reqTJarName  = s"reqT-$reqTVer.jar"
lazy val reqTJacopVer = "1.1.0" 
lazy val jacopVer     = "4.10.0"
lazy val scalaVer     = "3.3.3"  // use LTS only
lazy val RSTAVer      = "3.3.2"
lazy val AutoCompVer  = "3.3.1"
lazy val OSLibVer     = "0.9.3"

ThisBuild / version       := reqTVer
ThisBuild / scalaVersion  := scalaVer
ThisBuild / organization  := "io.github.reqt"

console / initialCommands := """import reqt.*"""
Global / onChangedBuildSource := ReloadOnSourceChanges
Compile / doc / scalacOptions ++= Seq("-siteroot", "docs")

fork := true
outputStrategy := Some(StdoutOutput)
run / javaOptions += "-Xmx8G"
run / connectInput := true

def githubDep(lib: String, org: String, repo: String, ver: String): ModuleID = {
  val s = s"https://github.com/$org/$repo/releases/download/v$ver/${lib}_3-$ver.jar"
  lib % lib % ver from s 
}

lazy val `reqT` = (project in file("."))
  .settings(
    name := "reqT",
    scalacOptions := Seq("-encoding", "utf8", "-deprecation", "-unchecked", "-Werror"),

    assembly / assemblyJarName := reqTJarName,
    assembly / mainClass := Some("reqt.Main"),

    libraryDependencies += githubDep("reqt-lang", "reqT", "reqT-lang", reqTLangVer),
    libraryDependencies += githubDep("reqt-jacop", "reqT", "reqT-jacop", reqTJacopVer),

    libraryDependencies += "org.jacop"    % "jacop"           % jacopVer,
    libraryDependencies += "com.fifesoft" % "rsyntaxtextarea" % RSTAVer,
    libraryDependencies += "com.fifesoft" % "autocomplete"    % AutoCompVer,
    libraryDependencies += "com.lihaoyi" %% "os-lib"          % OSLibVer,
  )