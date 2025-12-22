lazy val reqTVer      = "4.6.3"  // stay in sync with reqT-lang
lazy val reqTJarName  = s"reqT-$reqTVer.jar"

lazy val reqTLangVer  = "4.6.1" // https://github.com/reqT/reqT-lang/releases
lazy val reqTJacopVer = "1.1.2" // https://github.com/reqT/reqT-jacop/releases
lazy val jacopVer     = "4.10.0" // https://github.com/radsz/jacop/releases
lazy val scalaVer     = "3.3.7" // Use LTS only! https://www.scala-lang.org/download/all.html
lazy val RSTAVer      = "3.5.3" // https://github.com/bobbylight/RSyntaxTextArea/releases
lazy val AutoCompVer  = "3.3.1" // https://github.com/bobbylight/AutoComplete/releases
lazy val OSLibVer     = "0.11.6" // https://github.com/com-lihaoyi/os-lib/releases
lazy val scalaXmlVer  = "2.4.0"   // deprecated; migration needed eventually...
lazy val FlatLafVer   = "3.5.4"  // https://github.com/JFormDesigner/FlatLaf

ThisBuild / version       := reqTVer
ThisBuild / scalaVersion  := scalaVer
ThisBuild / organization  := "io.github.reqt"

console / initialCommands := """import reqt.*"""
Global / onChangedBuildSource := ReloadOnSourceChanges

Compile / doc / scalacOptions ++= Seq("-siteroot", "https://fileadmin.cs.lth.se/reqt/")
Compile / doc / target := file("target/api")
Compile / doc / scalacOptions ++= Seq("-project", "reqT")

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
    scalacOptions := Seq("-encoding", "utf8", "-deprecation", "-unchecked", "-Werror", "-feature"),

    assembly / assemblyJarName := reqTJarName,
    assembly / mainClass := Some("reqt.Main"),

    libraryDependencies += githubDep("reqt-lang", "reqT", "reqT-lang", reqTLangVer),
    libraryDependencies += githubDep("reqt-jacop", "reqT", "reqT-jacop", reqTJacopVer),

    libraryDependencies += "org.jacop"    % "jacop"           % jacopVer,
    libraryDependencies += "com.fifesoft" % "rsyntaxtextarea" % RSTAVer,
    libraryDependencies += "com.fifesoft" % "autocomplete"    % AutoCompVer,
    libraryDependencies += "com.lihaoyi" %% "os-lib"          % OSLibVer,
    libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % scalaXmlVer,
    libraryDependencies += "com.formdev" % "flatlaf" % FlatLafVer,
  )