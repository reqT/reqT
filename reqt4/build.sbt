scalaVersion := "3.2.1"

fork                := true // https://stackoverflow.com/questions/18676712
connectInput        := true // http://www.scala-sbt.org/1.x/docs/Forking.html
outputStrategy      := Some(StdoutOutput)

run / javaOptions ++= Seq("-cp", "'./lib/*'")

assembly / assemblyJarName := "reqt4.jar"

//assembly / assemblyMergeStrategy := { case x => MergeStrategy.first }
ThisBuild / assemblyMergeStrategy := {
  case PathList(ps @ _*) if ps.last endsWith "module-info.class" => MergeStrategy.first
  case x =>
    val oldStrategy = (ThisBuild / assemblyMergeStrategy).value
    oldStrategy(x)
}

// libraryDependencies ++= Seq(
//   "org.scala-lang" %% "scala3-compiler" % "3.1.0",
//   "org.scala-lang" %% "scala3-library" % "3.1.0",
//   "org.scala-lang" % "scala3-interfaces" % "3.1.0",
//   "org.scala-lang" %% "scala3-staging" % "3.1.0",
//   "org.scala-lang" %% "scala3-tasty-inspector" % "3.1.0",
//   "org.scala-lang.modules" % "scala-asm" % "9.1.0-scala-1", // used by the backend
//   "org.scala-lang" %% "tasty-core" % "3.1.0",
//   "org.jline" % "jline-reader" % "3.19.0",   // used by the REPL
//   "org.jline" % "jline-terminal" % "3.19.0",
//   "org.jline" % "jline-terminal-jna" % "3.19.0",
// )
