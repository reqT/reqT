// run with `scala-cli run publish.sc`

//> using scala 3.4
//> using toolkit default

println("*** Publish the reqT jar to github using sbt and gh ***\n")

val wd = os.pwd

def yes(msg: String): Boolean = 
  scala.io.StdIn.readLine(msg).headOption.map(_.toUpper) == Some('Y')

def getFromBuild(key: String): Option[String] = util.Try{
    val lines = os.read(wd / "build.sbt").split("\n").toSeq
    val line: String = lines.filter(_.contains(s"val $key")).head
    val value: String = line.split("=").toSeq.last.trim.takeWhile(!_.isWhitespace)
    value.stripPrefix("\"").stripSuffix("\"")
  }.toOption

extension (s: Seq[String]) def showSeq = s.mkString(" ")

val scalaVer = getFromBuild("scalaVer").getOrElse("")
val reqTVer = getFromBuild("reqTVer").getOrElse("")

println("from build.sbt:")
println(s"""val reqTVer  = "$reqTVer"""")
println(s"""val scalaVer = "$scalaVer"""")

println("\n*** Step 1: sbt clean; assembly")

if yes("Do you want a clean build (Y/n)? ") then
  os.proc("sbt", "package", "clean;assembly").call(cwd = wd, stdout = os.Inherit)

println("\n*** Step 2: copy jar")


val dir = s"${os.pwd}/target/scala-$scalaVer"
val file1 = s"$dir/reqT-$reqTVer.jar"
val file2 = s"$dir/reqT.jar"
val copyCmd = Seq("cp", file1, file2)
println(copyCmd.showSeq)
if yes("Do you want to run above cp? (Y/n) ") then
  os.proc(copyCmd).call(cwd = wd)

println("\n*** Step 3: publish to github using gh")
if reqTVer.isEmpty then 
    println("val reqTVer not found in build.sbt")
    sys.exit(1)
else 
  val preRel = 
    if reqTVer.contains("-M") || reqTVer.contains("_RC") then Seq("--prerelease") else Seq()
  val assemblyCmd = Seq()
  val createCmd = Seq("gh", "release", "create", reqTVer, "--generate-notes") ++ preRel

  val uploadCmd1 = Seq("gh", "release", "upload", reqTVer, file1)
  val uploadCmd2 = Seq("gh", "release", "upload", reqTVer, file2)
  

  val ghOpt = util.Try{os.proc("which", "gh").call(cwd = wd)}.toOption

  if ghOpt == None then 
    println("You need the github CLI 'gh' command on your path")
    println("Install from here: https://github.com/cli/cli/")

  println("\nRun these commands in terminal:\n")
  println(createCmd .showSeq)
  println(uploadCmd1.showSeq)
  println(uploadCmd2.showSeq)

//# if ! command -v ghg &> /dev/null
//# then
//#     echo "To run this script you need the github CLI with the 'gh' command installed"
//#     echo "Install from here: https://github.com/cli/cli/blob/trunk/docs/install_linux.md"
//# fi

