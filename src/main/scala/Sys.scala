package reqt

export Sys.{ls, pwd, cd}

object Sys:
  import scala.sys.process._
  val startDir = java.lang.System.getProperty("user.dir").slashify

  val homeDir = java.lang.System.getProperty("user.home").slashify

  @volatile private var workingDirectory = startDir

  def isWindows = sys.props("os.name").startsWith("Windows")
  
  def fileSep = java.lang.System.getProperty("file.separator")

  def resolveFileName(fileName: String): String = 
    val f = java.io.File(fileName)
    val fn = f.toString.slashify
    if f.isAbsolute || fn.take(1) == "/" || fn.contains(":") then fn 
    else workingDirectory + "/" + fn

  def listFiles(dir: String): Option[Seq[java.io.File]] = 
    val path = java.io.File(resolveFileName(dir))
    Option(path.listFiles).map(_.toSeq)

  def saveString(s:String, fileName:String) = 
    val fn = resolveFileName(fileName)
    val outFile = new java.io.File(fn)
    val outStream = new java.io.PrintWriter(outFile,"UTF-8")
    try outStream.println(s.toString) finally outStream.close
    println("Saved string to file: "+fn) 
  
  def loadLines(fileName:String): List[String] = 
    val fn = resolveFileName(fileName)
    val source = scala.io.Source.fromFile(fn)
    try source.getLines.toList finally source.close

  def loadResource(fileName:String): List[String] = 
    val r = getClass().getResource(fileName)
    val source = scala.io.Source.fromURL(r)
    try source.getLines.toList finally source.close

  def workDir = workingDirectory

  def pwd = println(workDir)

  def mkdir(d: String) = new java.io.File(d).mkdirs

  def ls(d: String): Unit = 
    val fsOpt = listFiles(d)
    if fsOpt.isEmpty then println("ERROR: Directory not found:" + d )
    else
      val xs = fsOpt.get.map: 
        f => 
          val d = if f.isDirectory then "/" else ""
          f.getName + d
      println(xs.mkString("\n"))
    
  def ls: Unit = ls(workingDirectory)
  
  def cd(d: String): Unit = { 
    val dd = if (d == "..") java.io.File(workingDirectory).getParent.toString  else resolveFileName(d)
    val f = new java.io.File(dd)
    if (f.isDirectory && f.exists) workingDirectory = dd else println("ERROR Directory not found:" + dd )
    pwd  
  }
  def cd: Unit = cd(startDir)

  def exists(fileName: String): Boolean = java.io.File(fileName).exists

  def fixCmd(cmd: Seq[String]): Seq[String] = 
    if (isWindows) Seq("cmd","/C",s"""${cmd.mkString(" ")}""") else cmd

  def runCmd(cmd: Seq[String]): Int = cmd.! 

  def desktopOpen(f: String) = java.awt.Desktop.getDesktop().open(java.io.File(f))

  def isDotInstalled: Boolean = runCmd(fixCmd(Seq("dot","-V"))) == 0
  
  def dotCmd(fileName: String, format: String = "pdf", layout: String = "dot", moreArgs: Seq[String] = Seq()): Seq[String] = 
    val q = if (isWindows) '"'.toString else "" 
    val cmd = Seq("dot",s"-T$format",s"-K$layout") ++ moreArgs ++
      Seq("-o", s"""$q${fileName.newFileType("."+format)}$q""",
        s"""$q${fileName.newFileType(".dot")}$q""")
    fixCmd(cmd)

  extension (s: String) 
    def stripAnySuffix: String = 
      if (s.contains('.')) s.split('.').dropRight(1).mkString(".") else s
    
    def suffix(suf: String):String = if (!s.endsWith(suf)) s + suf else s

    def newFileType(suf: String) = s.stripFileType.suffix(suf)

    def slashify = s.replace(fileSep, "/")
  
    def stripFileType = 
      val ss = s.slashify.split('/')
      val head = ss.dropRight(1)
      val tail = ss.lastOption.map(_.stripAnySuffix).getOrElse("")
      (head ++ Seq(tail)).mkString("/") 

