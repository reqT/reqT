#!/ usr / bin / env -S scala -cli shebang

//> using scala "2.12.17"

// run using scala-cli run 

val fn = "src/reqT/GlobalConstants.scala"

val b = "val reqT_BUILD ="

val source = scala.io.Source.fromFile(fn)

val in = source.getLines.toList

source.close

def getInt(s: String) = s.split("=")(1).trim.toInt

def transform(s: String) = if (!s.trim.startsWith(b)) s else {
    val oldbnr = getInt(s)
    val bnr = oldbnr + 1
    println(s"Build number updated from $oldbnr to $bnr.")
    s"  $b $bnr"
  }

val out = in.map(transform)

def saveString(text: String, fileName: String, enc: String = "UTF-8"): Unit = {
  val f = new java.io.File(fileName)
  val pw = new java.io.PrintWriter(f, enc)
  try pw.write(text) finally pw.close()
}

saveString(text = out.mkString("\n"), fileName = fn)