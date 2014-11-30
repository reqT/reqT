val fn = "src/reqT/GlobalConstants.scala"
val b = "val reqT_BUILD ="
val source = scala.io.Source.fromFile(fn)
val in = source.getLines.toList
source.close
def getInt(s: String) = s.split("=")(1).trim.toInt
def transform(s: String) = if (s.trim.startsWith(b)) {
	  val bnr = getInt(s)+1
		println(s"Build number: $bnr" )
		s"  $b $bnr"
	} else s
val out = in.map(transform)
scala.tools.nsc.io.File(fn).writeAll(out.mkString("\n"))