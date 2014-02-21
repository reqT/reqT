//put me in the initk lib of Kojo (you should also have reqT.jar in libk)
println("---")
import reqT._
reqT.initInterpreter(builtins.kojoInterp.interp)
println(s"Welcome to reqT version $reqT_VERSION build date $BUILD_DATE")
println("---")
