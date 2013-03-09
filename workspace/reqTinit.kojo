//put me in the initk lib of Kojo (you should also have the reqT jar in libk)
println("Welcome to reqT version " + reqt.VERSION)
import reqt._
Model.interpreter = Some(builtins.kojoInterp.interp)
println("import reqt._")
