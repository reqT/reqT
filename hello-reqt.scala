//> using scala 3.6.2
//> using dep "reqt:reqt:4.0.0-RC2,url=https://github.com/reqT/reqT/releases/download/v4.0.0-RC2/reqT-4.0.0-RC2.jar"

import reqt.*
val m = Model(
    Feature("helloWorld") has Spec("print greeting")
  )

@main def run = 
  println(m.toMarkdown)