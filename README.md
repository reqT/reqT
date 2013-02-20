# reqT
http://reqt.org is a free requirements modeling tool 

With reqT you can ...
* create and manage requirements models using versatile collections
* work distributed with requirements fragments in git and issue repos
* combine informal language expressiveness with type-safe modeling
* interoperate with spread sheet applications and web publishing
* do powerful scripting of requirements models with Scala
* extend the open, internal DSL with your own modeling semantics

## The reqT directory structure
When you clone this repo you get these files in directory reqT/
* `reqT` a scala script file for the REPL to init reqT with :load reqT
* `src/main/scala/`  a directory for the reqT source code
    * `metamodel.scala` entity, attribute, relationship and structure elements 
    * `model.scala`  a graph-oriented collection of requirements stored as a fast hashmap 
    * `constraints.scala`  a constraint satisfaction programming API for requirements
    * `jacop.scala`  an interface to the JaCoP solver
    * `package.scala` implicits and utility functions, values and variables  
* `target/` a directory for compiled class files
* `lib/` a directory for jars. You need to download and put the `JaCoP-v3.2.jar` that can be downloaded from http://sourceforge.net/projects/jacop-solver/files/latest/download 

##How to compile reqT
    scalac -cp lib/* -d target src/main/scala/*

##How to run reqT in the Scala REPL (compile reqT first)
Unix-like: `scala -cp "lib/*:target"`
Windows: `scala -cp "lib\*;target"` or execute the `run.cmd` file

When the REPL has started you need to init reqT with the :load reqT command:

    scala> :load reqT