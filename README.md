<img src="http://reqt.org/reqT.jpg" width="80"> 

reqT: Requirements => Code
==========================
http://reqt.org is a free tool for codifying software requirement models

With reqT you can ...
* create and manage requirements models using versatile collections
* work distributed with model chunks in emails, wikis, git repos, tickets etc.
* combine informal language expressiveness with type-safe modeling
* interoperate with spread sheet applications and web publishing
* do powerful scripting of requirements models with object-functional Scala
* extend the open, Scala-internal DSL with your own modeling semantics

Directory structure
-------------------
When you clone this repo you get these files in directory `reqT/`:
* `reqT` a scala script file for the REPL to init reqT with :load reqT
* `src/main/scala/`  a directory for the reqT source code modules:
    * `metamodel.scala` entity, attribute, relationship and structure elements 
    * `model.scala`  a graph-like collection of requirements in fast hashmaps 
    * `constraints.scala`  a constraint solving API for requirements models
    * `jacop.scala`  an interface to the http://www.jacop.eu/ solver
    * `package.scala` implicits, utility functions, and global values  
* `lib/` a directory for jars including: 
    * the JaCoP jar from http://sourceforge.net/projects/jacop-solver/ 

Create this directory to store compiled class files:
* `target/` 

Command files for Windows:
* `compile.cmd` compiles reqT into `target/` (see below) 
* `run.cmd` starts the scala REPL (see below) 

How to compile reqT
-------------------
To compile reqT you need to have Scala installed:
* http://www.scala-lang.org/downloads

Then compile reqT with:

    scalac -cp lib/* -d target src/main/scala/*

How to run reqT in the Scala REPL 
---------------------------------

After you have compiled reqT you can run reqT using these commands:
* Unix-like: `scala -cp "lib/*:target"`
* Windows: `scala -cp "lib\*;target"` or execute the `run.cmd` file

When the REPL has started type `:load reqT` by the scala prompt:

    scala> :load reqT