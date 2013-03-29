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

How to run reqT
---------------
* Install scala 2.10.1 from http://www.scala-lang.org/downloads
* Create a new directory e.g. `<somepath>/reqT/lib`
* Download the `reqT.jar` and put it in that directory
* Append `;<somepath>/reqT/lib/reqT.jar` to your `CLASSPATH` system environment variable
* Start reqT with `scala reqT.jar`

You can also just start the scala repl with `scala -cp reqT.jar` 
and then at the scala repl prompt type:

    scala> reqt.init($intp)

Directory structure
-------------------
When you clone this repo you get these files in directory `reqT/`:
* `reqT.jar` a pre-packaged jar file ready to run as describet above
* `src/main/scala/`  a directory for the reqT source code modules:
    * `metamodel.scala` entity, attribute, relationship and structure elements 
    * `model.scala`  a graph-like collection for storing requirements in fast hashmaps 
    * `constraints.scala`  a constraint solving API for requirements models
    * `jacop.scala`  an  interface to the http://www.jacop.eu/ solver
    * `package.scala` implicits, utility functions, and global values  
* `lib/` a directory including: 
    * the JaCoP jar from http://sourceforge.net/projects/jacop-solver/ 

Create this directory to store compiled class files:
* `target/` 

How to build reqT from code
---------------------------
There are several ways to build reqT after cloning this repo:
* You can build reqT from scratch step by step using:

    scalac -feature -deprecation -cp "lib\*" -d target "src\main\scala\*"

    jar xf lib/JaCoP-3.2.jar JaCoP

    mv JaCoP target/JaCoP

    jar cfe reqT.jar reqt.start -C target/ .

* Or on Windows you can run the provided `build.cmd` that 
    * compiles reqT into `target/`
    * packages compiled class files into `reqT.jar`
    * copies relevant files to `/Users/<yourusername>/reqT/`
