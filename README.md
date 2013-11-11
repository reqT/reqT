<img src="http://reqt.org/reqT.jpg" width="80"> 

reqT: Requirements => Code
==========================
reqT is a free tool for software requirements modelling. For more information see http://reqt.org 

```scala

val about = Model(
  Feature("create") has Gist("Model requirements as computational entities in Scala"),
  Feature("collaborate") has Gist("Collaborate using reqT model chunks in emails, wikis,, tickets, ..."),
  Feature("express") has Gist("Combine flexibel natural language powerful graph-oriented modeling"),
  Feature("interop") has Gist("Interoperate with spread sheets and web publishing"),
  Feature("scalable") has Gist("Modularize models in hierarchies and script them in Scala"),
  Feature("open") has Gist("Extend the Scala-embedded DSL with your own semantics")
)

```

How to run reqT
---------------
Preparations:
* Install Scala from http://www.scala-lang.org/downloads
* Download the [reqT.jar](http://reqT.org/reqT.jar) and put it in some directory of your chioce, e.g. `\Users\<user>\reqT\lib` (Windows) or `~/reqT/lib` (Linux or MacOS) 
 
Different ways of running reqT, depending on your preference:

1.  Quick try-it-out: 
    
    Open a command window (terminal, bash, cmd, or powershell etc.) and change directory to where you have put the [reqT.jar](http://reqT.org/reqT.jar) and type: 
    
        > scala -toolcp reqT.jar reqT.jar
    
2.  Run inside Kojo

    Kojo is an easy-to-use scripting environment for Scala with a nice syntax-highligting script editor.
    * Install Kojo from http://www.kogics.net/kojo-download
    * Put the `reqT.jar` file in the `<home>/.kojo/lite/libk` directory
    * Put the `reqTinit.kojo` file in the `<home>/.kojo/lite/initk` directory
    * Launch Kojo and after a while a message similar to this should print in the output window:
        
            ---
            ** Initializing reqT for the Scala interpreter ...
            Welcome to reqT version 2.3.0
            ---

3.  Install `reqt` as a command
    * Create a new directory e.g. `<home>/reqT/lib`
    * Put [reqT.jar](http://reqT.org/reqT.jar)  in the `<home>/reqT/lib` directory
    * Create a new directory e.g. `<home>/reqT/bin` and put in that directory either [reqt.cmd](https://github.com/reqT/reqT/raw/master/reqt.cmd) (Windows) or [reqt.sh](https://github.com/reqT/reqT/raw/master/reqt.sh) (Linux/MacOS) and put that dir on your path as explained below:
    
    **Windows**  Append the `<home>\reqT\bin`  directory to your `PATH` user environment variable, by (1) right-clicking on your computer, (2) select "Properties", (3) follow "Advanced system settings", (4) click on the "Environment variables..." button and (5) select the PATH user variable and (6) click Edit and (7) append the string `;<home>\reqT\bin` (note the semicolon). Make sure that the <home> path *does not include spaces*, as the current scala.bat launcher [assumes non-spaced paths](https://issues.scala-lang.org/browse/SI-7355). 

    **Linux/MacOS**  Open a terminal window and execute something similar to these commands:
          
          cd ~/reqT/bin
          mv reqt.sh reqt
          chmod u+x reqt
          
    Append this text at the end of your `~/.profile` file:
     
          export PATH=$PATH:~/reqT/bin

          
     **Start reqt** Now you should be able to start reqT from any directory by typing the `reqt`command by your cmd, powershell or bash prompt. You should see something similar to this:
     
            > reqt
            Starting reqT-v2.3.0 compiled with Scala 2.10.1 ...
            
            **                  _______        
            **                 |__   __|       
            **   _ __  ___   __ _ | |          
            **  |  __|/ _ \ / _  || |        http://reqT.org
            **  | |  |  __/| (_| || |   
            **  |_|   \___| \__  ||_|   
            **                 | |      
            **                 |_|      

            ** Type ? for help on reqT

            ** Initializing Scala interpreter for reqT ...

            reqT> 

4.  Run reqT as a gui app:
    * Download http://reqT.org/reqT-gui.jar and save in a dir myDir
    * Try to double click on the jar. If that does not work, navigate to myDir and run with
     `java -jar reqT-gui.jar`    

5. Run reqT from a compiled app with a main in Eclipse:
    * download the Scala ide for Eclipse from http://scala-ide.org/
    * create a new Scala project and add a new package called `myPackage`
    * add a new Scala object
    * right click in the package explorer and choose Build Path -> Configure Build Path... 
    * in the Libraries tab press the Add external jar... button. Add the reqT.jar and also the scala-compiler.jar and scala-reflect.jar. The latter libraries can be found in your scala installation's lib directory.
    * Execute `reqt.initInterpreter()` in your main, using something similar to this code: 
            ```scala
            package myPackage

            object myStuff { 
              import reqt._
              def makeMyModel: Model = {
                var m1 = Model()
                m1 += Feature("hej") has Gist("greeting in Swedish")
                val m2 = """Model(Feature("modelFromString") has Gist("interpret"))""".toModel
                m1 ++ m2
              }
            }

            object myMain {
              def main(args: Array[String]): Unit = {
                reqt.initInterpreter() //will create new IMain
                println("** ... ready to rock!\n")
                println(myStuff.makeMyModel)
              }
            }
            ```    
     
Hello reqT
----------
Type this at the reqT> prompt, in the Kojo script editor or in the plain scala REPL scala> prompt:
           
            var m = Model(Feature("hi") has Gist("Hello reqT!"))

Editors with scala support
--------------------------
* [Notepad++](http://notepad-plus-plus.org/) for windows: use this [Scala-definition userDefineLang.xml file](http://www.reqt.org/download/userDefineLang.xml) and put it in `%APPDATA%\Notepad++`
* [Textwrangler](http://www.barebones.com/products/textwrangler/) for MacOS: use this [config file](https://github.com/scala/scala-dist/tree/master/tool-support/src/textwrangler) 
* [Sublime text 2](http://www.sublimetext.com/2)
* [Eclipse plugin for Scala](http://scala-ide.org/)
            
Directory structure of this repo
--------------------------------
When you clone this repo you get these files in directory `reqT/`:
* `src/main/scala/`  a directory for the reqT source code modules:
    * `metamodel.scala` entity, attribute, relationship and structure elements 
    * `model.scala`  a graph-like collection for storing requirements in fast hashmaps 
    * `constraints.scala`  a constraint solving API for requirements models
    * `jacop.scala`  an  interface to the http://www.jacop.eu/ solver
    * `package.scala` implicits, utility functions, and global values  
* `lib/` a directory including: 
    * the JaCoP jar from http://sourceforge.net/projects/jacop-solver/ 


How to build reqT from code
---------------------------
There are several ways to build reqT after cloning this repo:

1. On Windows you can download and execute the provided [build.cmd](https://github.com/reqT/reqT/raw/master/build.cmd) 
    
2. You can build reqT from scratch step by step after cloning this repo using something similar to:

          mkdir target
          scalac -cp "lib\*" -d target "src\main\scala\*"
          jar xf lib/JaCoP-3.2.jar JaCoP
          mv JaCoP target/JaCoP
          jar cfe reqT.jar reqt.start -C target/ .

3. Use http://www.scala-sbt.org/ and the file build.sbt

How to generate scaladoc from code
----------------------------------
`scaladoc -d apidoc src/*.scala`