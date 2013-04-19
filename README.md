<img src="http://reqt.org/reqT.jpg" width="80"> 

reqT: Requirements => Code
==========================
reqT is a free tool for software requirements modelling. For more information see http://reqt.org 

```scala

val about = Model(
  Feature("create") has Gist("Create requirements and test cases using versatile collections"),
  Feature("collaborate") has Gist("Work distributed with model chunks in emails, wikis, git repos, tickets, ..."),
  Feature("express") has Gist("Combine natural language expressiveness with graph-oriented modeling"),
  Feature("interop") has Gist("Interoperate with spread sheet applications and web publishing"),
  Feature("scalable") has Gist("Modularize models in hierarchies and script them in Scala"),
  Feature("open") has Gist("Extend the Scala-internal DSL with your own modeling semantics")
)

```

How to run reqT
---------------
Preparations:
* Install Scala 2.10.1 from http://www.scala-lang.org/downloads
* Download the [reqT.jar](https://github.com/reqT/reqT/raw/master/reqT.jar) and put it in some directory of your chioce, e.g. `\Users\<user>\reqT\lib` (Windows) or `~/reqT/lib` (Linux or MacOS) 
 
Different ways of running reqT, depending on your preference:

1.  Quick try-it-out: 
    
    Open a command window (terminal, bash, cmd, or powershell etc.) and change directory to where you have put the  [reqT.jar](https://github.com/reqT/reqT/raw/master/reqT.jar) and type: 
    
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
    * Put [reqT.jar](https://github.com/reqT/reqT/raw/master/reqT.jar)  in the `<home>/reqT/lib` directory
    * Create a new directory e.g. `<home>/reqT/bin` and put `reqt.cmd`(Windows) or `reqt.sh`(Unix-like) as explained below:
    
    **Windows** Put the `reqt.cmd` file in some directory `<home>\reqT\bin`.  
    Make sure that the <home> path *does not include spaces*, as the current scala.bat launcher [assumes non-spaced paths](https://issues.scala-lang.org/browse/SI-7355). Append the `<home>\reqT\bin`  directory to your `PATH` user environment variable, by (1) right-clicking on your computer, (2) select "Properties", (3) follow "Advanced system settings", (4) click on the "Environment variables..." button and (5) select the PATH user variable and (6) click Edit and (7) append the string `;<home>\reqT\bin` (note the semicolon).

    **Unix-like**  Put the `reqt.sh` bash file in the directory ~/reqT/bin and open a terminal window and execute these commands:
          
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

Hello reqT
----------
Type this at the reqT> prompt, in the Kojo script editor or in the plain scala REPL scala> prompt:
           
            var m = Model(Feature("hello") has Gist("Hello reqT!"))

Editors with scala support
--------------------------
* [Notepad++](http://notepad-plus-plus.org/) for windows: use this [Scala-definition userDefineLang.xml file](http://www.reqt.org/download/userDefineLang.xml) and put it in `%APPDATA%\Notepad++`
* [Textwrangler](http://www.barebones.com/products/textwrangler/) for MacOS: use this [config file](https://github.com/scala/scala-dist/tree/master/tool-support/src/textwrangler) 
* [Sublime text 2](http://www.sublimetext.com/2)
* [Eclipse plugin for Scala](http://scala-ide.org/)
            
Directory structure of this repo
--------------------------------
When you clone this repo you get these files in directory `reqT/`:
* `reqT.jar` a pre-packaged jar file ready to run as described above
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

1. On Windows you can download and execute the provided [build.cmd](https://github.com/reqT/reqT/raw/master/build.cmd) 
    
2. You can build reqT from scratch step by step after cloning this repo using something similar to:

          scalac -cp "lib\*" -d target "src\main\scala\*"
          jar xf lib/JaCoP-3.2.jar JaCoP
          mv JaCoP target/JaCoP
          jar cfe reqT.jar reqt.start -C target/ .

3. Use http://www.scala-sbt.org/ and the file build.sbt