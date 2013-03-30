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
Preparations:
* Install Scala 2.10.1 from http://www.scala-lang.org/downloads
* Download the reqT.jar and put it in some directory <reqtdir> of your chioce
 
Different ways of running reqT, depending on your preference:

1.  Simple start
    Open a terminal window and change directory to where you put the reqT.jar and type: 
    
        >scala -cp reqT.jar
    
    When the scala interpreter has started, type `reqt.init($intp)` by the scala prompt:
    
        scala> reqt.init($intp)


2.  Run using Kojo

    Kojo is an easy to use environment for Scala with a syntax-highligting editor and interactive scripting support.

    * Install Kojo from 
    * Put the `reqT.jar` file in the `<home>/.kojo/lite/libk` directory
    * Put the `reqTinit.kojo` file in the `<home>/.kojo/lite/initk` directory
    * Launch Kojo and after a while the following message should print in the output window:
        
3.  Install a `reqt` command for easy start of the reqT interpreter wrapper anywhere

    To make it easy to start reqT anywhere from a terminal och command prompt and to avoid having to type `reqt.init($intp)` everytime, you can make an executable script that appends the `reqT.jar` to your `CLASSPATH` and then starts reqT with `scala <home>/reqT/lib/reqT.jar`. Make sure to append the directory of your script to your system environment PATH variable to enable execution from anywhere. This can be done using something similar to this (replace <home> with suitable directory):
    
    * Create a new directory e.g. `<home>/reqT/lib`
    * Put `reqT.jar` in the `<home>/reqT/lib` directory
    * Create a new directory e.g. `<home>/reqT/bin`
    * Put `reqt.cmd` file (Windows) or a `reqt` bash file (Unix-like) including something similar to:
        
        :: put this code in a file called reqt.cmd
        @echo OFF
        set _jarfilename=%USERPROFILE%\reqT\lib\reqT.jar
        echo Adding %_jarfilename% to CLASSPATH
        set CLASSPATH=%CLASSPATH%;%_jarfilename%
        echo %CLASSPATH%
        call scala %USERPROFILE%\reqT\lib\reqT.jar
        echo ** Exit reqT at %TIME%


        
        
Directory structure
-------------------
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

1. On Windows you can run the provided `build.cmd` that will do this for you
    * compiles reqT into `target\`
    * packages compiled class files into `reqT.jar`
    * copies relevant files to `%HOMEDRIVE%%HOMEPATH%\reqT\`
    
    If you want, you can put the `reqt.cmd` file in the `%HOMEDRIVE%%HOMEPATH%\reqT\bin` directory and append that directory to your `PATH`user environment variable, by (1) right-clicking on your computer, (2) select "Properties", (3) follow "Advanced system settings", (4) click on the "Environment variables..." button and (5) select the PATH user variable and (6) click Edit and (7) append the string `;C:\Users\bjornr\reqT\bin` where you should replace C:\Users\bjornr with your home directiry.     

2. You can build reqT from scratch step by step using something similar to:

    scalac -feature -deprecation -cp "lib\*" -d target "src\main\scala\*"
    jar xf lib/JaCoP-3.2.jar JaCoP
    mv JaCoP target/JaCoP
    jar cfe reqT.jar reqt.start -C target/ .

3. Use sbt