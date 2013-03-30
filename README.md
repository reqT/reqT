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
* Download the reqT.jar from https://github.com/reqT/reqT/raw/master/reqT.jar and put it in some directory of your chioce, e.g. `~/reqT/lib` 
 
Different ways of running reqT, depending on your preference:

1.  Simplistic (but a bit too verbose for repeated usage): 
    
    Open a terminal or cmd window and change directory to where you have put the reqT.jar and type: 
    
        > scala -cp reqT.jar
    
    When the scala interpreter has started, type `reqt.init($intp)` by the scala prompt.


2.  Run inside Kojo

    Kojo is an easy to use scripting environment for Scala with a nice syntax-highligting script editor.
    * Install Kojo from http://www.kogics.net/kojo-download
    * Put the `reqT.jar` file in the hidden `<home>/.kojo/lite/libk` directory
    * Put the `reqTinit.kojo` file in the hidden `<home>/.kojo/lite/initk` directory
    * Launch Kojo and after a while the following message should print in the output window:
        
            ---
            ** Initializing reqT for the Scala interpreter ...
            Welcome to reqT version 2.3.0
            ---

3.  Install `reqt` as command for easy start of the reqT interpreter wrapper in any directory using cmd or bash:

    To make it easy to start reqT anywhere from a terminal och command prompt and to avoid having to type `reqt.init($intp)` everytime, you can make an executable script that appends the `reqT.jar` to your `CLASSPATH` and then starts reqT with `scala <home>/reqT/lib/reqT.jar`. Then append the directory of your script to your system environment PATH variable to enable execution as a command. This can be done using something similar to this (replace <home> with suitable directory):
    * Create a new directory e.g. `<home>/reqT/lib`
    * Put `reqT.jar` in the `<home>/reqT/lib` directory
    * Create a new directory e.g. `<home>/reqT/bin` and put `reqt.cmd`(Windows) or `reqt.sh`(Unix-like) as explained below:
    
    **Windows: reqt command** Put the `reqt.cmd` file (Windows) in directory `\Users\yourname\reqT\bin` 

    **Windows: Add reqt to PATH**  Append the `\Users\yourname\reqT\bin`  directory to your `PATH` user environment variable, by (1) right-clicking on your computer, (2) select "Properties", (3) follow "Advanced system settings", (4) click on the "Environment variables..." button and (5) select the PATH user variable and (6) click Edit and (7) append the string `;C:\Users\<yourusername>\reqT\bin` (note the semicolon and replace <yourusername> with your user name).

    **Linux or MacOS: reqt command**  Put the `reqt.sh` bash file in the directory ~/reqT/bin and open a terminal window and execute these commands:
          
          cd ~/reqT/bin
          mv reqt.sh reqt
          chmod u+x reqt
          
     **Linux or MacOS: Add reqt to PATH** Append this text to the end of your ~/.profile file e.g. using pluma or vi:
     
          export PATH=$PATH:~/reqT/bin
        
        
     **Now you should be able to start reqT by typing the `reqt`command by your cmd or bash prompt. You should see something similar to this:
     
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
Type this at the reqT> prompt, in the Kojo script editor or in the plain scala REPL:
           
            var m = Model(Feature("hello") has Gist("Hello reqT!"))

     
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

1. On Windows you can run the provided `build.cmd` that will do this:
    * compiles reqT into `target\`
    * packages compiled class files into `reqT.jar`
    * copies relevant files to `%HOMEDRIVE%%HOMEPATH%\reqT\`
    
2. You can build reqT from scratch step by step using something similar to:

          scalac -feature -deprecation -cp "lib\*" -d target "src\main\scala\*"
          jar xf lib/JaCoP-3.2.jar JaCoP
          mv JaCoP target/JaCoP
          jar cfe reqT.jar reqt.start -C target/ .

3. Use http://www.scala-sbt.org/ and the file build.sbt