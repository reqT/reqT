<img src="http://reqt.org/reqT.jpg" width="80"> 

reqT: Requirements => Code
==========================
reqT is a free tool for software requirements modelling. For more information see http://reqT.org 

With reqT you can model, analyse, visualize and plan your software requirements using a powerful DSL embedded in Scala.

This is the 3.0.x branch. The old 2.3.x branch is [here](https://github.com/reqT/reqT/tree/2.3.x)

How to run reqT
---------------

1. Check if you have Java 1.7.x installed by opening a terminal window (bash, cmd, or powershell etc.) and type this command:

        java -version 
    
     and check if it prints something similar to `java version "1.7.0_65"`
     
2. **If** the above step failed **then** install Java 1.7 from here: 
    * [Windows/Mac: scroll down to Java SE7](http://www.oracle.com/technetwork/java/javase/downloads) 
    * [linux install openjdk-7] (http://openjdk.java.net/install/index.html) 

3. Download [reqT.jar](http://reqT.org/reqT.jar) and save the file in some directory. 
 
4. Open a terminal/cmd/powershell window and launch reqT with this command: 
    
        java -jar reqT.jar

 
How to build reqT from code
---------------------------

1. Install Scala version 2.11.2 from the [Scala homepage](http://scala-lang.org/download/2.11.2.html)
2. Download the [reqT.zip](https://github.com/reqT/reqT/archive/3.0.x.zip) and unpack it.
3. Run the build command:
    * On Windows: start powershell and type `./build`
    * On Linux/Mac: start terminal and type  `./build.sh`     
4. Run the generated jar with `java -jar reqT.jar`

Editors with Scala support
--------------------------
* [Notepad++](http://notepad-plus-plus.org/) for windows: use this [Scala-definition userDefineLang.xml file](http://www.reqt.org/download/userDefineLang.xml) and put it in `%APPDATA%\Notepad++`
* [Textwrangler](http://www.barebones.com/products/textwrangler/) for MacOS: use this [config file](https://github.com/scala/scala-dist/tree/master/tool-support/src/textwrangler) 
* [Sublime text 2](http://www.sublimetext.com/2)
* [Eclipse Scala IDE](http://scala-ide.org/)