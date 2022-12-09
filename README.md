<img src="http://reqt.org/reqT.jpg" width="100"> 

reqT: Requirements => Code
==========================
reqT is a free requirements modelling tool for software developers. With reqT you can model, analyse and visualize your system requirements and test specifications using a powerful DSL embedded in Scala.

For more information see http://reqT.org 

How to run reqT
---------------
* Download [reqT.jar](http://reqT.org/reqT.jar) 

* Launch reqT with this command: 
    
        java -jar reqT.jar

 
How to build reqT from code
---------------------------

The reqT build is only supported for Linux but it may work on other systems.

1. Install SDKMAN from https://sdkman.io/install
2. Download this [zip](https://github.com/reqT/reqT/archive/3.1.x.zip) and extract all files in some directory.
3. Run the `./build.sh` script among the extracted files
4. Run the generated jar with `java -jar reqT.jar`

The reqT v3 branch is unfortunately stuck on old Scala 2.12.15 because of its tight integration with the repl and the standard library.

How to change the reqT metamodel
--------------------------------

The metamodel of reqT is written in reqT [here](https://github.com/reqT/reqT/blob/3.0.x/src/reqT/meta.scala) and is used to generate the concrete case classes in the reqT Scala DSL as shown [here](https://github.com/reqT/reqT/blob/3.0.x/src/reqT/GENERATED-metamodel.scala). If you change the metamodel you need to first recompile reqT and then generate a new `GENERATED-metamodel.scala` and then recompile again. All this is done by the provided `meta-build` script. 

If you want the syntax coloring in the reqT gui editor to be updated in line with your new metamodel you also need to update [reqT-syntax](https://github.com/reqT/reqT-syntax) as described [here](https://github.com/reqT/reqT-syntax/blob/master/README.md).   

Dependencies 
-------------

The creators and users of reqT sincerely thank all the contributors of the following **open source libraries** [(licences)](https://github.com/reqT/reqT/tree/3.0.x/licences):
* [Scala](http://www.scala-lang.org/) used for embedded DSL construction and more
* [JaCoP](http://sourceforge.net/projects/jacop-solver/) used for constraint solving
* [RSyntaxTextArea](http://fifesoft.com/rsyntaxtextarea/) used for syntax coloring
* [Autocomplete](http://fifesoft.com/autocomplete/) used for autocompletion
* [jLine](https://github.com/jline/jline2) used for the reqT command line interface

