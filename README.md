<img src="https://github.com/reqT/reqT/raw/4.x/logo/reqT.jpeg" width="100"> 

reqT: Requirements => Code
==========================
* reqT is an open source requirements modelling tool for software developers with a permissive license. 
* With reqT you can model, analyze and visualize your system requirements using both readable natural language and a powerful DSL embedded in Scala.
* For more information on **how to use reqT** see docs at https://reqt.github.io/ 

How to run reqT
---------------
* Install Java 17 from here: https://adoptium.net 

* Download the latest stable [reqT.jar](https://github.com/reqT/reqT/releases) 

* Launch reqT with this command: 
    
        java -jar reqT.jar

 
How to build the latest reqT from code
--------------------------------------

1. Clone this repo locally. You may want to use `git clone --depth 1` (to avoid the history with some old binaries).
2. Install java, scala, sbt
3. Run `sbt assembly` in terminal.
4. Run the thin jar with `scala target/scala-3.3.0-RC2/reqt_3-4.0.0-M1.jar` (check target for latest version)
5. Run the fat jar with `java -jar target/scala-3.3.0-RC2/reqT.jar` (check target for latest version)

Dependencies 
-------------

This repo depends on [reqT-lang](https://github.com/reqT/reqT-lang/) library containing the underlying language and api.

This repo builds on the following open source projects (with their own licenses):
* [Scala](http://www.scala-lang.org/) used for embedded DSL construction and more
* [JaCoP](https://github.com/radsz/jacop) used for constraint solving
* [RSyntaxTextArea](http://fifesoft.com/rsyntaxtextarea/) used for syntax coloring
* [Autocomplete](http://fifesoft.com/autocomplete/) used for autocompletion

