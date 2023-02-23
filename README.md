<img src="https://github.com/reqT/reqT/raw/4.x/logo/reqT.jpeg" width="100"> 

reqT: Requirements => Code
==========================
reqT is a free requirements modelling tool for software developers. With reqT you can model, analyze and visualize your system requirements using both readable natural language and a powerful DSL embedded in Scala.

For more information see https://reqt.github.io/ 

How to run reqT
---------------
* Download the latest [reqT.jar](https://github.com/reqT/reqT/releases) 

* Launch reqT with this command: 
    
        java -jar reqT.jar

 
How to build reqT from code
---------------------------

1. Clone this repo locally. You may want to use `git clone --depth 1` (to avoid the history with some old binaries).
2. Install sbt
3. Run `sbt package` in terminal.

Dependencies 
-------------

This repo depends on [reqT-lang](https://github.com/reqT/reqT-lang/) library containing the underlying language and api.

This repo builds on the following open source projects (with their own licenses):
* [Scala](http://www.scala-lang.org/) used for embedded DSL construction and more
* [JaCoP](https://github.com/radsz/jacop) used for constraint solving
* [RSyntaxTextArea](http://fifesoft.com/rsyntaxtextarea/) used for syntax coloring
* [Autocomplete](http://fifesoft.com/autocomplete/) used for autocompletion

