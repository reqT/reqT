<img src="https://github.com/reqT/reqT/raw/4.x/logo/reqT.jpeg" width="100"> 

reqT: Requirements => Code
==========================
* reqT is an open source requirements modelling tool for software developers with a permissive license. 
* With reqT you can model, analyze and visualize your system requirements using both readable natural language and a powerful DSL embedded in Scala.
* For more information on **how to use reqT** see docs at https://reqt.github.io/ 

How to run reqT
---------------

* Download the latest [reqT.jar](https://github.com/reqT/reqT/releases) 

* Launch reqT with this command: 
    
        java -jar reqT.jar

* If java is missing on your system then install Java JDK LTS from here: https://adoptium.net 
  * Java JDK version 17 is recommended.
 
  * The reqT jar-file is built with Java 17 but later LTS versions may also work. If you experience problems then revert to Java JDK version 17.

 
How to build the latest reqT from source
----------------------------------------

1. Clone this repo locally. You may want to use `git clone --depth 1` (to avoid the history with some old binaries).
2. Install java, scala, sbt
3. Run `sbt "clean; package; assembly"`
4. Run the *thin* jar with `scala target/scala-latest-version/reqt_3-latest-version.jar` (check target for latest version)
5. Run the **fat** jar with `java -jar target/scala-latest-version/reqT.jar` (check target for latest version)

Dependencies 
-------------

The reqT app primarily depends on [**reqT-lang**](https://github.com/reqT/reqT-lang/), a zero-dependency, cross-platform, permissive-licensed library containing the underlying reqT language and api.

This repo uses the following open source projects (with their own licenses):
* [**reqT-lang**](https://github.com/reqT/reqT-lang) the reqT language as a platform-independent, zero-dependency scala library with a permissive license 
* [**reqT-jacop**](https://github.com/reqT/reqT-jacop) an interface to the JaCoP constraint solver
* [**JaCoP**](https://github.com/radsz/jacop) used for constraint solving
* [**RSyntaxTextArea**](http://fifesoft.com/rsyntaxtextarea/) used for syntax coloring in the reqT swing editor
* [**Autocomplete**](http://fifesoft.com/autocomplete/) used for autocompletion in the reqT swing editor
* [**Scala**](http://www.scala-lang.org/) the lean and powerful programming language
* [**sbt**](https://www.scala-sbt.org/) the scala build tool


