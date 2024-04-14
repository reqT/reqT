<img src="https://github.com/reqT/reqT/raw/4.x/logo/reqT.jpeg" width="100"> 

# `reqT`: a requirements tool

* reqT is an open source requirements modelling tool for software developers. 

* With reqT you can model, analyze and visualize your system requirements using both readable natural language and a 
powerful DSL embedded in Scala.

* For more information on see https://reqt.github.io/ 

## Getting started


### Download

* Download the latest version from here: [reqT.jar](https://github.com/reqT/reqT/releases) or use curl in terminal like so:
    
      curl -LO https://github.com/reqT/reqT/releases/download/4.0.0-M2/reqT.jar


### Run

* Run reqT with: 
    
      java -jar reqT.jar

* If java is missing on your system then install Java JDK LTS from here: https://adoptium.net 
  * Java JDK version 17 is recommended.
 
  * The reqT jar-file is built with Java 17 but later LTS versions may also work. If you experience problems then revert to Java JDK version 17.

### Install

* To install the `reqt` command on your path:
  * Create a dir `reqT` and put the `reqT.jar` (see Download above) in that dir. 
    * Linux/MacOS: 
      * put a file named `reqt` in `~/bin` with the contents below and do `chmod +x ~/bin/reqt`
      
          #!/bin/bash
          java -jar ~/reqT/reqT.jar $@ 
      * Change the path to the reqT.jar to where you put it. 
      * Make `reqt` executable by `chmod +x ~/bin/reqt`
      * This requires that `~/bin` is on your `$PATH` which normally is the case in e.g. Ubuntu

    * Windows:
      * TODO
  * Now you should be able to run reqT by typing `reqt` in terminal. 
 
## Dependencies 

The reqT app primarily depends on [**reqT-lang**](https://github.com/reqT/reqT-lang/), a zero-dependency, cross-platform, permissive-licensed library containing the underlying reqT language and api.

This repo uses the following open source projects (with their own licenses):
* [**reqT-lang**](https://github.com/reqT/reqT-lang) the reqT language as a platform-independent, zero-dependency scala library with a permissive license 
* [**reqT-jacop**](https://github.com/reqT/reqT-jacop) an interface to the JaCoP constraint solver
* [**JaCoP**](https://github.com/radsz/jacop) used for constraint solving
* [**RSyntaxTextArea**](http://fifesoft.com/rsyntaxtextarea/) used for syntax coloring in the reqT swing editor
* [**Autocomplete**](http://fifesoft.com/autocomplete/) used for autocompletion in the reqT swing editor
* [**Scala**](http://www.scala-lang.org/) the lean and powerful programming language
* [**sbt**](https://www.scala-sbt.org/) the scala build tool

## Build

* Clone this repo locally. You may want to use `git clone --depth 1` (to avoid the history with some old binaries).
* Make sure you have installed [`scala-cli`](https://scala-cli.virtuslab.org/) and [`sbt`](https://www.scala-sbt.org/)
* Run `sbt assembly`
* You will find the jar in `target`

## Publish
For maintainers with github access to this repo:
* Run `scala-cli run publish.sc` and follow instructions


