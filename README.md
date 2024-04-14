<img src="https://github.com/reqT/reqT/raw/4.x/logo/reqT.jpeg" width="100"> 

# `reqT` is a requirements engineering tool

* reqT is an open source requirements modelling tool for software developers. 

* With reqT you can: 
  * model, analyse, visualize and prioritize requirements
  * combine natural language with formal structure
  * flexibly mix abstraction levels and modelling approaches
  * integrate requirements and test specification
  * export and import models via open formats
  * manipulate models with powerful Scala scripts
  * allocate requirements to releases by constraint solving

* For more information on see https://reqt.github.io/ 

## Getting started


### Download

* Download the latest `reqT.jar` from https://github.com/reqT/reqT/releases or use curl in terminal like so:
    
      curl -LO https://github.com/reqT/reqT/releases/download/4.0.0-M2/reqT.jar


### Run app

* Run reqT with: 
    
      java -jar reqT.jar

* If `java` is missing on your system then install Java JDK LTS from here: https://adoptium.net 
  * Java JDK version 17 is recommended.
 
  * The reqT jar-file is built with Java 17 but later LTS versions may also work. If you experience problems then revert to Java JDK version 17.

### Run shell

* Run reqT in the Scala repl with:
    
      scala-cli repl --jar reqT.jar

* When the `scala>` prompt is shown type: `import reqt.*` and you are [good to go scripting](https://reqt.github.io/).

* If `scala-cli` is missing on your system then install it from here: https://scala-cli.virtuslab.org/

### Install 

It is optional but convenient to install the `reqt` command on your path like so:

  * Linux/MacOS: 
    * Create a dir called `reqT` in your home directory and put the [`reqT.jar`](https://github.com/reqT/reqT/releases) there.
    * Create a file named `reqt` (name in lower case) in `~/bin` with the contents below:
      ```bash
      #!/bin/bash
      java -jar ~/reqT/reqT.jar $@
      ```
    * Do `chmod +x ~/bin/reqt` to make it executable. 
    * Make `reqt` executable by `chmod +x ~/bin/reqt`
    * This requires that `~/bin` is on your `$PATH` which normally is the case in e.g. Ubuntu.

  * Windows:
    * Create a dir called `reqT` in your home directory and put the `reqT.jar` (see Download above) in that dir.
    * Create a file named `reqt.cmd` (name in lower case) in the `reqT` dir with the contents below:
      ```cmd
      @echo OFF
      set _jarfile=%HOMEDRIVE%%HOMEPATH%\reqT\reqT.jar
      call java %_jarfile% %*
      ```
    * Add the `reqT` dir to your Windows Path e.g. by [following these instructions](https://stackoverflow.com/questions/44272416).

After a log out and log in you should be able to run reqT by typing `reqt` in terminal in any working dir. 

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
* [**os-lib**](https://github.com/com-lihaoyi/os-lib) for using files and processes

## Build

* Clone this repo locally. You may want to use `git clone --depth 1` (to avoid the history with some old binaries).
* Make sure you have installed [`scala-cli`](https://scala-cli.virtuslab.org/) and [`sbt`](https://www.scala-sbt.org/)
* Run `sbt assembly`
* You will find the jar in `target`

## Publish
For maintainers with github access to this repo:
* Run `scala-cli run publish.sc` and follow instructions


