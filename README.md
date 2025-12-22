<img src="https://github.com/reqT/reqT/blob/4.x/logo/reqT.jpg?raw=true" width="100"> 

# `reqT` is a requirements engineering tool

* reqT is an open source requirements engineering tool for software developers. 

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

### Prerequisites

* You need Java JDK 21, install from [here](https://adoptium.net/temurin/releases/?version=21&os=any&arch=any).
* Other Java versions may also work, but if you get strange errors make sure you have Java JDK 21.

### Download

* Download the latest `reqT.jar` from https://github.com/reqT/reqT/releases by clicking on the jar file under "Assets". or by using the terminal command below for your system:
  * Linux:
    ```
    wget https://github.com/reqT/reqT/releases/latest/download/reqT.jar
    ```
  * Windows Powershell:
    ```
    curl -Uri "https://github.com/reqT/reqT/releases/latest/download/reqT.jar" -OutFile reqT.jar
    ```
  * MacOS:
    ```
    curl -O https://github.com/reqT/reqT/releases/latest/download/reqT.jar
    ```
### Run desktop app

* Run reqT either by double-clicking on the downloaded jar or using this terminal command: 
    
      java -jar reqT.jar

### Run shell in terminal

* If `scala` is missing on your system then install it from here: https://www.scala-lang.org/

* Download reqT.jar (see above) and run reqT in the Scala repl with 
```
java -jar reqT.jar repl`
```

* You can also start the Scala repl with reqT using this command, which does *not* require downloading reqT.jar:
```
scala repl -S 3.6.4 --dep reqT:reqT:4.6.2,url=https://github.com/reqT/reqT/releases/download/v4.6.2/reqT-4.6.2.jar
```

### Manually install reqt on your path

It is optional but convenient to install the `reqt` command on your path like so:

  * Linux/MacOS: 
    * Create a dir called `reqT` in your home directory and put the [`reqT.jar`](https://github.com/reqT/reqT/releases) there.
    * Create a file named `reqt` (name in lower case) in `~/bin` with the contents below:
      ```bash
      #!/bin/bash
      java -jar ~/reqT/reqT.jar $@
      ```
    * Make `reqt` executable by `chmod +x ~/bin/reqt`
    * This requires that `~/bin` is on your `$PATH` which is [the default in e.g. Ubuntu](https://askubuntu.com/a/402410). On MacOS you need to `sudo nano /etc/paths` and add `/Users/YOURUSERNAME/bin` at the bottom of the file.
    * Restart your computer and then the command `reqt` should work in terminal.

  * Windows:
    * Create a dir called `reqT` in your home directory and put the `reqT.jar` (see Download above) in that dir.
    * Create a file named `reqt.cmd` (name in lower case) in the `reqT` dir with the contents below:
      ```cmd
      @echo OFF
      set _jarfile=%HOMEDRIVE%%HOMEPATH%\reqT\reqT.jar
      call java -jar %_jarfile% %*
      ```
    * Add the `reqT` dir to your Windows Path by [following these instructions](https://stackoverflow.com/questions/44272416).
    * Restart your computer and then the command `reqt` should work in terminal.


## Dependencies 

The reqT app primarily depends on [**reqT-lang**](https://github.com/reqT/reqT-lang/), a zero-dependency, cross-platform, permissive-licensed library containing the underlying reqT language and api. 

This repo uses the following open source projects, each with its own license:
* [**reqT-lang**](https://github.com/reqT/reqT-lang): the reqT language (external and embedded DSL for requirements) is provided as a zero-dependency platform-independent Scala library with a permissive license: [Apache v2](https://github.com/reqT/reqT-lang/blob/main/LICENSE).
* [**reqT-jacop**](https://github.com/reqT/reqT-jacop): an abstraction layer for integer constraint solving and interface to the JaCoP constraint solver (license: [AGPLv3](https://github.com/reqT/reqT-jacop/blob/main/LICENSE)).
* [**JaCoP**](https://github.com/radsz/jacop) used by reqT-jacop under the hood for constraint solving (license: [AGPLv3](https://github.com/radsz/jacop/blob/develop/LICENSE.md)).
* [**RSyntaxTextArea**](https://github.com/bobbylight/RSyntaxTextArea): used for syntax coloring in the reqT swing editor (license: [BSD3](https://github.com/bobbylight/RSyntaxTextArea/blob/master/LICENSE.md))
* [**Autocomplete**](https://github.com/bobbylight/AutoComplete): used for autocompletion in the reqT swing editor (license: [BSD3](https://github.com/bobbylight/AutoComplete/blob/master/LICENSE.md))
* [**Scala**](http://www.scala-lang.org/): the leanest, most scalable and expressive programming language of planet Earth (license: [Apache v2](https://github.com/scala/scala3/blob/main/LICENSE)).
* [**sbt**](https://www.scala-sbt.org/): the scala build tool (license: [Apache v2](https://github.com/sbt/sbt/blob/1.10.x/LICENSE)).
* [**os-lib**](https://github.com/com-lihaoyi/os-lib): used for IO and spawning OS processes (license: [MIT](https://github.com/com-lihaoyi/os-lib)).
* [**scala-xml**](https://github.com/scala/scala-xml): used for svg graphics generation (license: [Apache v2](https://github.com/scala/scala-xml/blob/main/LICENSE)). 
* [**Open JDK**](https://github.com/openjdk/jdk): the underlying JVM platform and graphical user interface Swing (license: [GPLv2](https://github.com/openjdk/jdk/blob/master/LICENSE))

The reqT desktop app is transitively, due to the JaCoP constraint solver, licensed as AGPLv3, but the aim is to eventually transfer to [Choco](https://github.com/chocoteam/choco-solver) to enable re-licensing of reqT as permissive Appache v2.

## Build

* Clone this repo locally. You may want to use `git clone --depth 1` to avoid large history.
* Make sure you have installed latest [`scala`](https://www.scala-lang.org/) and [`sbt`](https://www.scala-sbt.org/)
* Run `sbt assembly`
* You will find the jar in `target/scala-<version>/` called `reqT-<version>.jar`

## Publish

For maintainers with github access to this repo:
* Run `scala run publish.sc -S 3.3.7` and follow instructions


