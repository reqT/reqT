reqT
====
Create this file structure on your system:
In directory <somewhere>/reqT/
- reqT a script file for the REPL to init reqT with :load reqT
- src/main/scala/  a directory for the reqT soruce code
- lib/ a directory for jars 
- target/ a directory where to compile to

How to compile
-----
scalac -cp "lib\JaCoP-3.2.jar" -d target src/main/scala/*

How to run
-----
scala -cp "target;lib\JaCoP-3.2.jar" 

scala> :load reqT