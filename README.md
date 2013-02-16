reqT
====
Create this file structure on your system:
In directory <somewhere>/reqT/
- reqT a script file for the REPL to init reqT with :load reqT
- src/main/scala/  a directory for the reqT source code
  -- metamodel.scala  entity, attribute, relationship and structure elements 
  -- model.scala  a map collection called Model to store elements in a graph structure 
  -- csp.scala   constraints imposed on models can be satisfied if consistent
  -- jacop.scala  an interface to the JaCoP constraint satisfaction solver
  -- util.scala string and file utilities for reqT  
- lib/ a directory for jars including JaCoP v3.2
- target/ a directory for compiled class files

How to compile
-----
scalac -cp "lib\JaCoP-3.2.jar" -d target src/main/scala/*

How to run
-----
scala -cp "target;lib\JaCoP-3.2.jar" 

scala> :load reqT