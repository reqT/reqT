@ECHO OFF
ECHO Starting scala REPL ... %TIME%
ECHO To start reqT type:
ECHO :l reqT
ECHO ...
call scala -cp "lib\*;target"
ECHO Exit scala REPL         %TIME%
