:: put this code in a file called reqT.cmd in a dir appended to path
@echo OFF
set _jarfile=%HOMEDRIVE%%HOMEPATH%\reqT\lib\reqT.jar
call scala -toolcp %_jarfile% %_jarfile% %*
