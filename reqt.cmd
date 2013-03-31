:: put this code in a file called reqt.cmd in a dir appended to path
@echo OFF
set _jarfilename=%HOMEDRIVE%%HOMEPATH%\reqT\lib\reqT.jar
set CLASSPATH=%CLASSPATH%;%_jarfilename%
call scala %HOMEDRIVE%%HOMEPATH%\reqT\lib\reqT.jar %*
