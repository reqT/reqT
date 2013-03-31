:: put this code in a file called reqt.cmd in a dir appended to path
@echo OFF
set _jarfilename=%HOMEDRIVE%%HOMEPATH%\reqT\lib\reqT.jar
echo Adding %_jarfilename% to CLASSPATH
set CLASSPATH=%CLASSPATH%;%_jarfilename%
echo %CLASSPATH%
call scala %HOMEDRIVE%%HOMEPATH%\reqT\lib\reqT.jar %*
echo ** Exit reqT at %TIME%