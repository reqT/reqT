@echo OFF
set _jarfilename=%USERPROFILE%\reqT\lib\reqT.jar
echo Adding %_jarfilename% to CLASSPATH
set CLASSPATH=%CLASSPATH%;%_jarfilename%
echo %CLASSPATH%
call scala %USERPROFILE%\reqT\lib\reqT.jar
echo ** Exit reqT at %TIME%