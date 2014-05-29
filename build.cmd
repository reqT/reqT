@echo OFF
set _jarfilename=reqT.jar

:clean
if not exist bin call makebin
if %ERRORLEVEL% NEQ 0 goto error

if exist bin\reqT echo Cleaning bin/reqT & rd /S /Q bin\reqT
if %ERRORLEVEL% NEQ 0 goto error

:compile
echo scalac -feature -deprecation -cp "lib\*" -d bin src/reqT/* 
echo Start compilation  ...  %TIME%
call scalac -feature -deprecation -cp "lib\*" -d bin src/reqT/* 
if %ERRORLEVEL% NEQ 0 goto error
echo Compilation ready!  %TIME%

:package
echo Packaging reqT into jar file: %_jarfilename% 
echo Start packaging ... %TIME%
call jar cfe %_jarfilename% reqT.Main -C bin/ .
if %ERRORLEVEL% NEQ 0 goto error
echo Packaging ready!    %TIME%

:checklibdir
if exist "%USERPROFILE%\reqT\lib" goto checkbindir
mkdir "%USERPROFILE%\reqT\lib"

:checkbindir
if exist "%USERPROFILE%\reqT\bin" goto copyreqt
mkdir "%USERPROFILE%\reqT\bin"

:copyreqt
echo Copying %_jarfilename% to %USERPROFILE%\reqT\lib\
copy /Y %_jarfilename% "%USERPROFILE%\reqT\lib\."
echo Copying reqT.cmd to %USERPROFILE%\reqT\bin\
copy /Y reqT.cmd "%USERPROFILE%\reqT\bin\."
echo If %USERPROFILE%\reqT\bin is in your Path you can run reqT as a command
goto end

:error
echo Error level: %ERRORLEVEL%

:end
