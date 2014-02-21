@echo OFF
set _jarfilename=reqT.jar
echo Cleaning target/reqT ...  %TIME%
rd /S /Q target\reqT
if %ERRORLEVEL% NEQ 0 goto error

echo scalac -feature -deprecation -cp "lib\*" -d target src/main/scala/* src/test/scala/*
echo Start compilation  ...  %TIME%
call scalac -feature -deprecation -cp "lib\*" -d target src/main/scala/* src/test/scala/*
if %ERRORLEVEL% NEQ 0 goto error
echo Compilation ready!  %TIME%

echo Packaging reqT into jar file: %_jarfilename% 
echo Start packaging ... %TIME%
call jar cfe %_jarfilename% reqT.Main -C target/ .
if %ERRORLEVEL% NEQ 0 goto error
echo Packaging ready!    %TIME%
if not exist "%USERPROFILE%\.kojo\lite\libk" goto checklibdir
echo Copying %_jarfilename% to "%USERPROFILE%\.kojo\lite\libk\."
copy /Y %_jarfilename% "%USERPROFILE%\.kojo\lite\libk\."
echo Copying "reqTinit.kojo" to "%USERPROFILE%\.kojo\lite\initk\."
copy /Y "reqTinit.kojo" "%USERPROFILE%\.kojo\lite\initk\."
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
