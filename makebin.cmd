@echo OFF
set _jarfilename=reqT.jar
:clearbin
if exist bin echo Clear bin & rd /S /Q bin
if not exist bin md bin
cd bin
echo Extracting files in lib:
for /f %%f in ('dir /b "..\lib"') do call echo %%f & jar xf "..\lib\%%f"
cd .. 
