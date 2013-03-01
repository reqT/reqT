@ECHO OFF
ECHO Compiling reqT ... %TIME%
call fsc -feature -deprecation -cp "lib\*" -d target "src\main\scala\*"
ECHO Ready!             %TIME%
ECHO Error level: %ERRORLEVEL%
