@ECHO OFF
ECHO Compiling reqT ... %TIME%
call scalac -feature -deprecation -cp "lib\*" -d target "src\main\scala\*"
ECHO Ready!             %TIME%
ECHO Error level: %ERRORLEVEL%
