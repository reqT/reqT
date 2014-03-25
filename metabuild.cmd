call build
if %ERRORLEVEL% NEQ 0 goto end

call run -m
if %ERRORLEVEL% NEQ 0 goto error

call move /Y GENERATED-metamodel.scala src\reqT\
if %ERRORLEVEL% NEQ 0 goto error

call build
if %ERRORLEVEL% NEQ 0 goto end
goto end
:error
echo Error level: %ERRORLEVEL%
:end