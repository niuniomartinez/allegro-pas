@echo off

rem Sets makefile source code for the different platforms
rem Based on fix.bat of Allegro.
rem Modified By Kronoman - In loving memory of my father.
rem Modified By Guillermo "Ñuño" Martínez.
rem ************************************************
rem REMEMBER TO ALTER THE TEST TO SUIT YOUR NEEDS!!!
rem ************************************************

if [%1] == [linux]   goto linux
if [%1] == [win32] goto win32
if [%1] == [test] goto test
goto help


:win32
echo Configuring for Windows/Mingw32...
echo # Warning! This file will be overwritten by configuration routines! > target.os
echo TARGET=WIN32>> target.os
goto done


:linux
echo Configuring for Linux/GCC...
echo # Warning! This file will be overwritten by configuration routines! > target.os
echo TARGET=LINUX>> target.os
goto done


:help
echo Usage: fix platform
echo.
echo Where platform is one of: win32 or linux. 
echo.
echo NOTICE:
echo You can also call: fix test
echo to check if your system can compile this project.
echo.
goto end

:done
echo Done!

:end
