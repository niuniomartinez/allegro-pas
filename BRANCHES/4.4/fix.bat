@echo off

rem Sets makefile source code for the different platforms
rem Based on fix.bat of Allegro.
rem Modified By Kronoman - In loving memory of my father.
rem Modified By Guillermo "Ñuño" Martínez.

if [%1] == [linux]   goto linux
if [%1] == [win32] goto win32
goto help


:win32
echo Configuring for Windows/FPC...
echo # Warning! This file will be overwritten by configuration routines! > target.os
echo TARGET=WIN32>> target.os
goto done


:linux
echo Configuring for GNU/Linux...
echo # Warning! This file will be overwritten by configuration routines! > target.os
echo TARGET=LINUX>> target.os
goto done


:help
echo Usage: fix platform
echo.
echo Where platform is one of: win32 or linux. 
echo.
goto end

:done
echo Done!

:end
