@echo off

rem Sets makefile source code for the different platforms
rem Based on fix.bat of Allegro.
rem Modified By Kronoman - In loving memory of my father.
rem ************************************************
rem REMEMBER TO ALTER THE TEST TO SUIT YOUR NEEDS!!!
rem ************************************************

if [%1] == [linux]   goto linux
if [%1] == [djgpp]   goto djgpp
if [%1] == [mingw32] goto mingw32
if [%1] == [test] goto test
goto help


:test
REM REMEMBER TO ALTER THIS TEST TO SUIT YOUR NEEDS!!!

REM You first need to configure the platform
if exist target.os goto targetok
    echo Before test, you first must configure your platform.
goto help

:targetok

echo Testing, please wait...
make test

if not errorlevel 0 goto testfail
if not exist test.run goto testfail

    echo.
    echo * SUCESS *
    echo Congratulations, the test compiled!
    echo.

goto testdone

:testfail
    echo.
    echo * ERROR *
    echo.
    echo The compilation returned a error!
    echo Check that:
    echo (*) You have all compiler tools installed (gcc,make,etc)
    echo.

:testdone
    echo Cleaning the test...
    make cleantest
    
goto done

:djgpp
echo Configuring for DOS/djgpp...
echo # Warning! This file will be overwritten by configuration routines! > target.os
echo TARGET=DJGPP>> target.os
goto done


:mingw32
echo Configuring for Windows/Mingw32...
echo # Warning! This file will be overwritten by configuration routines! > target.os
echo TARGET=MINGW32>> target.os
goto done


:linux
echo Configuring for Linux/GCC...
echo # Warning! This file will be overwritten by configuration routines! > target.os
echo TARGET=LINUX>> target.os
goto done


:help
echo Usage: fix platform
echo.
echo Where platform is one of: djgpp, mingw32 or linux. 
echo.
echo NOTICE:
echo You can also call: fix test
echo to check if your system can compile this project.
echo.
goto end

:done
echo Done!

:end
