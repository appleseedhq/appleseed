@ECHO OFF

REM ~ Copyright 2006-2008 Rene Rivera.
REM ~ Distributed under the Boost Software License, Version 1.0.
REM ~ (See accompanying file LICENSE_1_0.txt or http://www.boost.org/LICENSE_1_0.txt)

REM ~ set BJAM=bjam
REM ~ set BJAM_SRC=..\src
REM ~ set BJAM_BIN=..\src\bin.ntx86\bjam.exe

setlocal
goto Start

:Test_Path
REM Tests for the given file(executable) presence in the directories in the PATH
REM environment variable. Additionaly sets FOUND_PATH to the path of the
REM found file.
setlocal & endlocal & ver>NUL
setlocal
set test=%~$PATH:1
endlocal
if not errorlevel 1 set FOUND_PATH=%~dp$PATH:1
goto :eof

:Guess_BJAM
setlocal & endlocal & ver>NUL
if NOT "_%BJAM%_" == "__" goto :eof
call :Test_Path bjam.exe
if not errorlevel 1 (
    set BJAM=bjam.exe
    goto :eof)
if "_%BJAM%_" == "__" (
    set BJAM=%BJAM_BIN%
    goto :eof)
setlocal & endlocal & ver>NUL
goto :eof

:Build_BJAM_To_Test
setlocal & endlocal & ver>NUL
if "_%BJAM_SRC%_" == "__" set BJAM_SRC=..\src
PUSHD %BJAM_SRC%
call build.bat
@ECHO OFF
POPD
set BJAM_BIN=%BJAM_SRC%\bin.ntx86\bjam.exe
setlocal & endlocal & ver>NUL
goto :eof

:Start
call :Build_BJAM_To_Test
call :Guess_BJAM
@ECHO ON
%BJAM% -f test.jam "-sBJAM=%BJAM_BIN%"
