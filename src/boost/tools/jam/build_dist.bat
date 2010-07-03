@ECHO OFF

REM ~ Copyright 2006-2007 Rene Rivera.
REM ~ Distributed under the Boost Software License, Version 1.0.
REM ~ (See accompanying file LICENSE_1_0.txt or http://www.boost.org/LICENSE_1_0.txt)

REM ~ Make stage for building.
rd /S /Q stage
md stage
cd stage

REM ~ Copy sources to stage.
cd ..\src
xcopy *.bat ..\stage\
xcopy *.jam ..\stage\
xcopy *.sh ..\stage\
xcopy *.com ..\stage\
xcopy *.c ..\stage\
xcopy *.h ..\stage\
xcopy *.y ..\stage\
xcopy *.yy ..\stage\
xcopy Jambase ..\stage\
xcopy /S /I /Y modules ..\stage\modules
xcopy /S /I /Y boehm_gc ..\stage\boehm_gc
cd ..\stage
call .\build.bat

REM ~ Build docs, and copy result to stage.
cd ..\doc
rd /S /Q html
..\stage\bin.ntx86\bjam --v2
xcopy /S /I /Y html ..\stage
cd ..\stage

REM ~ Build distribution archives.
call .\build.bat --- dist
