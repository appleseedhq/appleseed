@echo off

set build=..\..\..\build\win-vs110

REM Cleanup.

rd /S /Q bin 2>nul
rd /S /Q include 2>nul
rd /S /Q lib 2>nul

rd /S /Q sample\Debug 2>nul
rd /S /Q sample\ipch 2>nul
rd /S /Q sample\Release 2>nul
rd /S /Q sample\x64 2>nul
del /Q sample\sample.opensdf 2>nul
del /Q sample\sample.sdf 2>nul
del /Q sample\sample.suo 2>nul

rd /S /Q sample\output 2>nul
mkdir sample\output
echo This file allows to preserve this otherwise empty directory. > sample\output\preserve.txt

REM Copy Binaries.

mkdir bin\Debug
copy /Y %build%\appleseed\Debug\appleseed.dll               bin\Debug\
copy /Y %build%\appleseed\Debug\appleseed.pdb               bin\Debug\
copy /Y %build%\appleseed.shared\Debug\appleseed.shared.dll bin\Debug\
copy /Y %build%\appleseed.shared\Debug\appleseed.shared.pdb bin\Debug\

mkdir bin\Release
copy /Y %build%\appleseed\Ship\appleseed.dll                bin\Release\
copy /Y %build%\appleseed.shared\Ship\appleseed.shared.dll  bin\Release\

REM Copy Import Libraries.

mkdir lib\Debug
copy /Y %build%\appleseed\Debug\appleseed.lib               lib\Debug\
copy /Y %build%\appleseed.shared\Debug\appleseed.shared.lib lib\Debug\

mkdir lib\Release
copy /Y %build%\appleseed\Ship\appleseed.lib                lib\Release\
copy /Y %build%\appleseed.shared\Ship\appleseed.shared.lib  lib\Release\

REM Copy Headers.

xcopy /E ..\..\..\src\appleseed\*.h            include\appleseed\
xcopy /E ..\..\..\src\appleseed.shared\*.h     include\appleseed.shared\
xcopy /E ..\..\..\3rdparty\openexr\include\*.h include\openexr\
