@echo off

set build=..\..\..\build\win-vs110

REM Remove previous devkit.
rd /S /Q bin 2>nul
rd /S /Q include 2>nul
rd /S /Q lib 2>nul

REM Copy main appleseed libraries.

mkdir bin\Debug
copy /Y %build%\appleseed\Debug\appleseed.dll               bin\Debug\
copy /Y %build%\appleseed\Debug\appleseed.pdb               bin\Debug\
copy /Y %build%\appleseed.shared\Debug\appleseed.shared.dll bin\Debug\
copy /Y %build%\appleseed.shared\Debug\appleseed.shared.pdb bin\Debug\

mkdir bin\Release
copy /Y %build%\appleseed\Ship\appleseed.dll                bin\Release\
copy /Y %build%\appleseed.shared\Ship\appleseed.shared.dll  bin\Release\

REM Copy import libraries.

mkdir lib\Debug
copy /Y %build%\appleseed\Debug\appleseed.lib               lib\Debug\
copy /Y %build%\appleseed.shared\Debug\appleseed.shared.lib lib\Debug\

mkdir lib\Release
copy /Y %build%\appleseed\Ship\appleseed.lib                lib\Release\
copy /Y %build%\appleseed.shared\Ship\appleseed.shared.lib  lib\Release\

REM Copy headers files.

xcopy /E ..\..\..\src\appleseed\*.h                         include\appleseed\
xcopy /E ..\..\..\src\appleseed.shared\*.h                  include\appleseed.shared\
xcopy /E ..\..\..\3rdparty\openexr\include\*.h              include\openexr\
