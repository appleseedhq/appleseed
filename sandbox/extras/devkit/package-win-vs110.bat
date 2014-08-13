@echo off

set packageName=appleseed-win-vs110-devkit.zip

REM Remove previous package and staging directory.
del %packageName% 2>nul
rd /S /Q package 2>nul

REM Create staging directory.
mkdir package
xcopy /E bin package\bin\
xcopy /E cmake package\cmake\
xcopy /E include package\include\
xcopy /E lib package\lib\
xcopy /E sample package\sample\
xcopy /E heightfield package\heightfield\

REM Enter staging directory.
cd package

REM Cleanup package\heightfield\ directory.
cd heightfield
call %~dp0\cleanup-sample.bat
rd /S /Q "data\source images" 2>nul
cd ..

REM Cleanup package\sample\ directory.
cd sample
call %~dp0\cleanup-sample.bat
cd ..

REM Generate the package.
"c:\Program Files\7-Zip\7z.exe" a -r ..\%packageName%

REM Leave staging directory.
cd ..

REM Remove staging directory.
rd /S /Q package 2>nul
