@echo off

set packageName=appleseed-win-vs100-devkit.zip

REM Remove leftovers.
del %packageName% 2>nul
rd /S /Q package 2>nul

REM Create staging directory.
mkdir package
xcopy /E bin package\bin\
xcopy /E include package\include\
xcopy /E lib package\lib\
xcopy /E sample package\sample\

cd package\sample

REM Remove build artifacts from sample directory.
rd /S /Q Debug 2>nul
rd /S /Q ipch 2>nul
rd /S /Q Release 2>nul
rd /S /Q x64 2>nul

REM Remove output files.
del /Q output\scene.obj 2>nul
del /Q output\test*.* 2>nul

REM Remove non-deployable files.
del .gitignore 2>nul
del /Q *.opensdf 2>nul
del /Q *.sdf 2>nul
del /Q *.suo 2>nul
del /Q *.user 2>nul

cd ..

REM Generate the package.
"c:\Program Files\7-Zip\7z.exe" a -r ..\%packageName%

cd ..

REM Remove staging directory.
rd /S /Q package 2>nul

pause
