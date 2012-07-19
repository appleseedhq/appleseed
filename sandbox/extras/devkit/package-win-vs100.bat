@echo off

rd /S /Q package 2>nul

mkdir package

xcopy /E bin package\bin\
xcopy /E include package\include\
xcopy /E lib package\lib\
xcopy /E sample package\sample\

cd package\sample

rd /S /Q Debug 2>nul
rd /S /Q ipch 2>nul
rd /S /Q Release 2>nul

del /Q output\test*.* 2>nul

del .gitignore 2>nul
del /Q *.opensdf 2>nul
del /Q *.sdf 2>nul
del /Q *.suo 2>nul
del /Q *.user 2>nul

cd ..

"c:\Program Files\7-Zip\7z.exe" a -r ..\appleseed-win-vs100-devkit.zip

cd ..

rd /S /Q package 2>nul

pause
