@echo off

REM Remove build artifacts.
rd /S /Q x64 2>nul

REM Recreate output directory.
rd /S /Q output 2>nul
mkdir output
echo This file allows to preserve this otherwise empty directory. > output\preserve.txt

REM Remove non-deployable files.
del .gitignore 2>nul
del /Q *.opensdf 2>nul
del /Q *.sdf 2>nul
del /Q *.suo 2>nul
del /Q *.user 2>nul
