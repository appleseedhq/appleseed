@echo off

echo Packaging Release...

rmdir /S /Q tmp 2>nul
del appleseed-package.zip 2>nul

mkdir tmp
cd tmp

svn export https://svn.appleseedhq.net/trunk/sandbox

copy ..\..\sandbox\schemas\* sandbox\schemas\

copy ..\..\sandbox\bin\win32.vs90\Ship\*.exe sandbox\bin\
copy ..\..\sandbox\bin\win32.vs90\Ship\*.dll sandbox\bin\

copy C:\Qt\2009.03-win32.vs90\qt\bin\QtCore4.dll sandbox\bin\
copy C:\Qt\2009.03-win32.vs90\qt\bin\QtGui4.dll sandbox\bin\

move sandbox appleseed
tools\7-zip\7z.exe a ..\appleseed-package.zip appleseed

cd ..
rmdir /S /Q tmp 2>nul

pause
