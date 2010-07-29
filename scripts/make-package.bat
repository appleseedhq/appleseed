@echo off

set platform=win32.vs90
set configuration=Ship
set qt-path=C:\Qt\2010.04\qt

set package-name=appleseed-VERSION-MATURITY-%platform%.zip
set package-path=..\archives\%package-name%

set log-file=make-package.log

echo Starting to build %package-path%
echo.

echo Removing left over package...
if exist %package-path% del %package-path%

echo Removing left over staging directory...
if exist appleseed rmdir /S /Q appleseed

echo Clearing log file...
if exist %log-file% del %log-file%

echo Archiving sandbox from HEAD...
pushd ..\sandbox >>%log-file% 2>&1
call git archive -o sandbox.zip HEAD
popd

echo Unzipping sandbox archive to staging directory...
mkdir appleseed >>%log-file% 2>&1
tools\7-zip\7z.exe x -oappleseed ..\sandbox\sandbox.zip >>%log-file% 2>&1
del ..\sandbox\sandbox.zip >>%log-file% 2>&1

echo Removing unwanted files from staging directory...
rmdir /S /Q appleseed\scenes\cyberdemon >>%log-file% 2>&1
del "appleseed\scenes\killeroo\killeroo ao.appleseed" >>%log-file% 2>&1
del "appleseed\scenes\killeroo\killeroo ao close up.appleseed" >>%log-file% 2>&1
rmdir /S /Q appleseed\scenes\smoke >>%log-file% 2>&1
rmdir /S /Q appleseed\scenes\tests\invalid >>%log-file% 2>&1
rmdir /S /Q appleseed\scenes\tests\scope >>%log-file% 2>&1
rmdir /S /Q "appleseed\scenes\tests\self intersections" >>%log-file% 2>&1
rmdir /S /Q appleseed\scenes\winosi\renders >>%log-file% 2>&1

echo Adding local schemas to staging directory...
if not exist appleseed\schemas mkdir appleseed\schemas >>%log-file% 2>&1
copy ..\sandbox\schemas\*.xsd appleseed\schemas\ >>%log-file% 2>&1

echo Adding local binaries to staging directory...
copy ..\sandbox\bin\%configuration%\*.exe appleseed\bin\ >>%log-file% 2>&1
copy ..\sandbox\bin\%configuration%\*.dll appleseed\bin\ >>%log-file% 2>&1

echo Adding dependencies to staging directory...
copy %qt-path%\lib\QtCore4.dll appleseed\bin\ >>%log-file% 2>&1
copy %qt-path%\lib\QtGui4.dll appleseed\bin\ >>%log-file% 2>&1
xcopy /E runtimes\%platform%\*.* appleseed\bin\ >>%log-file% 2>&1

echo Adding LICENSE.txt and README.txt...
copy ..\LICENSE.txt appleseed\ >>%log-file% 2>&1
copy ..\README.txt appleseed\ >>%log-file% 2>&1

echo Building final zip file from staging directory...
tools\7-zip\7z.exe a -xr!*.gitignore %package-path% appleseed >>%log-file% 2>&1

echo Removing staging directory...
rmdir /S /Q appleseed >>%log-file% 2>&1

echo Done.
echo.

echo Check %log-file% for errors.
echo.

pause
