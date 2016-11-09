@echo off
set PATH=%PATH%;..\..\..\sandbox\bin\v140\Debug
set PYTHONPATH=%PYTHONPATH%;..\..\..\sandbox\bin\v140\Debug
python runtests.py
pause
