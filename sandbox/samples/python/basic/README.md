Instructions to run basic.py
============================

On Windows
----------

With an END-USER release of appleseed:

    set PYTHONPATH=%PYTHONPATH%;C:\path\to\appleseed\lib\python2.7
    set PATH=%PATH%;C:\path\to\appleseed\bin
    python basic.py


With a DEVELOPER release of appleseed:

    set PYTHONPATH=%PYTHONPATH%;C:\path\to\appleseed\sandbox\lib\<compiler>\<config>\python2.7
    set PATH=%PATH%;C:\path\to\appleseed\sandbox\bin\<compiler>\<config>
    python basic.py


On Linux and macOS
------------------

We're assuming a Bash shell and Python 2.7.

With an END-USER release of appleseed:

    export PYTHONPATH=$PYTHONPATH:/path/to/appleseed/lib/python2.7
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/path/to/appleseed/lib
    python basic.py


With a DEVELOPER release of appleseed:

    export PYTHONPATH=$PYTHONPATH:/path/to/appleseed/sandbox/lib/<config>/python2.7
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/path/to/appleseed/sandbox/lib/<config>
    python basic.py
