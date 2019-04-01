Instructions to run render.py
=============================

On Windows
----------

With an **end-user** release of appleseed:

    set PYTHONPATH=%PYTHONPATH%;C:\path\to\appleseed\lib\python
    set PATH=%PATH%;C:\path\to\appleseed\bin
    python render.py

With a **working copy** of the appleseed repository:

    set PYTHONPATH=%PYTHONPATH%;C:\path\to\appleseed\sandbox\lib\<compiler>\<config>\python
    set PATH=%PATH%;C:\path\to\appleseed\sandbox\bin\<compiler>\<config>
    python render.py


On Linux and macOS
------------------

We're assuming a Bash shell and Python 2.7.

With an **end-user** release of appleseed:

    export PYTHONPATH=$PYTHONPATH:/path/to/appleseed/lib/python
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/path/to/appleseed/lib
    python render.py

With a **working copy** of the appleseed repository:

    export PYTHONPATH=$PYTHONPATH:/path/to/appleseed/sandbox/lib/<config>/python
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/path/to/appleseed/sandbox/lib/<config>
    python render.py
