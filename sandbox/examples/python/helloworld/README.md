Instructions to run helloworld.py
=================================

On Windows
----------

With an **end-user** release of appleseed:

    set PYTHONPATH=%PYTHONPATH%;C:\path\to\appleseed\lib\python
    set PATH=%PATH%;C:\path\to\appleseed\bin
    python helloworld.py

With a **working copy** of the appleseed repository:

    set PYTHONPATH=%PYTHONPATH%;C:\path\to\appleseed\sandbox\lib\<compiler>\<config>\python
    set PATH=%PATH%;C:\path\to\appleseed\sandbox\bin\<compiler>\<config>
    python helloworld.py

where

- `<compiler>` is `vc140` (Visual Studio 2015) or `vc141` (Visual Studio 2017);
- `<config>` is `Debug`, `Release`, `Profile` or `Ship`.

On Linux and macOS
------------------

We're assuming a Bash shell and Python 2.7.

With an **end-user** release of appleseed:

    export PYTHONPATH=$PYTHONPATH:/path/to/appleseed/lib/python
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/path/to/appleseed/lib
    python helloworld.py

With a **working copy** of the appleseed repository:

    export PYTHONPATH=$PYTHONPATH:/path/to/appleseed/sandbox/lib/<config>/python
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/path/to/appleseed/sandbox/lib/<config>
    python helloworld.py

where `<config>` is `Debug`, `Release`, `Profile` or `Ship`.
