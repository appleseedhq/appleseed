Instructions to compile C++ plugins
===================================

On Windows
----------

The following instructions assume that:

  - You are using Visual Studio 2015.
  - A [properly-patched Boost 1.55](https://github.com/appleseedhq/appleseed/wiki/Building-appleseed-on-Windows#boost) is installed in `C:\boost_1_55_0`.
  - [appleseed's dependency package](https://github.com/appleseedhq/appleseed/wiki/Building-appleseed-on-Windows#alternative-1-using-prebuilt-third-party-libraries) is installed in `C:\appleseed\appleseed-deps`.

Make sure to adapt them as required.

Open a Command Prompt inside a plugin's folder, then type:

  - With an **end-user** release of appleseed:

        mkdir build
        cd build
        cmake -G "Visual Studio 14 2015 Win64" -DAPPLESEED_INCLUDE_DIR=..\..\..\..\include -DAPPLESEED_LIBRARY=..\..\..\..\lib\appleseed.lib -DBOOST_ROOT=C:\boost_1_55_0 -DIMATH_INCLUDE_DIR=C:\appleseed\appleseed-deps\stage\vc14\ilmbase-release\include -DIMATH_MATH_LIBRARY=C:\appleseed\appleseed-deps\stage\vc14\ilmbase-release\lib\Imath-2_2.lib -DIMATH_IEX_LIBRARY=C:\appleseed\appleseed-deps\stage\vc14\ilmbase-release\lib\Iex-2_2.lib -DIMATH_HALF_LIBRARY=C:\appleseed\appleseed-deps\stage\vc14\ilmbase-release\lib\Half.lib -DOPENEXR_INCLUDE_DIR=C:\appleseed\appleseed-deps\stage\vc14\openexr-release\include -DOPENEXR_IMF_LIBRARY=C:\appleseed\appleseed-deps\stage\vc14\openexr-release\lib\IlmImf-2_2.lib -DOPENEXR_THREADS_LIBRARY=C:\appleseed\appleseed-deps\stage\vc14\ilmbase-release\lib\IlmThread-2_2.lib -DAPPLESEED_DEPS_STAGE_DIR=C:\appleseed\appleseed-deps\stage\vc14 ..

  - With a **working copy** of the appleseed repository:

        mkdir build
        cd build
        cmake -G "Visual Studio 14 2015 Win64" -DAPPLESEED_INCLUDE_DIR=..\..\..\..\..\src\appleseed -DAPPLESEED_LIBRARY=..\..\..\..\lib\v140\Ship\appleseed.lib -DBOOST_ROOT=C:\boost_1_55_0 -DIMATH_INCLUDE_DIR=C:\appleseed\appleseed-deps\stage\vc14\ilmbase-release\include -DIMATH_MATH_LIBRARY=C:\appleseed\appleseed-deps\stage\vc14\ilmbase-release\lib\Imath-2_2.lib -DIMATH_IEX_LIBRARY=C:\appleseed\appleseed-deps\stage\vc14\ilmbase-release\lib\Iex-2_2.lib -DIMATH_HALF_LIBRARY=C:\appleseed\appleseed-deps\stage\vc14\ilmbase-release\lib\Half.lib -DOPENEXR_INCLUDE_DIR=C:\appleseed\appleseed-deps\stage\vc14\openexr-release\include -DOPENEXR_IMF_LIBRARY=C:\appleseed\appleseed-deps\stage\vc14\openexr-release\lib\IlmImf-2_2.lib -DOPENEXR_THREADS_LIBRARY=C:\appleseed\appleseed-deps\stage\vc14\ilmbase-release\lib\IlmThread-2_2.lib -DAPPLESEED_DEPS_STAGE_DIR=C:\appleseed\appleseed-deps\stage\vc14 ..

Open the Visual Studio solution file (.sln file) that was generated in build and build the plugin in the configuration of your choice (Debug or Release).


On Linux and macOS
------------------

In a Bash shell, inside a plugin's directory, type:

  - With an **end-user** release of appleseed:

        mkdir build
        cd build
        cmake <TODO> ..
        make

  - With a **working copy** of the appleseed repository:

        mkdir build
        cd build
        cmake <TODO> ..
        make
