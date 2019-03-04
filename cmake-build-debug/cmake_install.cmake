# Install script for directory: /Users/work/Projects/appleseed-build/appleseed

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "/usr/local")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "Debug")
  endif()
  message(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
endif()

# Set the component getting installed.
if(NOT CMAKE_INSTALL_COMPONENT)
  if(COMPONENT)
    message(STATUS "Install component: \"${COMPONENT}\"")
    set(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  else()
    set(CMAKE_INSTALL_COMPONENT)
  endif()
endif()

# Is this installation the result of a crosscompile?
if(NOT DEFINED CMAKE_CROSSCOMPILING)
  set(CMAKE_CROSSCOMPILING "FALSE")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/bin" TYPE FILE FILES
    "/Users/work/Projects/appleseed-build/appleseed/scripts/cleanmany.py"
    "/Users/work/Projects/appleseed-build/appleseed/scripts/convertmany.py"
    "/Users/work/Projects/appleseed-build/appleseed/scripts/rendermanager.py"
    "/Users/work/Projects/appleseed-build/appleseed/scripts/rendermany.py"
    "/Users/work/Projects/appleseed-build/appleseed/scripts/rendernode.py"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/schemas" TYPE FILE FILES
    "/Users/work/Projects/appleseed-build/appleseed/sandbox/schemas/settings.xsd"
    "/Users/work/Projects/appleseed-build/appleseed/sandbox/schemas/project.xsd"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/bin" TYPE FILE FILES
    "/usr/local/bin/oslc"
    "/usr/local/bin/oslinfo"
    "/usr/local/bin/maketx"
    "/usr/local/bin/oiiotool"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/." TYPE DIRECTORY FILES "/Users/work/Projects/appleseed-build/appleseed/sandbox/shaders")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/." TYPE DIRECTORY FILES
    "/Users/work/Projects/appleseed-build/appleseed/sandbox/docs"
    "/Users/work/Projects/appleseed-build/appleseed/sandbox/icons"
    "/Users/work/Projects/appleseed-build/appleseed/sandbox/settings"
    "/Users/work/Projects/appleseed-build/appleseed/sandbox/share"
    "/Users/work/Projects/appleseed-build/appleseed/sandbox/stylesheets"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  FILE (MAKE_DIRECTORY /usr/local/images)
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  FILE (MAKE_DIRECTORY /usr/local/images/autosave)
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/." TYPE DIRECTORY FILES "/Users/work/Projects/appleseed-build/appleseed/sandbox/samples")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/tests" TYPE DIRECTORY FILES
    "/Users/work/Projects/appleseed-build/appleseed/sandbox/tests/unit benchmarks"
    "/Users/work/Projects/appleseed-build/appleseed/sandbox/tests/unit tests"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include" TYPE DIRECTORY FILES "/Users/work/Projects/appleseed-build/appleseed/src/appleseed/foundation" FILES_MATCHING REGEX "/[^/]*\\.h$")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include" TYPE DIRECTORY FILES "/Users/work/Projects/appleseed-build/appleseed/src/appleseed/main" FILES_MATCHING REGEX "/[^/]*\\.h$")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include" TYPE DIRECTORY FILES "/Users/work/Projects/appleseed-build/appleseed/src/appleseed/renderer" FILES_MATCHING REGEX "/[^/]*\\.h$")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for each subdirectory.
  include("/Users/work/Projects/appleseed-build/appleseed/cmake-build-debug/src/thirdparty/bcd/cmake_install.cmake")
  include("/Users/work/Projects/appleseed-build/appleseed/cmake-build-debug/src/appleseed/cmake_install.cmake")
  include("/Users/work/Projects/appleseed-build/appleseed/cmake-build-debug/src/appleseed.shaders/cmake_install.cmake")
  include("/Users/work/Projects/appleseed-build/appleseed/cmake-build-debug/src/appleseed.shared/cmake_install.cmake")
  include("/Users/work/Projects/appleseed-build/appleseed/cmake-build-debug/src/appleseed.cli/cmake_install.cmake")
  include("/Users/work/Projects/appleseed-build/appleseed/cmake-build-debug/src/appleseed.python/cmake_install.cmake")
  include("/Users/work/Projects/appleseed-build/appleseed/cmake-build-debug/src/appleseed.studio/cmake_install.cmake")
  include("/Users/work/Projects/appleseed-build/appleseed/cmake-build-debug/src/tools/animatecamera/cmake_install.cmake")
  include("/Users/work/Projects/appleseed-build/appleseed/cmake-build-debug/src/tools/convertmeshfile/cmake_install.cmake")
  include("/Users/work/Projects/appleseed-build/appleseed/cmake-build-debug/src/tools/denoiser/cmake_install.cmake")
  include("/Users/work/Projects/appleseed-build/appleseed/cmake-build-debug/src/tools/dumpmetadata/cmake_install.cmake")
  include("/Users/work/Projects/appleseed-build/appleseed/cmake-build-debug/src/tools/makefluffy/cmake_install.cmake")
  include("/Users/work/Projects/appleseed-build/appleseed/cmake-build-debug/src/tools/projecttool/cmake_install.cmake")

endif()

if(CMAKE_INSTALL_COMPONENT)
  set(CMAKE_INSTALL_MANIFEST "install_manifest_${CMAKE_INSTALL_COMPONENT}.txt")
else()
  set(CMAKE_INSTALL_MANIFEST "install_manifest.txt")
endif()

string(REPLACE ";" "\n" CMAKE_INSTALL_MANIFEST_CONTENT
       "${CMAKE_INSTALL_MANIFEST_FILES}")
file(WRITE "/Users/work/Projects/appleseed-build/appleseed/cmake-build-debug/${CMAKE_INSTALL_MANIFEST}"
     "${CMAKE_INSTALL_MANIFEST_CONTENT}")
