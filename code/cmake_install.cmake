# Install script for directory: /home/rishav/Desktop/appleseed

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
    set(CMAKE_INSTALL_CONFIG_NAME "Release")
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

# Install shared libraries without execute permission?
if(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)
  set(CMAKE_INSTALL_SO_NO_EXE "1")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/bin" TYPE FILE FILES
    "/home/rishav/Desktop/appleseed/scripts/cleanmany.py"
    "/home/rishav/Desktop/appleseed/scripts/convertmany.py"
    "/home/rishav/Desktop/appleseed/scripts/rendermanager.py"
    "/home/rishav/Desktop/appleseed/scripts/rendermany.py"
    "/home/rishav/Desktop/appleseed/scripts/rendernode.py"
    )
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/schemas" TYPE FILE FILES
    "/home/rishav/Desktop/appleseed/sandbox/schemas/settings.xsd"
    "/home/rishav/Desktop/appleseed/sandbox/schemas/project.xsd"
    )
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/bin" TYPE FILE FILES
    "/home/rishav/Desktop/prebuilt-linux-deps/bin/oslc"
    "/home/rishav/Desktop/prebuilt-linux-deps/bin/oslinfo"
    "/home/rishav/Desktop/prebuilt-linux-deps/bin/maketx"
    "/home/rishav/Desktop/prebuilt-linux-deps/bin/oiiotool"
    )
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/." TYPE DIRECTORY FILES "/home/rishav/Desktop/appleseed/sandbox/shaders")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/." TYPE DIRECTORY FILES
    "/home/rishav/Desktop/appleseed/sandbox/docs"
    "/home/rishav/Desktop/appleseed/sandbox/icons"
    "/home/rishav/Desktop/appleseed/sandbox/settings"
    "/home/rishav/Desktop/appleseed/sandbox/share"
    "/home/rishav/Desktop/appleseed/sandbox/stylesheets"
    )
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  FILE (MAKE_DIRECTORY /usr/local/images)
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  FILE (MAKE_DIRECTORY /usr/local/images/autosave)
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/." TYPE DIRECTORY FILES "/home/rishav/Desktop/appleseed/sandbox/samples")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/tests" TYPE DIRECTORY FILES
    "/home/rishav/Desktop/appleseed/sandbox/tests/unit benchmarks"
    "/home/rishav/Desktop/appleseed/sandbox/tests/unit tests"
    )
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include" TYPE DIRECTORY FILES "/home/rishav/Desktop/appleseed/src/appleseed/foundation" FILES_MATCHING REGEX "/[^/]*\\.h$")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include" TYPE DIRECTORY FILES "/home/rishav/Desktop/appleseed/src/appleseed/main" FILES_MATCHING REGEX "/[^/]*\\.h$")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include" TYPE DIRECTORY FILES "/home/rishav/Desktop/appleseed/src/appleseed/renderer" FILES_MATCHING REGEX "/[^/]*\\.h$")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for each subdirectory.
  include("/home/rishav/Desktop/appleseed/code/src/thirdparty/bcd/cmake_install.cmake")
  include("/home/rishav/Desktop/appleseed/code/src/thirdparty/lz4/cmake_install.cmake")
  include("/home/rishav/Desktop/appleseed/code/src/appleseed/cmake_install.cmake")
  include("/home/rishav/Desktop/appleseed/code/src/appleseed.shaders/cmake_install.cmake")
  include("/home/rishav/Desktop/appleseed/code/src/appleseed.shared/cmake_install.cmake")
  include("/home/rishav/Desktop/appleseed/code/src/appleseed.cli/cmake_install.cmake")
  include("/home/rishav/Desktop/appleseed/code/src/appleseed.python/cmake_install.cmake")
  include("/home/rishav/Desktop/appleseed/code/src/appleseed.studio/cmake_install.cmake")
  include("/home/rishav/Desktop/appleseed/code/src/tools/animatecamera/cmake_install.cmake")
  include("/home/rishav/Desktop/appleseed/code/src/tools/convertmeshfile/cmake_install.cmake")
  include("/home/rishav/Desktop/appleseed/code/src/tools/denoiser/cmake_install.cmake")
  include("/home/rishav/Desktop/appleseed/code/src/tools/dumpmetadata/cmake_install.cmake")
  include("/home/rishav/Desktop/appleseed/code/src/tools/makefluffy/cmake_install.cmake")
  include("/home/rishav/Desktop/appleseed/code/src/tools/projecttool/cmake_install.cmake")

endif()

if(CMAKE_INSTALL_COMPONENT)
  set(CMAKE_INSTALL_MANIFEST "install_manifest_${CMAKE_INSTALL_COMPONENT}.txt")
else()
  set(CMAKE_INSTALL_MANIFEST "install_manifest.txt")
endif()

string(REPLACE ";" "\n" CMAKE_INSTALL_MANIFEST_CONTENT
       "${CMAKE_INSTALL_MANIFEST_FILES}")
file(WRITE "/home/rishav/Desktop/appleseed/code/${CMAKE_INSTALL_MANIFEST}"
     "${CMAKE_INSTALL_MANIFEST_CONTENT}")
