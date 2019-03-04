# Install script for directory: /Users/work/Projects/appleseed-build/appleseed/src/appleseed.studio

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
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/bin" TYPE EXECUTABLE FILES "/Users/work/Projects/appleseed-build/appleseed/cmake-build-debug/src/appleseed.studio/appleseed.studio")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/appleseed.studio" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/appleseed.studio")
    execute_process(COMMAND "/usr/bin/install_name_tool"
      -change "/Users/work/Projects/appleseed-build/appleseed/cmake-build-debug/src/appleseed.shared/libappleseed.shared.dylib" "libappleseed.shared.dylib"
      -change "/Users/work/Projects/appleseed-build/appleseed/cmake-build-debug/src/appleseed/libappleseed.dylib" "libappleseed.dylib"
      "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/appleseed.studio")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/appleseed.studio")
    endif()
  endif()
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/stylesheets" TYPE FILE FILES "/Users/work/Projects/appleseed-build/appleseed/src/appleseed.studio/../../sandbox/stylesheets/default.qss")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/settings" TYPE FILE FILES "/Users/work/Projects/appleseed-build/appleseed/src/appleseed.studio/../../sandbox/settings/appleseed.studio.xml")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/." TYPE DIRECTORY FILES "/Users/work/Projects/appleseed-build/appleseed/src/appleseed.studio/../../sandbox/ocio")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/." TYPE DIRECTORY FILES "/Users/work/Projects/appleseed-build/appleseed/src/appleseed.studio/../../sandbox/seexpr")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/." TYPE DIRECTORY FILES "/Users/work/Projects/appleseed-build/appleseed/src/appleseed.studio/../../sandbox/studio")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/python2.7" TYPE DIRECTORY FILES "/Users/work/Projects/appleseed-build/appleseed/src/appleseed.studio/../../sandbox/lib/python/site-packages")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/python2.7/appleseed" TYPE DIRECTORY FILES "/Users/work/Projects/appleseed-build/appleseed/src/appleseed.studio/python/studio")
endif()

