
#
# This source file is part of appleseed.
# Visit https://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2019 Francois Beaune, The appleseedhq Organization
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#


#--------------------------------------------------------------------------------------------------
# Packages.
#--------------------------------------------------------------------------------------------------

# Qt 5.
find_package (Qt5 REQUIRED COMPONENTS Concurrent Core Network Widgets)


#--------------------------------------------------------------------------------------------------
# Auto-generate encryption file.
#--------------------------------------------------------------------------------------------------

if (NOT EXISTS "${PROJECT_SOURCE_DIR}/src/appleseed.bench/utility/encryptionkey.cpp")
    configure_file (
        ${PROJECT_SOURCE_DIR}/src/appleseed.bench/utility/encryptionkey.cpp.in
        ${PROJECT_SOURCE_DIR}/src/appleseed.bench/utility/encryptionkey.cpp
        COPYONLY
    )
endif ()


#--------------------------------------------------------------------------------------------------
# Source files.
#--------------------------------------------------------------------------------------------------

set (help_about_sources
    help/about/aboutwindow.cpp
    help/about/aboutwindow.h
    help/about/aboutwindow.ui
)
list (APPEND appleseed.bench_sources
    ${help_about_sources}
)
source_group ("help\\about" FILES
    ${help_about_sources}
)

set (main_sources
    main/main.cpp
)
if (WIN32)
    set (main_sources
        ${main_sources}
        main/windowsapp.manifest
    )
endif ()
list (APPEND appleseed.bench_sources
    ${main_sources}
)
source_group ("main" FILES
    ${main_sources}
)

set (mainwindow_rendering_sources
    mainwindow/rendering/qtrenderercontroller.cpp
    mainwindow/rendering/qtrenderercontroller.h
    mainwindow/rendering/qttilecallback.cpp
    mainwindow/rendering/qttilecallback.h
    mainwindow/rendering/renderingmanager.cpp
    mainwindow/rendering/renderingmanager.h
)
list (APPEND appleseed.bench_sources
    ${mainwindow_rendering_sources}
)
source_group ("mainwindow\\rendering" FILES
    ${mainwindow_rendering_sources}
)

set (mainwindow_sources
    mainwindow/applicationsettingswindow.cpp
    mainwindow/applicationsettingswindow.h
    mainwindow/applicationsettingswindow.ui
    mainwindow/constants.h
    mainwindow/mainwindow.cpp
    mainwindow/mainwindow.h
    mainwindow/mainwindow.ui
    mainwindow/rankingswindow.cpp
    mainwindow/rankingswindow.h
    mainwindow/rankingswindow.ui
    mainwindow/renderingtimedisplay.cpp
    mainwindow/renderingtimedisplay.h
    mainwindow/scalingtestparams.h
    mainwindow/throttlingtestparams.h
)
list (APPEND appleseed.bench_sources
    ${mainwindow_sources}
)
source_group ("mainwindow" FILES
    ${mainwindow_sources}
)

set (utility_sources
    utility/backendapi.cpp
    utility/backendapi.h
    utility/encryptionkey.cpp
    utility/encryptionkey.h
    utility/formatrendertime.h
    utility/settingskeys.h
    utility/systeminfo.cpp
    utility/systeminfo.h
    utility/windowbase.cpp
    utility/windowbase.h
)
list (APPEND appleseed.bench_sources
    ${utility_sources}
)
source_group ("utility" FILES
    ${utility_sources}
)

set (resources
    resources/resources.qrc
)
list (APPEND appleseed.bench_sources
    ${resources}
)
source_group ("resources" FILES
    ${resources}
)

if (WIN32)
    set (windows_resources
        resources/windows_resources.rc
    )
    list (APPEND appleseed.bench_sources
        ${windows_resources}
    )
    source_group ("resources" FILES
        ${resources}
        ${windows_resources}
    )
endif ()


#--------------------------------------------------------------------------------------------------
# Generate ui_* files.
#--------------------------------------------------------------------------------------------------

# Collect all .ui files amongst the source files.
filter_list (
    appleseed.bench_ui_files
    "${appleseed.bench_sources}"
    ".*\\\\.ui"
)

QT5_WRAP_UI (appleseed.bench_generated_ui_files
    ${appleseed.bench_ui_files}
)

include_directories (${CMAKE_CURRENT_BINARY_DIR})


#--------------------------------------------------------------------------------------------------
# Generate moc_* files.
#--------------------------------------------------------------------------------------------------

set (moc_options
    --no-notes
)

# Moc .h files.
filter_list (
    appleseed.bench_h_files
    "${appleseed.bench_sources}"
    ".*\\\\.h"
)
QT5_WRAP_CPP (appleseed.bench_generated_moc_h_files
    ${appleseed.bench_h_files}
    OPTIONS ${moc_options}
)

# Moc .cpp files.
filter_list (
    appleseed.bench_cpp_files
    "${appleseed.bench_sources}"
    ".*\\\\.cpp"
)
QT5_WRAP_CPP_CPLUSPLUS_FILES (appleseed.bench_generated_moc_cpp_files
    ${appleseed.bench_cpp_files}
    OPTIONS ${moc_options}
)


#--------------------------------------------------------------------------------------------------
# Compile Qt resource files.
#--------------------------------------------------------------------------------------------------

QT5_ADD_RESOURCES (appleseed.bench_resource_files
    ${resources}
)


#--------------------------------------------------------------------------------------------------
# Target.
#--------------------------------------------------------------------------------------------------

add_executable (appleseed.bench
    ${appleseed.bench_sources}
    ${appleseed.bench_generated_ui_files}
    ${appleseed.bench_generated_moc_h_files}
    ${appleseed.bench_resource_files}
)

set_target_properties (appleseed.bench PROPERTIES FOLDER "Applications")

if (WIN32)
    set_target_properties (appleseed.bench PROPERTIES
        # Build a GUI application instead of a console application on Windows.
        WIN32_EXECUTABLE TRUE
    )
endif ()

if (USE_RPATH_ORIGIN)
    set_target_properties (appleseed.bench PROPERTIES
        INSTALL_RPATH "\$ORIGIN/../lib"
    )
endif ()


#--------------------------------------------------------------------------------------------------
# Include paths.
#--------------------------------------------------------------------------------------------------

include_directories (
    .
    ../appleseed.common
    ../appleseed.qtcommon
    ${OPENCOLORIO_INCLUDE_DIRS}
)


#--------------------------------------------------------------------------------------------------
# Preprocessor definitions.
#--------------------------------------------------------------------------------------------------

set (APPLESEED_BENCH_BACKEND_API_URL "" CACHE STRING "appleseed.bench backend API URL (Base64)")
set (APPLESEED_BENCH_BACKEND_API_KEY "" CACHE STRING "appleseed.bench backend API key (Base64)")
set (APPLESEED_BENCH_ENCRYPTION_KEY "" CACHE STRING "Encryption key (Base64, 24-character long)")

apply_preprocessor_definitions (appleseed.bench)

append_custom_preprocessor_definitions (appleseed.bench
    APPLESEED_BENCH_BACKEND_API_URL=${APPLESEED_BENCH_BACKEND_API_URL}
    APPLESEED_BENCH_BACKEND_API_KEY=${APPLESEED_BENCH_BACKEND_API_KEY}
    APPLESEED_BENCH_ENCRYPTION_KEY=${APPLESEED_BENCH_ENCRYPTION_KEY}
)


#--------------------------------------------------------------------------------------------------
# Static libraries.
#--------------------------------------------------------------------------------------------------

# Static libraries must be specified in order of reverse-dependency.
link_against_platform (appleseed.bench)
link_against_openexr (appleseed.bench)
link_against_ocio (appleseed.bench)

target_link_libraries (appleseed.bench
    appleseed
    appleseed.common
    appleseed.qtcommon
    ${Boost_LIBRARIES}
    Qt5::Core
    Qt5::Concurrent
    Qt5::Network
    Qt5::Widgets
    ${OPENGL_LIBRARY}
)


#--------------------------------------------------------------------------------------------------
# Post-build commands.
#--------------------------------------------------------------------------------------------------

add_copy_target_exe_to_sandbox_command (appleseed.bench)


#--------------------------------------------------------------------------------------------------
# Installation.
#--------------------------------------------------------------------------------------------------

install (TARGETS appleseed.bench
    DESTINATION bin
)

install (FILES ../../sandbox/stylesheets/default.qss
    DESTINATION stylesheets
)

install (FILES ../../sandbox/settings/appleseed.bench.xml
    DESTINATION settings
)

install (DIRECTORY ../../sandbox/bench
    DESTINATION .
)
