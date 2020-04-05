
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

# OpenGL.
find_package (OpenGL REQUIRED)

# Qt 5.
find_package (Qt5 REQUIRED COMPONENTS Concurrent Core OpenGL Widgets)


#--------------------------------------------------------------------------------------------------
# Source files.
#--------------------------------------------------------------------------------------------------

set (meta_tests_sources
    meta/tests/test_encryption.cpp
    meta/tests/test_interop.cpp
    meta/tests/test_projectmanager.cpp
)
list (APPEND appleseed.qtcommon_sources
    ${meta_tests_sources}
)
source_group ("meta\\tests" FILES
    ${meta_tests_sources}
)

set (project_sources
    project/projectmanager.cpp
    project/projectmanager.h
)
list (APPEND appleseed.qtcommon_sources
    ${project_sources}
)
source_group ("project" FILES
    ${project_sources}
)

set (utility_sources
    utility/encryption.cpp
    utility/encryption.h
    utility/interop.h
    utility/miscellaneous.cpp
    utility/miscellaneous.h
)
list (APPEND appleseed.qtcommon_sources
    ${utility_sources}
)
source_group ("utility" FILES
    ${utility_sources}
)

set (widgets_sources
    widgets/chartwidget.cpp
    widgets/chartwidget.h
    widgets/doubleslider.cpp
    widgets/doubleslider.h
    widgets/foldablepanelwidget.cpp
    widgets/foldablepanelwidget.h
    widgets/icapturablewidget.h
    widgets/logwidget.cpp
    widgets/logwidget.h
    widgets/mousecoordinatestracker.cpp
    widgets/mousecoordinatestracker.h
    widgets/mousewheelfocuseventfilter.cpp
    widgets/mousewheelfocuseventfilter.h
    widgets/qtlogtarget.cpp
    widgets/qtlogtarget.h
    widgets/renderwidget.cpp
    widgets/renderwidget.h
    widgets/scrollareapanhandler.cpp
    widgets/scrollareapanhandler.h
    widgets/treewidget.cpp
    widgets/treewidget.h
    widgets/widgetzoomhandler.cpp
    widgets/widgetzoomhandler.h
)
list (APPEND appleseed.qtcommon_sources
    ${widgets_sources}
)
source_group ("widgets" FILES
    ${widgets_sources}
)

set (resources
    resources/qtcommonresources.qrc
)
list (APPEND appleseed.qtcommon_sources
    ${resources}
)
source_group ("resources" FILES
    ${resources}
)


#--------------------------------------------------------------------------------------------------
# Generate ui_* files.
#--------------------------------------------------------------------------------------------------

# Collect all .ui files amongst the source files.
filter_list (
    appleseed.qtcommon_ui_files
    "${appleseed.qtcommon_sources}"
    ".*\\\\.ui"
)

QT5_WRAP_UI (appleseed.qtcommon_generated_ui_files
    ${appleseed.qtcommon_ui_files}
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
    appleseed.qtcommon_h_files
    "${appleseed.qtcommon_sources}"
    ".*\\\\.h"
)
QT5_WRAP_CPP (appleseed.qtcommon_generated_moc_h_files
    ${appleseed.qtcommon_h_files}
    OPTIONS ${moc_options}
)

# Moc .cpp files.
filter_list (
    appleseed.qtcommon_cpp_files
    "${appleseed.qtcommon_sources}"
    ".*\\\\.cpp"
)
QT5_WRAP_CPP_CPLUSPLUS_FILES (appleseed.qtcommon_generated_moc_cpp_files
    ${appleseed.qtcommon_cpp_files}
    OPTIONS ${moc_options}
)


#--------------------------------------------------------------------------------------------------
# Compile Qt resource files.
#--------------------------------------------------------------------------------------------------

QT5_ADD_RESOURCES (appleseed.qtcommon_resource_files
    ${resources}
)


#--------------------------------------------------------------------------------------------------
# Target.
#--------------------------------------------------------------------------------------------------

add_library (appleseed.qtcommon STATIC
    ${appleseed.qtcommon_sources}
    ${appleseed.qtcommon_generated_ui_files}
    ${appleseed.qtcommon_generated_moc_h_files}
    ${appleseed.qtcommon_resource_files}
)

set_target_properties (appleseed.qtcommon PROPERTIES FOLDER "Core")


#--------------------------------------------------------------------------------------------------
# Include paths.
#--------------------------------------------------------------------------------------------------

include_directories (
    .
    ../appleseed.common
    ${Qt5Core_INCLUDE_DIRS}
    ${Qt5Concurrent_INCLUDE_DIRS}
    ${Qt5OpenGL_INCLUDE_DIRS}
    ${Qt5Widgets_INCLUDE_DIRS}
)


#--------------------------------------------------------------------------------------------------
# Preprocessor definitions.
#--------------------------------------------------------------------------------------------------

apply_preprocessor_definitions (appleseed.qtcommon)


#--------------------------------------------------------------------------------------------------
# Installation.
#--------------------------------------------------------------------------------------------------

install (TARGETS appleseed.qtcommon
    DESTINATION lib
)
