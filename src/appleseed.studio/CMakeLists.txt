
#
# This source file is part of appleseed.
# Visit https://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
# Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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

# Boost.
if (NOT PYTHON_MAJOR_VERSION STREQUAL "2")
    message (FATAL_ERROR "Python 2.x needed.")
else ()
    if (Boost_VERSION GREATER_EQUAL 106700)
        find_package (Boost 1.61 REQUIRED COMPONENTS python27)
    else ()
        find_package (Boost 1.61 REQUIRED COMPONENTS python)
    endif ()
endif ()

# OpenGL.
find_package (OpenGL REQUIRED)

# Qt 5.
find_package (Qt5 REQUIRED COMPONENTS Concurrent Core OpenGL Widgets)


#--------------------------------------------------------------------------------------------------
# Source files.
#--------------------------------------------------------------------------------------------------

set (debug_benchmarks_sources
    debug/benchmarks/benchmarkrunnerthread.cpp
    debug/benchmarks/benchmarkrunnerthread.h
    debug/benchmarks/benchmarkwindow.cpp
    debug/benchmarks/benchmarkwindow.h
    debug/benchmarks/benchmarkwindow.ui
)
list (APPEND appleseed.studio_sources
    ${debug_benchmarks_sources}
)
source_group ("debug\\benchmarks" FILES
    ${debug_benchmarks_sources}
)

set (debug_tests_sources
    debug/tests/autodeletetestsuiterepository.h
    debug/tests/qttestlistener.cpp
    debug/tests/qttestlistener.h
    debug/tests/testoutputitem.cpp
    debug/tests/testoutputitem.h
    debug/tests/testoutputwidgetdecorator.cpp
    debug/tests/testoutputwidgetdecorator.h
    debug/tests/testresultwidgetdecorator.cpp
    debug/tests/testresultwidgetdecorator.h
    debug/tests/testrunnerthread.cpp
    debug/tests/testrunnerthread.h
    debug/tests/testwindow.cpp
    debug/tests/testwindow.h
    debug/tests/testwindow.ui
)
list (APPEND appleseed.studio_sources
    ${debug_tests_sources}
)
source_group ("debug\\tests" FILES
    ${debug_tests_sources}
)

set (help_about_sources
    help/about/aboutwindow.cpp
    help/about/aboutwindow.h
    help/about/aboutwindow.ui
)
list (APPEND appleseed.studio_sources
    ${help_about_sources}
)
source_group ("help\\about" FILES
    ${help_about_sources}
)

set (main_sources
    main/commandlinehandler.cpp
    main/commandlinehandler.h
    main/main.cpp
)
if (WIN32)
    set (main_sources
        ${main_sources}
        main/windowsapp.manifest
    )
endif ()
list (APPEND appleseed.studio_sources
    ${main_sources}
)
source_group ("main" FILES
    ${main_sources}
)

set (mainwindow_project_sources
    mainwindow/project/assemblycollectionitem.cpp
    mainwindow/project/assemblycollectionitem.h
    mainwindow/project/assemblyinstanceitem.cpp
    mainwindow/project/assemblyinstanceitem.h
    mainwindow/project/assemblyitem.cpp
    mainwindow/project/assemblyitem.h
    mainwindow/project/attributeeditor.cpp
    mainwindow/project/attributeeditor.h
    mainwindow/project/basegroupitem.cpp
    mainwindow/project/basegroupitem.h
    mainwindow/project/collectionitem.h
    mainwindow/project/collectionitembase.h
    mainwindow/project/customentityui.h
    mainwindow/project/entityactions.h
    mainwindow/project/entitybrowser.cpp
    mainwindow/project/entitybrowser.h
    mainwindow/project/entitybrowserwindow.cpp
    mainwindow/project/entitybrowserwindow.h
    mainwindow/project/entitybrowserwindow.ui
    mainwindow/project/entitycreatorbase.cpp
    mainwindow/project/entitycreatorbase.h
    mainwindow/project/entityeditor.cpp
    mainwindow/project/entityeditor.h
    mainwindow/project/entityeditorcontext.h
    mainwindow/project/entityeditorformfactorybase.cpp
    mainwindow/project/entityeditorformfactorybase.h
    mainwindow/project/entityeditorwindow.cpp
    mainwindow/project/entityeditorwindow.h
    mainwindow/project/entityeditorwindow.ui
    mainwindow/project/entityinputwidget.cpp
    mainwindow/project/entityinputwidget.h
    mainwindow/project/entityitem.h
    mainwindow/project/entityitembase.h
    mainwindow/project/exceptioninvalidentityname.h
    mainwindow/project/fixedmodelentityeditorformfactory.h
    mainwindow/project/fixedmodelentityitem.h
    mainwindow/project/frameitem.cpp
    mainwindow/project/frameitem.h
    mainwindow/project/ientityvalueprovider.h
    mainwindow/project/instancecollectionitem.h
    mainwindow/project/itembase.cpp
    mainwindow/project/itembase.h
    mainwindow/project/itemregistry.cpp
    mainwindow/project/itemregistry.h
    mainwindow/project/materialassignmenteditorwindow.cpp
    mainwindow/project/materialassignmenteditorwindow.h
    mainwindow/project/materialassignmenteditorwindow.ui
    mainwindow/project/materialcollectionitem.cpp
    mainwindow/project/materialcollectionitem.h
    mainwindow/project/materialitem.cpp
    mainwindow/project/materialitem.h
    mainwindow/project/multimodelcollectionitem.h
    mainwindow/project/multimodelentityeditorformfactory.h
    mainwindow/project/multimodelentityitem.h
    mainwindow/project/objectcollectionitem.cpp
    mainwindow/project/objectcollectionitem.h
    mainwindow/project/objectinstanceitem.cpp
    mainwindow/project/objectinstanceitem.h
    mainwindow/project/objectitem.cpp
    mainwindow/project/objectitem.h
    mainwindow/project/outputitem.cpp
    mainwindow/project/outputitem.h
    mainwindow/project/projectbuilder.cpp
    mainwindow/project/projectbuilder.h
    mainwindow/project/projectexplorer.cpp
    mainwindow/project/projectexplorer.h
    mainwindow/project/projectitem.cpp
    mainwindow/project/projectitem.h
    mainwindow/project/sceneitem.cpp
    mainwindow/project/sceneitem.h
    mainwindow/project/searchpathswindow.cpp
    mainwindow/project/searchpathswindow.h
    mainwindow/project/searchpathswindow.ui
    mainwindow/project/singlemodelcollectionitem.h
    mainwindow/project/singlemodelentityeditorformfactory.cpp
    mainwindow/project/singlemodelentityeditorformfactory.h
    mainwindow/project/singlemodelentityitem.h
    mainwindow/project/texturecollectionitem.cpp
    mainwindow/project/texturecollectionitem.h
    mainwindow/project/textureinstanceitem.cpp
    mainwindow/project/textureinstanceitem.h
    mainwindow/project/textureitem.cpp
    mainwindow/project/textureitem.h
    mainwindow/project/tools.cpp
    mainwindow/project/tools.h
)
list (APPEND appleseed.studio_sources
    ${mainwindow_project_sources}
)
source_group ("mainwindow\\project" FILES
    ${mainwindow_project_sources}
)

set (mainwindow_pythonconsole_sources
    mainwindow/pythonconsole/linenumberarea.cpp
    mainwindow/pythonconsole/linenumberarea.h
    mainwindow/pythonconsole/outputredirector.cpp
    mainwindow/pythonconsole/outputredirector.h
    mainwindow/pythonconsole/pythonconsolewidget.cpp
    mainwindow/pythonconsole/pythonconsolewidget.h
    mainwindow/pythonconsole/pythoneditor.cpp
    mainwindow/pythonconsole/pythoneditor.h
    mainwindow/pythonconsole/pythonhighlighter.cpp
    mainwindow/pythonconsole/pythonhighlighter.h
    mainwindow/pythonconsole/pythonoutput.cpp
    mainwindow/pythonconsole/pythonoutput.h
    mainwindow/pythonconsole/zoomableplaintextedit.cpp
    mainwindow/pythonconsole/zoomableplaintextedit.h
)
list (APPEND appleseed.studio_sources
    ${mainwindow_pythonconsole_sources}
)
source_group ("mainwindow\\pythonconsole" FILES
    ${mainwindow_pythonconsole_sources}
)

set (mainwindow_rendering_sources
    mainwindow/rendering/cameracontroller.cpp
    mainwindow/rendering/cameracontroller.h
    mainwindow/rendering/lightpathspickinghandler.cpp
    mainwindow/rendering/lightpathspickinghandler.h
    mainwindow/rendering/lightpathstab.cpp
    mainwindow/rendering/lightpathstab.h
    mainwindow/rendering/lightpathswidget.cpp
    mainwindow/rendering/lightpathswidget.h
    mainwindow/rendering/materialdrophandler.cpp
    mainwindow/rendering/materialdrophandler.h
    mainwindow/rendering/pixelcolortracker.cpp
    mainwindow/rendering/pixelcolortracker.h
    mainwindow/rendering/pixelinspectorhandler.cpp
    mainwindow/rendering/pixelinspectorhandler.h
    mainwindow/rendering/qtrenderercontroller.cpp
    mainwindow/rendering/qtrenderercontroller.h
    mainwindow/rendering/qttilecallback.cpp
    mainwindow/rendering/qttilecallback.h
    mainwindow/rendering/renderclipboardhandler.cpp
    mainwindow/rendering/renderclipboardhandler.h
    mainwindow/rendering/renderingmanager.cpp
    mainwindow/rendering/renderingmanager.h
    mainwindow/rendering/renderregionhandler.cpp
    mainwindow/rendering/renderregionhandler.h
    mainwindow/rendering/rendertab.cpp
    mainwindow/rendering/rendertab.h
    mainwindow/rendering/scenepickinghandler.cpp
    mainwindow/rendering/scenepickinghandler.h
)
list (APPEND appleseed.studio_sources
    ${mainwindow_rendering_sources}
)
source_group ("mainwindow\\rendering" FILES
    ${mainwindow_rendering_sources}
)

set (mainwindow_sources
    mainwindow/applicationsettingswindow.cpp
    mainwindow/applicationsettingswindow.h
    mainwindow/applicationsettingswindow.ui
    mainwindow/collapsiblesectionwidget.cpp
    mainwindow/collapsiblesectionwidget.h
    mainwindow/configurationmanagerwindow.cpp
    mainwindow/configurationmanagerwindow.h
    mainwindow/configurationmanagerwindow.ui
    mainwindow/falsecolorswindow.cpp
    mainwindow/falsecolorswindow.h
    mainwindow/falsecolorswindow.ui
    mainwindow/mainwindow.cpp
    mainwindow/mainwindow.h
    mainwindow/mainwindow.ui
    mainwindow/minimizebutton.cpp
    mainwindow/minimizebutton.h
    mainwindow/renderingsettingswindow.cpp
    mainwindow/renderingsettingswindow.h
    mainwindow/renderingsettingswindow.ui
    mainwindow/statusbar.cpp
    mainwindow/statusbar.h
)
list (APPEND appleseed.studio_sources
    ${mainwindow_sources}
)
source_group ("mainwindow" FILES
    ${mainwindow_sources}
)

set (python_sources
    python/module.cpp
    python/pythoninterpreter.cpp
    python/pythoninterpreter.h
)
list (APPEND appleseed.studio_sources
    ${python_sources}
)
source_group ("python" FILES
    ${python_sources}
)

set (studio_py_sources
    python/studio/__init__.py
    python/studio/plugins.py
    python/studio/ui.py
)
list (APPEND appleseed.studio_sources
    ${studio_py_sources}
)
source_group ("python\\studio" FILES
    ${studio_py_sources}
)

set (utility_sources
    utility/inputwidgetproxies.cpp
    utility/inputwidgetproxies.h
    utility/settingskeys.h
    utility/windowbase.cpp
    utility/windowbase.h
)
list (APPEND appleseed.studio_sources
    ${utility_sources}
)
source_group ("utility" FILES
    ${utility_sources}
)

set (resources
    resources/resources.qrc
)
list (APPEND appleseed.studio_sources
    ${resources}
)
source_group ("resources" FILES
    ${resources}
)

if (WIN32)
    set (windows_resources
        resources/windows_resources.rc
    )
    list (APPEND appleseed.studio_sources
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
    appleseed.studio_ui_files
    "${appleseed.studio_sources}"
    ".*\\\\.ui"
)

QT5_WRAP_UI (appleseed.studio_generated_ui_files
    ${appleseed.studio_ui_files}
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
    appleseed.studio_h_files
    "${appleseed.studio_sources}"
    ".*\\\\.h"
)
QT5_WRAP_CPP (appleseed.studio_generated_moc_h_files
    ${appleseed.studio_h_files}
    OPTIONS ${moc_options}
)

# Moc .cpp files.
filter_list (
    appleseed.studio_cpp_files
    "${appleseed.studio_sources}"
    ".*\\\\.cpp"
)
QT5_WRAP_CPP_CPLUSPLUS_FILES (appleseed.studio_generated_moc_cpp_files
    ${appleseed.studio_cpp_files}
    OPTIONS ${moc_options}
)


#--------------------------------------------------------------------------------------------------
# Compile Qt resource files.
#--------------------------------------------------------------------------------------------------

QT5_ADD_RESOURCES (appleseed.studio_resource_files
    ${resources}
)


#--------------------------------------------------------------------------------------------------
# Target.
#--------------------------------------------------------------------------------------------------

add_executable (appleseed.studio
    ${appleseed.studio_sources}
    $<TARGET_OBJECTS:appleseed.py2.obj>
    ${appleseed.studio_generated_ui_files}
    ${appleseed.studio_generated_moc_h_files}
    ${appleseed.studio_resource_files}
)

set_target_properties (appleseed.studio PROPERTIES FOLDER "Applications")

if (WIN32)
    set_target_properties (appleseed.studio PROPERTIES
        # Build a GUI application instead of a console application on Windows.
        WIN32_EXECUTABLE TRUE
    )
endif ()

if (USE_RPATH_ORIGIN)
    set_target_properties (appleseed.studio PROPERTIES
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
    ${PYTHON_INCLUDE_DIRS}
)


#--------------------------------------------------------------------------------------------------
# Preprocessor definitions.
#--------------------------------------------------------------------------------------------------

apply_preprocessor_definitions (appleseed.studio)


#--------------------------------------------------------------------------------------------------
# Static libraries.
#--------------------------------------------------------------------------------------------------

# Static libraries must be specified in order of reverse-dependency.
link_against_platform (appleseed.studio)
link_against_openexr (appleseed.studio)
link_against_ocio (appleseed.studio)

target_link_libraries (appleseed.studio
    appleseed
    appleseed.common
    appleseed.qtcommon
    ${Boost_LIBRARIES}
    Qt5::Core
    Qt5::Concurrent
    Qt5::OpenGL
    Qt5::Widgets
    ${OPENGL_LIBRARY}
    ${PYTHON_LIBRARIES}
)


#--------------------------------------------------------------------------------------------------
# Post-build commands.
#--------------------------------------------------------------------------------------------------

add_custom_target (
    appleseed.studio.copy_py_files ALL
    VERBATIM
)

set_target_properties (appleseed.studio.copy_py_files PROPERTIES FOLDER "Contents")
set_target_progress_message (appleseed.studio.copy_py_files "Copying appleseed.studio Python files...")

add_copy_studio_py_file_to_sandbox_py_module_command (${PROJECT_SOURCE_DIR}/src/appleseed.studio/python/studio/__init__.py appleseed.studio)
add_copy_studio_py_file_to_sandbox_py_module_command (${PROJECT_SOURCE_DIR}/src/appleseed.studio/python/studio/ui.py appleseed.studio)
add_copy_studio_py_file_to_sandbox_py_module_command (${PROJECT_SOURCE_DIR}/src/appleseed.studio/python/studio/plugins.py appleseed.studio)

add_copy_target_exe_to_sandbox_command (appleseed.studio)


#--------------------------------------------------------------------------------------------------
# Installation.
#--------------------------------------------------------------------------------------------------

install (TARGETS appleseed.studio
    DESTINATION bin
)

install (FILES ../../sandbox/stylesheets/default.qss
    DESTINATION stylesheets
)

install (FILES ../../sandbox/settings/appleseed.studio.xml
    DESTINATION settings
)

install (DIRECTORY ../../sandbox/ocio
    DESTINATION .
)

install (DIRECTORY ../../sandbox/studio
    DESTINATION .
)

install (DIRECTORY ../../sandbox/lib/python/site-packages DESTINATION "lib/python${PYTHON_MAJOR_VERSION}.${PYTHON_MINOR_VERSION}")
install (DIRECTORY python/studio DESTINATION "lib/python${PYTHON_MAJOR_VERSION}.${PYTHON_MINOR_VERSION}/appleseed")
