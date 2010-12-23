
#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2010 Francois Beaune
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


#
# Find Packages.
#

# OpenGL.
find_package(OpenGL REQUIRED)

# Qt 4.
find_package(Qt4 REQUIRED)
set(QT_USE_QTOPENGL TRUE)
include(${QT_USE_FILE})


#
# Source Files.
#

set(debug_benchmarks_sources
    debug/benchmarks/benchmarkrunnerthread.cpp
    debug/benchmarks/benchmarkrunnerthread.h
    debug/benchmarks/benchmarkwindow.cpp
    debug/benchmarks/benchmarkwindow.h
    debug/benchmarks/benchmarkwindow.ui
)
list(APPEND appleseed.studio_sources
    ${debug_benchmarks_sources}
)
source_group("debug\\benchmarks" FILES
    ${debug_benchmarks_sources}
)

set(debug_tests_sources
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
list(APPEND appleseed.studio_sources
    ${debug_tests_sources}
)
source_group("debug\\tests" FILES
    ${debug_tests_sources}
)

set(help_about_sources
    help/about/aboutwindow.cpp
    help/about/aboutwindow.h
    help/about/aboutwindow.ui
)
list(APPEND appleseed.studio_sources
    ${help_about_sources}
)
source_group("help\\about" FILES
    ${help_about_sources}
)

set(main_sources
    main/main.cpp
)
list(APPEND appleseed.studio_sources
    ${main_sources}
)
source_group("main" FILES
    ${main_sources}
)

set(mainwindow_project_sources
    mainwindow/project/assemblycollectionitem.cpp
    mainwindow/project/assemblycollectionitem.h
    mainwindow/project/assemblyentitybrowser.cpp
    mainwindow/project/assemblyentitybrowser.h
    mainwindow/project/assemblyinstancecollectionitem.cpp
    mainwindow/project/assemblyinstancecollectionitem.h
    mainwindow/project/assemblyitem.cpp
    mainwindow/project/assemblyitem.h
    mainwindow/project/bsdfcollectionitem.cpp
    mainwindow/project/bsdfcollectionitem.h
    mainwindow/project/collectionitem.cpp
    mainwindow/project/collectionitem.h
    mainwindow/project/collectionitembase.cpp
    mainwindow/project/collectionitembase.h
    mainwindow/project/colorcollectionitem.cpp
    mainwindow/project/colorcollectionitem.h
    mainwindow/project/edfcollectionitem.cpp
    mainwindow/project/edfcollectionitem.h
    mainwindow/project/entitybrowserwindow.cpp
    mainwindow/project/entitybrowserwindow.h
    mainwindow/project/entitybrowserwindow.ui
    mainwindow/project/entitycreatorbase.cpp
    mainwindow/project/entitycreatorbase.h
    mainwindow/project/entityeditorformfactorybase.cpp
    mainwindow/project/entityeditorformfactorybase.h
    mainwindow/project/entityeditorwindow.cpp
    mainwindow/project/entityeditorwindow.h
    mainwindow/project/entityeditorwindow.ui
    mainwindow/project/entityitem.h
    mainwindow/project/entityitembase.cpp
    mainwindow/project/entityitembase.h
    mainwindow/project/entitynames.h
    mainwindow/project/environmentedfcollectionitem.cpp
    mainwindow/project/environmentedfcollectionitem.h
    mainwindow/project/environmentshadercollectionitem.cpp
    mainwindow/project/environmentshadercollectionitem.h
    mainwindow/project/itembase.cpp
    mainwindow/project/itembase.h
    mainwindow/project/itemtypemap.h
    mainwindow/project/lightcollectionitem.cpp
    mainwindow/project/lightcollectionitem.h
    mainwindow/project/materialcollectionitem.cpp
    mainwindow/project/materialcollectionitem.h
    mainwindow/project/multimodelentityeditorformfactory.h
    mainwindow/project/multimodelentityitem.h
    mainwindow/project/objectcollectionitem.cpp
    mainwindow/project/objectcollectionitem.h
    mainwindow/project/objectinstancecollectionitem.cpp
    mainwindow/project/objectinstancecollectionitem.h
    mainwindow/project/objectinstanceitem.cpp
    mainwindow/project/objectinstanceitem.h
    mainwindow/project/projectbuilder.cpp
    mainwindow/project/projectbuilder.h
    mainwindow/project/projectexplorer.cpp
    mainwindow/project/projectexplorer.h
    mainwindow/project/projectmanager.cpp
    mainwindow/project/projectmanager.h
    mainwindow/project/projecttree.cpp
    mainwindow/project/projecttree.h
    mainwindow/project/singlemodelentityeditorformfactory.cpp
    mainwindow/project/singlemodelentityeditorformfactory.h
    mainwindow/project/singlemodelentityitem.h
    mainwindow/project/surfaceshadercollectionitem.cpp
    mainwindow/project/surfaceshadercollectionitem.h
    mainwindow/project/texturecollectionitem.cpp
    mainwindow/project/texturecollectionitem.h
    mainwindow/project/textureinstancecollectionitem.cpp
    mainwindow/project/textureinstancecollectionitem.h
    mainwindow/project/tools.cpp
    mainwindow/project/tools.h
)
list(APPEND appleseed.studio_sources
    ${mainwindow_project_sources}
)
source_group("mainwindow\\project" FILES
    ${mainwindow_project_sources}
)

set(mainwindow_rendering_sources
    mainwindow/rendering/cameracontroller.cpp
    mainwindow/rendering/cameracontroller.h
    mainwindow/rendering/glrenderwidget.cpp
    mainwindow/rendering/glrenderwidget.h
    mainwindow/rendering/irenderwidget.h
    mainwindow/rendering/qtrenderercontroller.cpp
    mainwindow/rendering/qtrenderercontroller.h
    mainwindow/rendering/qttilecallbackfactory.cpp
    mainwindow/rendering/qttilecallbackfactory.h
    mainwindow/rendering/renderingmanager.cpp
    mainwindow/rendering/renderingmanager.h
    mainwindow/rendering/renderingtimer.h
    mainwindow/rendering/renderwidget.cpp
    mainwindow/rendering/renderwidget.h
)
list(APPEND appleseed.studio_sources
    ${mainwindow_rendering_sources}
)
source_group("mainwindow\\rendering" FILES
    ${mainwindow_rendering_sources}
)

set(mainwindow_sources
    mainwindow/logwidget.cpp
    mainwindow/logwidget.h
    mainwindow/mainwindow.cpp
    mainwindow/mainwindow.h
    mainwindow/mainwindow.ui
    mainwindow/qtlogtarget.cpp
    mainwindow/qtlogtarget.h
    mainwindow/statusbar.cpp
    mainwindow/statusbar.h
)
list(APPEND appleseed.studio_sources
    ${mainwindow_sources}
)
source_group("mainwindow" FILES
    ${mainwindow_sources}
)

set(meta_templates_sources
    meta/templates/studio_component_template.cpp
    meta/templates/studio_component_template.h
    meta/templates/studio_test_template.cpp
)
list(APPEND appleseed.studio_sources
    ${meta_templates_sources}
)
source_group("meta\\templates" FILES
    ${meta_templates_sources}
)
set_source_files_properties(
    ${meta_templates_sources}
    PROPERTIES HEADER_FILE_ONLY TRUE
)

set(meta_tests_sources
    meta/tests/test_projectmanager.cpp
    meta/tests/test_tools.cpp
)
list(APPEND appleseed.studio_sources
    ${meta_tests_sources}
)
source_group("meta\\tests" FILES
    ${meta_tests_sources}
)

set(utility_sources
    utility/chartwidget.cpp
    utility/chartwidget.h
    utility/interop.h
    utility/scrollareapanhandler.cpp
    utility/scrollareapanhandler.h
    utility/tweaks.cpp
    utility/tweaks.h
    utility/widgetzoomhandler.cpp
    utility/widgetzoomhandler.h
)
list(APPEND appleseed.studio_sources
    ${utility_sources}
)
source_group("utility" FILES
    ${utility_sources}
)

set(resources
    resources/resources.qrc
)
list(APPEND appleseed.studio_sources
    ${resources}
)
source_group("resources" FILES
    ${resources}
)


#
# Generate ui_* files.
#

# Collect all .ui files amongst the source files.
filter_list(
    appleseed.studio_ui_files
    "${appleseed.studio_sources}"
    ".*\\\\.ui"
)

QT4_WRAP_UI(appleseed.studio_generated_ui_files
    ${appleseed.studio_ui_files}
)

include_directories(${CMAKE_CURRENT_BINARY_DIR})


#
# Generate moc_* files.
#

# Moc .h files.
filter_list(
    appleseed.studio_h_files
    "${appleseed.studio_sources}"
    ".*\\\\.h"
)
QT4_WRAP_CPP(appleseed.studio_generated_moc_h_files
    ${appleseed.studio_h_files}
)

# Moc .cpp files.
filter_list(
    appleseed.studio_cpp_files
    "${appleseed.studio_sources}"
    ".*\\\\.cpp"
)
QT4_WRAP_CPP_CPLUSPLUS_FILES(appleseed.studio_generated_moc_cpp_files
    ${appleseed.studio_cpp_files}
)


#
# Compile resource files.
#

QT4_ADD_RESOURCES(appleseed.studio_resource_files
    ${resources}
)


#
# Define the target and its dependencies.
#

add_executable(appleseed.studio
    ${appleseed.studio_sources}
    ${appleseed.studio_generated_ui_files}
    ${appleseed.studio_generated_moc_h_files}
    ${appleseed.studio_resource_files}
)

if(WIN32)
    set_target_properties(appleseed.studio PROPERTIES
        WIN32_EXECUTABLE TRUE
    )
endif()


#
# Include Paths.
#

include_directories(
    .
    ../appleseed.shared
)


#
# Preprocessor Definitions.
#

apply_preprocessor_definitions(appleseed.studio)


#
# Static Libraries.
#

link_against_common_static_libraries(appleseed.studio)

target_link_libraries(appleseed.studio
    appleseed
    appleseed.shared
    ${QT_LIBRARIES}
    ${OPENGL_LIBRARY}
)

if(WIN32)
    target_link_libraries(appleseed.studio
        ${QT_QTMAIN_LIBRARY}
    )
endif()


#
# Post-Build Commands.
#

add_copy_target_to_sandbox_command(appleseed.studio)
