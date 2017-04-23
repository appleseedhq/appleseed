
#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
# Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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

# Qt 4.
find_package (Qt4 REQUIRED)
set (QT_USE_QTOPENGL TRUE)
include (${QT_USE_FILE})


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
    mainwindow/project/projectmanager.cpp
    mainwindow/project/projectmanager.h
    mainwindow/project/sceneitem.cpp
    mainwindow/project/sceneitem.h
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
if (WITH_DISNEY_MATERIAL)
    list (APPEND mainwindow_project_sources
        mainwindow/project/expressioneditorwindow.cpp
        mainwindow/project/expressioneditorwindow.h
        mainwindow/project/expressioneditorwindow.ui
        mainwindow/project/disneymaterialcustomui.cpp
        mainwindow/project/disneymaterialcustomui.h
        mainwindow/project/disneymateriallayerui.cpp
        mainwindow/project/disneymateriallayerui.h
    )
endif ()
list (APPEND appleseed.studio_sources
    ${mainwindow_project_sources}
)
source_group ("mainwindow\\project" FILES
    ${mainwindow_project_sources}
)

set (mainwindow_rendering_sources
    mainwindow/rendering/cameracontroller.cpp
    mainwindow/rendering/cameracontroller.h
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
    mainwindow/rendering/renderingtimer.h
    mainwindow/rendering/renderregionhandler.cpp
    mainwindow/rendering/renderregionhandler.h
    mainwindow/rendering/rendertab.cpp
    mainwindow/rendering/rendertab.h
    mainwindow/rendering/renderwidget.cpp
    mainwindow/rendering/renderwidget.h
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
    mainwindow/configurationmanagerwindow.cpp
    mainwindow/configurationmanagerwindow.h
    mainwindow/configurationmanagerwindow.ui
    mainwindow/logwidget.cpp
    mainwindow/logwidget.h
    mainwindow/mainwindow.cpp
    mainwindow/mainwindow.h
    mainwindow/mainwindow.ui
    mainwindow/minimizebutton.cpp
    mainwindow/minimizebutton.h
    mainwindow/qtlogtarget.cpp
    mainwindow/qtlogtarget.h
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

set (meta_tests_sources
    meta/tests/test_interop.cpp
    meta/tests/test_projectmanager.cpp
)
list (APPEND appleseed.studio_sources
    ${meta_tests_sources}
)
source_group ("meta\\tests" FILES
    ${meta_tests_sources}
)

set (utility_sources
    utility/chartwidget.cpp
    utility/chartwidget.h
    utility/doubleslider.cpp
    utility/doubleslider.h
    utility/foldablepanelwidget.cpp
    utility/foldablepanelwidget.h
    utility/inputwidgetproxies.cpp
    utility/inputwidgetproxies.h
    utility/interop.h
    utility/miscellaneous.cpp
    utility/miscellaneous.h
    utility/mousecoordinatestracker.cpp
    utility/mousecoordinatestracker.h
    utility/mousewheelfocuseventfilter.cpp
    utility/mousewheelfocuseventfilter.h
    utility/scrollareapanhandler.cpp
    utility/scrollareapanhandler.h
    utility/settingskeys.h
    utility/treewidget.cpp
    utility/treewidget.h
    utility/widgetzoomhandler.cpp
    utility/widgetzoomhandler.h
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

QT4_WRAP_UI (appleseed.studio_generated_ui_files
    ${appleseed.studio_ui_files}
)

include_directories (${CMAKE_CURRENT_BINARY_DIR})


#--------------------------------------------------------------------------------------------------
# Generate moc_* files.
#--------------------------------------------------------------------------------------------------

set (moc_options
    #
    # Work around moc's parsing failures.
    # See https://bugreports.qt-project.org/browse/QTBUG-22829
    #
    # BOOST_TT_HAS_OPERATOR_HPP_INCLUDED is already defined in the moc tool itself.
    # See http://code.qt.io/cgit/qt/qt.git/tree/src/tools/moc/main.cpp#n191
    #
    -DBOOST_NO_TEMPLATE_PARTIAL_SPECIALIZATION
    -DBOOST_TT_HAS_BIT_AND_HPP_INCLUDED
    -DBOOST_TT_HAS_BIT_AND_ASSIGN_HPP_INCLUDED
    -DBOOST_TT_HAS_BIT_OR_HPP_INCLUDED
    -DBOOST_TT_HAS_BIT_OR_ASSIGN_HPP_INCLUDED
    -DBOOST_TT_HAS_BIT_XOR_HPP_INCLUDED
    -DBOOST_TT_HAS_BIT_XOR_ASSIGN_HPP_INCLUDED
    -DBOOST_TT_HAS_COMPLEMENT_HPP_INCLUDED
    -DBOOST_TT_HAS_DEREFERENCE_HPP_INCLUDED
    -DBOOST_TT_HAS_DIVIDES_HPP_INCLUDED
    -DBOOST_TT_HAS_DIVIDES_ASSIGN_HPP_INCLUDED
    -DBOOST_TT_HAS_EQUAL_TO_HPP_INCLUDED
    -DBOOST_TT_HAS_GREATER_HPP_INCLUDED
    -DBOOST_TT_HAS_GREATER_EQUAL_HPP_INCLUDED
    -DBOOST_TT_HAS_LEFT_SHIFT_HPP_INCLUDED
    -DBOOST_TT_HAS_LEFT_SHIFT_ASSIGN_HPP_INCLUDED
    -DBOOST_TT_HAS_LESS_HPP_INCLUDED
    -DBOOST_TT_HAS_LESS_EQUAL_HPP_INCLUDED
    -DBOOST_TT_HAS_LOGICAL_AND_HPP_INCLUDED
    -DBOOST_TT_HAS_LOGICAL_NOT_HPP_INCLUDED
    -DBOOST_TT_HAS_LOGICAL_OR_HPP_INCLUDED
    -DBOOST_TT_HAS_MINUS_HPP_INCLUDED
    -DBOOST_TT_HAS_MINUS_ASSIGN_HPP_INCLUDED
    -DBOOST_TT_HAS_MODULUS_HPP_INCLUDED
    -DBOOST_TT_HAS_MODULUS_ASSIGN_HPP_INCLUDED
    -DBOOST_TT_HAS_MULTIPLIES_HPP_INCLUDED
    -DBOOST_TT_HAS_MULTIPLIES_ASSIGN_HPP_INCLUDED
    -DBOOST_TT_HAS_NEGATE_HPP_INCLUDED
    -DBOOST_TT_HAS_NEW_OPERATOR_HPP_INCLUDED
    -DBOOST_TT_HAS_NOT_EQUAL_TO_HPP_INCLUDED
    -DBOOST_TT_HAS_NOTHROW_ASSIGN_HPP_INCLUDED
    -DBOOST_TT_HAS_NOTHROW_CONSTRUCTOR_HPP_INCLUDED
    -DBOOST_TT_HAS_NOTHROW_COPY_HPP_INCLUDED
    -DBOOST_TT_HAS_NOTHROW_DESTRUCTOR_HPP_INCLUDED
    -DBOOST_TT_HAS_OPERATOR_HPP_INCLUDED
    -DBOOST_TT_HAS_PLUS_HPP_INCLUDED
    -DBOOST_TT_HAS_PLUS_ASSIGN_HPP_INCLUDED
    -DBOOST_TT_HAS_POST_DECREMENT_HPP_INCLUDED
    -DBOOST_TT_HAS_POST_INCREMENT_HPP_INCLUDED
    -DBOOST_TT_HAS_PRE_DECREMENT_HPP_INCLUDED
    -DBOOST_TT_HAS_PRE_INCREMENT_HPP_INCLUDED
    -DBOOST_TT_HAS_RIGHT_SHIFT_HPP_INCLUDED
    -DBOOST_TT_HAS_RIGHT_SHIFT_ASSIGN_HPP_INCLUDED
    -DBOOST_TT_HAS_TRIVIAL_ASSIGN_HPP_INCLUDED
    -DBOOST_TT_HAS_TRIVIAL_CONSTRUCTOR_HPP_INCLUDED
    -DBOOST_TT_HAS_TRIVIAL_COPY_HPP_INCLUDED
    -DBOOST_TT_HAS_TRIVIAL_DESTRUCTOR_HPP_INCLUDED
    -DBOOST_TT_HAS_TRIVIAL_MOVE_ASSIGN_HPP_INCLUDED
    -DBOOST_TT_HAS_TRIVIAL_MOVE_CONSTRUCTOR_HPP_INCLUDED
    -DBOOST_TT_HAS_UNARY_MINUS_HPP_INCLUDED
    -DBOOST_TT_HAS_UNARY_PLUS_HPP_INCLUDED
    -DBOOST_TT_HAS_VIRTUAL_DESTRUCTOR_HPP_INCLUDED
)

# Moc .h files.
filter_list (
    appleseed.studio_h_files
    "${appleseed.studio_sources}"
    ".*\\\\.h"
)
QT4_WRAP_CPP (appleseed.studio_generated_moc_h_files
    ${appleseed.studio_h_files}
    OPTIONS ${moc_options}
)

# Moc .cpp files.
filter_list (
    appleseed.studio_cpp_files
    "${appleseed.studio_sources}"
    ".*\\\\.cpp"
)
QT4_WRAP_CPP_CPLUSPLUS_FILES (appleseed.studio_generated_moc_cpp_files
    ${appleseed.studio_cpp_files}
    OPTIONS ${moc_options}
)


#--------------------------------------------------------------------------------------------------
# Compile Qt resource files.
#--------------------------------------------------------------------------------------------------

QT4_ADD_RESOURCES (appleseed.studio_resource_files
    ${resources}
)


#--------------------------------------------------------------------------------------------------
# Target.
#--------------------------------------------------------------------------------------------------

add_executable (appleseed.studio
    ${appleseed.studio_sources}
    ${appleseed.studio_generated_ui_files}
    ${appleseed.studio_generated_moc_h_files}
    ${appleseed.studio_resource_files}
)

if (WIN32)
    set_target_properties (appleseed.studio PROPERTIES
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
    ../appleseed.shared
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

target_link_libraries (appleseed.studio
    appleseed
    appleseed.shared
    ${Boost_LIBRARIES}
    ${QT_LIBRARIES}
    ${OPENGL_LIBRARY}
)

if (WITH_DISNEY_MATERIAL)
    link_against_seexpreditor (appleseed.studio)
endif ()

if (WIN32)
    target_link_libraries (appleseed.studio
        ${QT_QTMAIN_LIBRARY}
    )
endif ()


#--------------------------------------------------------------------------------------------------
# Post-build commands.
#--------------------------------------------------------------------------------------------------

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

install (DIRECTORY ../../sandbox/seexpr
    DESTINATION .
)
