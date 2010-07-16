
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
# Source Files.
#

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

set(mainwindow_sources
    mainwindow/logwidget.cpp
    mainwindow/logwidget.h
    mainwindow/mainwindow.cpp
    mainwindow/mainwindow.h
    mainwindow/projectexplorer.cpp
    mainwindow/projectexplorer.h
    mainwindow/projectmanager.cpp
    mainwindow/projectmanager.h
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
)
list(APPEND appleseed.studio_sources
    ${meta_tests_sources}
)
source_group("meta\\tests" FILES
    ${meta_tests_sources}
)

set(rendering_sources
    rendering/cameracontroller.cpp
    rendering/cameracontroller.h
    rendering/glrenderwidget.cpp
    rendering/glrenderwidget.h
    rendering/irenderwidget.h
    rendering/qtrenderercontroller.cpp
    rendering/qtrenderercontroller.h
    rendering/qttilecallbackfactory.cpp
    rendering/qttilecallbackfactory.h
    rendering/renderingmanager.cpp
    rendering/renderingmanager.h
    rendering/renderingtimer.h
    rendering/renderwidget.cpp
    rendering/renderwidget.h
)
list(APPEND appleseed.studio_sources
    ${rendering_sources}
)
source_group("rendering" FILES
    ${rendering_sources}
)

set(utility_sources
    utility/qtiostreamop.h
    utility/scrollareapanhandler.cpp
    utility/scrollareapanhandler.h
    utility/widgetzoomhandler.cpp
    utility/widgetzoomhandler.h
)
list(APPEND appleseed.studio_sources
    ${utility_sources}
)
source_group("utility" FILES
    ${utility_sources}
)


#
# Target.
#

add_executable(appleseed.studio ${appleseed.studio_sources})

target_link_libraries(appleseed.studio appleseed appleseed.shared)


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

set_property(TARGET appleseed.studio APPEND PROPERTY COMPILE_DEFINITIONS_DEBUG
    ${preprocessor_definitions_debug}
)

set_property(TARGET appleseed.studio APPEND PROPERTY COMPILE_DEFINITIONS_RELEASE
    ${preprocessor_definitions_release}
)

set_property(TARGET appleseed.studio APPEND PROPERTY COMPILE_DEFINITIONS_PROFILE
    ${preprocessor_definitions_release}
)

set_property(TARGET appleseed.studio APPEND PROPERTY COMPILE_DEFINITIONS_SHIP
    ${preprocessor_definitions_release}
    SHIP
)


#
# Static Libraries.
#

link_against_static_libraries(appleseed.studio)
