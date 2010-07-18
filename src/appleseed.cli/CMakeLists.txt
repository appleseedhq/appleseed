
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

set(meta_templates_sources
    meta/templates/cli_component_template.cpp
    meta/templates/cli_component_template.h
    meta/templates/cli_test_template.cpp
)
list(APPEND appleseed.cli_sources
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
    meta/tests/test_superlogger.cpp
)
list(APPEND appleseed.cli_sources
    ${meta_tests_sources}
)
source_group("meta\\tests" FILES
    ${meta_tests_sources}
)

set(sources
    commandline.cpp
    commandline.h
    main.cpp
    superlogger.cpp
    superlogger.h
)
list(APPEND appleseed.cli_sources
    ${sources}
)
source_group("" FILES
    ${sources}
)


#
# Target.
#

add_executable(appleseed.cli ${appleseed.cli_sources})

target_link_libraries(appleseed.cli appleseed appleseed.shared)


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

set_property(TARGET appleseed.cli APPEND PROPERTY COMPILE_DEFINITIONS_DEBUG
    ${preprocessor_definitions_debug}
)

set_property(TARGET appleseed.cli APPEND PROPERTY COMPILE_DEFINITIONS_RELEASE
    ${preprocessor_definitions_release}
)

set_property(TARGET appleseed.cli APPEND PROPERTY COMPILE_DEFINITIONS_PROFILE
    ${preprocessor_definitions_release}
)

set_property(TARGET appleseed.cli APPEND PROPERTY COMPILE_DEFINITIONS_SHIP
    ${preprocessor_definitions_release}
    SHIP
)


#
# Static Libraries.
#

link_against_static_libraries(appleseed.cli)


#
# Post-Build Command.
#

add_copy_target_to_sandbox_command(appleseed.cli)
