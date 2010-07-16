
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

set(application_sources
    application/application.cpp
    application/application.h
)
list(APPEND appleseed.shared_sources
    ${application_sources}
)
source_group("application" FILES
    ${application_sources}
)


#
# Target.
#

add_library(appleseed.shared SHARED ${appleseed.shared_sources})

target_link_libraries(appleseed.shared appleseed)


#
# Include Paths.
#

include_directories(
    .
)


#
# Preprocessor Definitions.
#

set(appleseed_shared_preprocessor_definitions_common
    APPLESEED_SHARED_EXPORTS
)

set_property(TARGET appleseed.shared APPEND PROPERTY COMPILE_DEFINITIONS_DEBUG
    ${preprocessor_definitions_debug}
    ${appleseed_shared_preprocessor_definitions_common}
)

set_property(TARGET appleseed.shared APPEND PROPERTY COMPILE_DEFINITIONS_RELEASE
    ${preprocessor_definitions_release}
    ${appleseed_shared_preprocessor_definitions_common}
)

set_property(TARGET appleseed.shared APPEND PROPERTY COMPILE_DEFINITIONS_PROFILE
    ${preprocessor_definitions_release}
    ${appleseed_shared_preprocessor_definitions_common}
)

set_property(TARGET appleseed.shared APPEND PROPERTY COMPILE_DEFINITIONS_SHIP
    ${preprocessor_definitions_release}
    ${appleseed_shared_preprocessor_definitions_common}
    SHIP
)


#
# Static Libraries.
#

link_against_static_libraries(appleseed.shared)
