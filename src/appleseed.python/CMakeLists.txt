
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
    message (FATAL_ERROR "Python 2.x needed")
endif ()

find_package (Boost 1.55 COMPONENTS python)

if (WITH_PYTHON3_BINDINGS)
    find_package (Python3 REQUIRED)
endif ()


#--------------------------------------------------------------------------------------------------
# Source files.
#--------------------------------------------------------------------------------------------------

file (GLOB test_sources test/*.py)
list (APPEND appleseed.python_sources
    ${test_sources}
)
source_group ("test" FILES
    ${test_sources}
)

set (sources
    __init__.py
    bindaov.cpp
    bindassembly.cpp
    bindbasis.cpp
    bindbbox.cpp
    bindbsdf.cpp
    bindbssrdf.cpp
    bindcamera.cpp
    bindcolor.cpp
    binddisplay.cpp
    bindedf.cpp
    bindentity.cpp
    bindentitycontainers.h
    bindenvironment.cpp
    bindframe.cpp
    bindfresnel.cpp
    bindimage.cpp
    bindlight.cpp
    bindlogger.cpp
    bindmasterrenderer.cpp
    bindmaterial.cpp
    bindmatrix.cpp
    bindmeshobject.cpp
    bindobject.cpp
    bindproject.cpp
    bindquaternion.cpp
    bindrenderercontroller.cpp
    bindscene.cpp
    bindshadergroup.cpp
    bindsurfaceshader.cpp
    bindtexture.cpp
    bindtilecallback.cpp
    bindtransform.cpp
    bindutility.cpp
    bindvector.cpp
    dict2dict.cpp
    dict2dict.h
    gillocks.h
    gillocks.cpp
    logtarget.py
    metadata.h
    module.cpp
    unalignedmatrix44.h
    unalignedtransform.h
)
list (APPEND appleseed.python_sources
    ${sources}
)
source_group ("" FILES
    ${sources}
)


#--------------------------------------------------------------------------------------------------
# Targets.
#--------------------------------------------------------------------------------------------------

add_library (appleseed.python MODULE
    ${appleseed.python_sources}
)

set_target_properties (appleseed.python PROPERTIES
    PREFIX ""
    OUTPUT_NAME "_appleseedpython"
)

if (CMAKE_SYSTEM_NAME STREQUAL "Windows")
    set_target_properties (appleseed.python PROPERTIES
        SUFFIX ".pyd"
    )
endif ()

if (WITH_PYTHON3_BINDINGS)
    add_library (appleseed.python3 MODULE
        ${appleseed.python_sources}
    )

    set_target_properties (appleseed.python3 PROPERTIES
        PREFIX ""
        OUTPUT_NAME "_appleseedpython3"
    )

    if (CMAKE_SYSTEM_NAME STREQUAL "Windows")
        set_target_properties (appleseed.python3 PROPERTIES
            SUFFIX ".pyd"
        )
    endif ()
endif ()

#--------------------------------------------------------------------------------------------------
# Include paths.
#--------------------------------------------------------------------------------------------------

include_directories (
    .
    ../appleseed.shared
)

target_include_directories (appleseed.python PRIVATE ${PYTHON_INCLUDE_DIRS})

if (WITH_PYTHON3_BINDINGS)
    target_include_directories (appleseed.python3 PRIVATE ${PYTHON3_INCLUDE_DIRS})
endif ()

#--------------------------------------------------------------------------------------------------
# Preprocessor definitions.
#--------------------------------------------------------------------------------------------------

apply_preprocessor_definitions (appleseed.python)

append_custom_preprocessor_definitions (appleseed.python
    APPLESEED_ENABLE_IMATH_INTEROP
)

if (WITH_PYTHON3_BINDINGS)
    apply_preprocessor_definitions (appleseed.python3)

    append_custom_preprocessor_definitions (appleseed.python3
        APPLESEED_ENABLE_IMATH_INTEROP
    )
endif ()

#--------------------------------------------------------------------------------------------------
# Static libraries.
#--------------------------------------------------------------------------------------------------

link_against_platform (appleseed.python)

target_link_libraries (appleseed.python
    appleseed
    ${Boost_LIBRARIES}
    ${PYTHON_LIBRARIES}
)

if (WITH_PYTHON3_BINDINGS)
    link_against_platform (appleseed.python3)

    target_link_libraries (appleseed.python3
        appleseed
        ${PYTHON3_LIBRARIES}
    )
endif ()

#--------------------------------------------------------------------------------------------------
# Post-build commands.
#--------------------------------------------------------------------------------------------------

add_custom_target (appleseed.python.copy_py_files ALL)

add_copy_target_to_sandbox_py_module_command (appleseed.python appleseed)
add_copy_py_file_to_sandbox_py_module_command (${PROJECT_SOURCE_DIR}/src/appleseed.python/__init__.py appleseed)
add_copy_py_file_to_sandbox_py_module_command (${PROJECT_SOURCE_DIR}/src/appleseed.python/logtarget.py appleseed)
add_copy_py_file_to_sandbox_py_module_command (${PROJECT_SOURCE_DIR}/src/appleseed.python/textureconverter.py appleseed)
add_copy_dir_to_sandbox_py_module_command (${PROJECT_SOURCE_DIR}/src/appleseed.python/test appleseed)

if (WITH_PYTHON3_BINDINGS)
    add_copy_target_to_sandbox_py_module_command (appleseed.python3 appleseed)
endif ()

#--------------------------------------------------------------------------------------------------
# Installation.
#--------------------------------------------------------------------------------------------------

set (py_module_dst "lib/python${PYTHON_MAJOR_VERSION}.${PYTHON_MINOR_VERSION}/appleseed")
install (FILES __init__.py DESTINATION ${py_module_dst})
install (FILES logtarget.py DESTINATION ${py_module_dst})
install (DIRECTORY test DESTINATION ${py_module_dst})
install (TARGETS appleseed.python DESTINATION ${py_module_dst})

if (WITH_PYTHON3_BINDINGS)
    install (TARGETS appleseed.python3 DESTINATION ${py_module_dst})
endif ()
