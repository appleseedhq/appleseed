
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
# Python.
#--------------------------------------------------------------------------------------------------

find_package (PythonLibs REQUIRED)

# Splitting version string into list.
string (REGEX MATCHALL "[0123456789]+" PYTHON_VERSION_LIST "${PYTHONLIBS_VERSION_STRING}")

if (PYTHON_VERSION_LIST)
    # Accessing major and minor version numbers with the version list we got.
    list (GET PYTHON_VERSION_LIST 0 PYTHON_MAJOR_VERSION)
    list (GET PYTHON_VERSION_LIST 1 PYTHON_MINOR_VERSION)
else ()
    # Default to latest Python 2 release if version string cannot be read.
    message (STATUS "Cannot find Python version. Using 2.7 by default")
    set (PYTHON_MAJOR_VERSION "2")
    set (PYTHON_MINOR_VERSION "7")
endif ()


#--------------------------------------------------------------------------------------------------
# Boost libraries.
#--------------------------------------------------------------------------------------------------

if (PYTHON_MAJOR_VERSION STREQUAL "3")
    find_package (Boost 1.55 COMPONENTS python3)
else ()
    find_package (Boost 1.55 COMPONENTS python)
endif ()


#--------------------------------------------------------------------------------------------------
# Source files.
#--------------------------------------------------------------------------------------------------

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
    logging.py
    metadata.h
    module.cpp
    pyseed.h
    unalignedmatrix44.h
    unalignedtransform.h
)
list (APPEND appleseed.python_sources
    ${sources}
)
file (GLOB py_test_sources test/*.py)
list (APPEND appleseed.python_sources
    ${py_test_sources}
)
source_group ("" FILES
    ${sources}
)


#--------------------------------------------------------------------------------------------------
# Target.
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


#--------------------------------------------------------------------------------------------------
# Include paths.
#--------------------------------------------------------------------------------------------------

include_directories (
    .
    ../appleseed.shared
    ${PYTHON_INCLUDE_DIRS}
)


#--------------------------------------------------------------------------------------------------
# Preprocessor definitions.
#--------------------------------------------------------------------------------------------------

apply_preprocessor_definitions (appleseed.python)

append_custom_preprocessor_definitions (appleseed.python
    APPLESEED_ENABLE_IMATH_INTEROP
)


#--------------------------------------------------------------------------------------------------
# Static libraries.
#--------------------------------------------------------------------------------------------------

link_against_platform (appleseed.python)

target_link_libraries (appleseed.python
    appleseed
    ${Boost_LIBRARIES}
    ${PYTHON_LIBRARIES}
)


#--------------------------------------------------------------------------------------------------
# Post-build commands.
#--------------------------------------------------------------------------------------------------

add_copy_target_to_sandbox_py_module_command (appleseed.python)
add_copy_py_file_to_sandbox_py_module_command (${PROJECT_SOURCE_DIR}/src/appleseed.python/__init__.py)
add_copy_py_file_to_sandbox_py_module_command (${PROJECT_SOURCE_DIR}/src/appleseed.python/logging.py)
add_copy_dir_to_sandbox_py_module_command (${PROJECT_SOURCE_DIR}/src/appleseed.python/test)


#--------------------------------------------------------------------------------------------------
# Installation.
#--------------------------------------------------------------------------------------------------

set (py_module_dst "lib/python${PYTHON_MAJOR_VERSION}.${PYTHON_MINOR_VERSION}/appleseed")
install (FILES __init__.py DESTINATION ${py_module_dst})
install (FILES logging.py DESTINATION ${py_module_dst})
install (DIRECTORY test DESTINATION ${py_module_dst})
install (TARGETS appleseed.python DESTINATION ${py_module_dst})
