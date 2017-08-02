
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

# Boost.
if (NOT PYTHON_MAJOR_VERSION STREQUAL "2")
    message (FATAL_ERROR "Python 2.x needed")
else ()
    find_package (Boost 1.55 COMPONENTS python)
endif ()


#--------------------------------------------------------------------------------------------------
# Source files.
#--------------------------------------------------------------------------------------------------

set (appleseed_module_sources
    appleseed/__init__.py
    appleseed/bindaov.cpp
    appleseed/bindassembly.cpp
    appleseed/bindbasis.cpp
    appleseed/bindbbox.cpp
    appleseed/bindbsdf.cpp
    appleseed/bindbssrdf.cpp
    appleseed/bindcamera.cpp
    appleseed/bindcolor.cpp
    appleseed/binddisplay.cpp
    appleseed/bindedf.cpp
    appleseed/bindentity.cpp
    appleseed/bindentitycontainers.h
    appleseed/bindenvironment.cpp
    appleseed/bindframe.cpp
    appleseed/bindimage.cpp
    appleseed/bindlight.cpp
    appleseed/bindlogger.cpp
    appleseed/bindmasterrenderer.cpp
    appleseed/bindmaterial.cpp
    appleseed/bindmatrix.cpp
    appleseed/bindmeshobject.cpp
    appleseed/bindobject.cpp
    appleseed/bindproject.cpp
    appleseed/bindquaternion.cpp
    appleseed/bindrenderercontroller.cpp
    appleseed/bindscene.cpp
    appleseed/bindshadergroup.cpp
    appleseed/bindsurfaceshader.cpp
    appleseed/bindtexture.cpp
    appleseed/bindtilecallback.cpp
    appleseed/bindtransform.cpp
    appleseed/bindutility.cpp
    appleseed/bindvector.cpp
    appleseed/dict2dict.cpp
    appleseed/dict2dict.h
    appleseed/gillocks.h
    appleseed/gillocks.cpp
    appleseed/logtarget.py
    appleseed/metadata.h
    appleseed/module.cpp
    appleseed/unalignedmatrix44.h
    appleseed/unalignedtransform.h
    appleseed/studio/__init__.py
)
list (APPEND appleseed.python_sources
    ${appleseed_module_sources}
)
file (GLOB py_test_sources test/*.py)
list (APPEND appleseed.python_sources
    ${py_test_sources}
)
source_group ("" FILES
    ${appleseed_module_sources}
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

add_copy_target_to_sandbox_py_module_command (appleseed.python appleseed)
add_copy_py_file_to_sandbox_py_module_command (${PROJECT_SOURCE_DIR}/src/appleseed.python/appleseed/__init__.py appleseed)
add_copy_py_file_to_sandbox_py_module_command (${PROJECT_SOURCE_DIR}/src/appleseed.python/appleseed/logtarget.py appleseed)
add_copy_py_file_to_sandbox_py_module_command (${PROJECT_SOURCE_DIR}/src/appleseed.python/appleseed/studio/__init__.py appleseed.studio)
add_copy_dir_to_sandbox_py_module_command (${PROJECT_SOURCE_DIR}/src/appleseed.python/appleseed/test appleseed)


#--------------------------------------------------------------------------------------------------
# Installation.
#--------------------------------------------------------------------------------------------------

set (py_module_dst "lib/python${PYTHON_MAJOR_VERSION}.${PYTHON_MINOR_VERSION}/appleseed")
install (FILES __init__.py DESTINATION ${py_module_dst})
install (FILES logtarget.py DESTINATION ${py_module_dst})
install (FILES studio.py DESTINATION ${py_module_dst})
install (DIRECTORY test DESTINATION ${py_module_dst})
install (TARGETS appleseed.python DESTINATION ${py_module_dst})
