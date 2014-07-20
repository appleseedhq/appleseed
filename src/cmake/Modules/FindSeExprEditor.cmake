# Copyright (c) 2014 Marius Avram, The appleseedhq Organization

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

# Find SeExpr headers and libraries.
#
#  This module defines
#  SEEXPREDITOR_INCLUDE_DIRS - where to find SEEXPREDITOR includes.
#  SEEXPREDITOR_LIBRARIES    - List of libraries when using SEEXPREDITOR.
#  SEEXPREDITOR_FOUND        - True if SEEXPREDITOR found.

# Look for the header file.
FIND_PATH( SEEXPREDITOR_INCLUDE_DIR NAMES SeExprEditor/SeExprEditor.h)

# Look for the library.
FIND_LIBRARY( SEEXPREDITOR_LIBRARY NAMES SeExprEditor)

# handle the QUIETLY and REQUIRED arguments and set SEEXPREDITOR_FOUND to TRUE if
# all listed variables are TRUE
INCLUDE( FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS( SEEXPREDITOR DEFAULT_MSG SEEXPREDITOR_LIBRARY SEEXPREDITOR_INCLUDE_DIR)

# Copy the results to the output variables.
IF( SEEXPREDITOR_FOUND)
    SET( SEEXPREDITOR_LIBRARIES ${SEEXPREDITOR_LIBRARY})
    SET( SEEXPREDITOR_INCLUDE_DIRS ${SEEXPR_INCLUDE_DIR})
ELSE()
    SET( SEEXPREDITOR_LIBRARIES)
    SET( SEEXPREDITOR_INCLUDE_DIRS)
ENDIF()

MARK_AS_ADVANCED( SEEXPREDITOR_INCLUDE_DIR SEEXPREDITOR_LIBRARY)
