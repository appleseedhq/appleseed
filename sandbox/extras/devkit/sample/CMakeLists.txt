
#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2013 Nicholas Yue, Jupiter Jazz Limited
# Copyright (c) 2014 Nicholas Yue, The appleseedhq Organization
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


CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

PROJECT ( appleseed_sample )

FIND_PACKAGE ( Appleseed )
FIND_PACKAGE ( ILMBase )
FIND_PACKAGE ( Boost )

INCLUDE_DIRECTORIES ( ${ILMBASE_INCLUDE_DIRS} )
INCLUDE_DIRECTORIES ( ${APPLESEED_INCLUDE_DIRS} )
INCLUDE_DIRECTORIES ( ${Boost_INCLUDE_DIR} )

MESSAGE ( "APPLESEED_INCLUDE_DIRS = ${APPLESEED_INCLUDE_DIRS}" )

ADD_EXECUTABLE ( sample
  half.cpp
  sample.cpp
  )

MESSAGE ( "Appleseed_appleseed_LIBRARY=${Appleseed_appleseed_LIBRARY}" )
MESSAGE ( "Appleseed_appleseed_shared_LIBRARY=${Appleseed_appleseed_shared_LIBRARY}" )

TARGET_LINK_LIBRARIES ( sample
  ${Appleseed_appleseed_LIBRARY}
  ${Appleseed_appleseed_shared_LIBRARY}
)
