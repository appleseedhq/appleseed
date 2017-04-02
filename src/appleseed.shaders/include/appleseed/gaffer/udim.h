
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2017 The masked shader writer, The appleseedhq Organization
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//

#ifndef APPLESEED_SHADERS_UDIM_H
#define APPLESEED_SHADERS_UDIM_H

#define MARI_UDIM_NAME(i) format("%s%d%s", Filename, i + 1001, UDIMPrefix)

#define TEN_MARI_UDIM_NAMES(j)  \
    MARI_UDIM_NAME(10 * j + 0), \
    MARI_UDIM_NAME(10 * j + 1), \
    MARI_UDIM_NAME(10 * j + 2), \
    MARI_UDIM_NAME(10 * j + 3), \
    MARI_UDIM_NAME(10 * j + 4), \
    MARI_UDIM_NAME(10 * j + 5), \
    MARI_UDIM_NAME(10 * j + 6), \
    MARI_UDIM_NAME(10 * j + 7), \
    MARI_UDIM_NAME(10 * j + 8), \
    MARI_UDIM_NAME(10 * j + 9)

#define TEN_MARI_UDIM_ROWS  \
    TEN_MARI_UDIM_NAMES(0), \
    TEN_MARI_UDIM_NAMES(1), \
    TEN_MARI_UDIM_NAMES(2), \
    TEN_MARI_UDIM_NAMES(3), \
    TEN_MARI_UDIM_NAMES(4), \
    TEN_MARI_UDIM_NAMES(5), \
    TEN_MARI_UDIM_NAMES(6), \
    TEN_MARI_UDIM_NAMES(7), \
    TEN_MARI_UDIM_NAMES(8), \
    TEN_MARI_UDIM_NAMES(9) 

#endif // !APPLESEED_SHADERS_UDIM_H
