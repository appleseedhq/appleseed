
//
// This software is released under the MIT licence
//
// Copyright (c) 2013 Anders Langlands
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of
// this software and associated documentation files (the "Software"), to deal in
// the Software without restriction, including without limitation the rights to
// use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
// the Software, and to permit persons to whom the Software is furnished to do so,
// subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
// FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
// COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
// IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//

// This code is ported from alshaders original C++ implementation.
// https://bitbucket.org/anderslanglands/alshaders

#ifndef ALSHADERS_OSL_SPACE_H
#define ALSHADERS_OSL_SPACE_H

// Constants.
#define SPACE_WORLD     0
#define SPACE_OBJECT    1
#define SPACE_PREF      2
#define SPACE_UV        3

// Params.
#define SPACE_DECLARE_PARAMS                            \
int in_space = 0                                        \
[[                                                      \
    string label = "Space",                             \
    string widget = "mapper",                           \
    string options = "world:0|object:1|PRef:2|UV:3",    \
    string as_maya_attribute_name = "space"             \
]],                                                     \
point in_p = P                                          \
[[                                                      \
    string label = "P",                                 \
    string as_maya_attribute_name = "P"                 \
]],                                                     \
point Pref = P                                          \
[[                                                      \
    int lockgeom = 0,                                   \
    string widget = "null"                              \
]]

#endif
