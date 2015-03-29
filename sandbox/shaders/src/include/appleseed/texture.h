
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015 The masked shader writer, The appleseedhq Organization
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

#ifndef APPLESEED_SHADERS_TEXTURE_H
#define APPLESEED_SHADERS_TEXTURE_H

#define APPLESEED_DECLARE_TEXTURE_FILENAME_PARAM    \
    string Filename = ""                            \
    [[                                              \
        string widget = "filename"                  \
    ]]

#define APPLESEED_DECLARE_TEXTURE_UV_PARAMS         \
    float U = u,                                    \
    float V = v

#define APPLESEED_DEFAULT_TEXTURE_WRAP "periodic"

#define APPLESEED_TEXTURE_WRAP_METADATA     \
    string widget = "popup",                \
    string options = "default|black|periodic|clamp|mirror"

#define APPLESEED_DECLARE_TEXTURE_WRAP_PARAMS              \
    string UWrap = APPLESEED_DEFAULT_TEXTURE_WRAP          \
    [[                                                     \
        APPLESEED_TEXTURE_WRAP_METADATA                    \
    ]],                                                    \
    string VWrap = APPLESEED_DEFAULT_TEXTURE_WRAP          \
    [[                                                     \
        APPLESEED_TEXTURE_WRAP_METADATA                    \
    ]]

#define APPLESEED_DEFAULT_TEXTURE_FILTER "smartcubic"

#define APPLESEED_TEXTURE_FILTER_METADATA     \
    string widget = "popup",                  \
    string options = "smartcubic|cubic|linear|closest"

#define APPLESEED_DECLARE_TEXTURE_FILTER_PARAM                  \
    string Filter = APPLESEED_DEFAULT_TEXTURE_FILTER            \
    [[                                                          \
        APPLESEED_TEXTURE_FILTER_METADATA                       \
    ]]

#define APPLESEED_DECLARE_TEXTURE_WIDTH_PARAMS                  \
    float UWidth = 1.0,                                         \
    float VWidth = 1.0

#define APPLESEED_DECLARE_TEXTURE_BLUR_PARAMS                  \
    float UBlur = 0.0,                                         \
    float VBlur = 0.0

#endif
