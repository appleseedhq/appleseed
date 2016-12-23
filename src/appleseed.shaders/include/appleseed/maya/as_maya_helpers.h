
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016 Luis Barrancos, The appleseedhq Organization
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
 
#ifndef AS_MAYA_HELPERS_H
#define AS_MAYA_HELPERS_H

#include "appleseed/color/as_color_data.h"
#include "appleseed/color/as_color_helpers.h"
#include "appleseed/math/as_math_helpers.h"

#define OUTSIDE_UVFRAME 999999
#define UVWRAP          1.0001

#define MAYA_COLORBALANCE_PARAMETERS                        \
    color in_defaultColor = color(0.5)                      \
    [[                                                      \
        string maya_attribute_name = "defaultColor",        \
        string maya_attribute_type = "vector",              \
        string label = "Default Color",                     \
        string page = "Color Balance"                       \
    ]],                                                     \
    color in_colorGain = color(1.0)                         \
    [[                                                      \
        string maya_attribute_name = "colorGain",           \
        string maya_attribute_type = "vector",              \
        string label = "Color Gain",                        \
        string page = "Color Balance"                       \
    ]],                                                     \
    color in_colorOffset = color(0.0)                       \
    [[                                                      \
        string maya_attribute_name = "colorOffset",         \
        string maya_attribute_type = "vector",              \
        string label = "Color Offset",                      \
        string page = "Color Balance"                       \
    ]],                                                     \
    float in_alphaGain = 1.0                                \
    [[                                                      \
        string maya_attribute_name = "alphaGain",           \
        string maya_attribute_type = "float",               \
        string label = "Alpha Gain",                        \
        string page = "Color Balance"                       \
    ]],                                                     \
    float in_alphaOffset = 0.0                              \
    [[                                                      \
        string maya_attribute_name = "alphaOffset",         \
        string maya_attribute_type = "float",               \
        string label = "Alpha Offset",                      \
        string page = "Color Balance"                       \
    ]],                                                     \
    int in_alphaIsLuminance = 1                             \
    [[                                                      \
        string maya_attribute_name = "alphaIsLuminance",    \
        string maya_attribute_type = "int",                 \
        string label = "Alpha Is Luminance",                \
        string widget = "checkBox",                         \
        string page = "Color Balance"                       \
    ]]

#define MAYA_EFFECTS_PARAMETERS                             \
    float in_filter = 1.0                                   \
    [[                                                      \
        string maya_attribute_name = "filter",              \
        string maya_attribute_type = "float",               \
        string label = "Filter",                            \
        string page = "Effects"                             \
    ]],                                                     \
    float in_filterOffset = 0.0                             \
    [[                                                      \
        string maya_attribute_name = "filterOffset",        \
        string maya_attribute_type = "float",               \
        string label = "Filter Offset",                     \
        string page = "Effects"                             \
    ]],                                                     \
    int in_invert = 0                                       \
    [[                                                      \
        string maya_attribute_name = "invert",              \
        string maya_attribute_type = "bool",                \
        string label = "Invert",                            \
        string widget = "checkBox",                         \
        string page = "Effects"                             \
    ]]

#define MAYA_UV_PARAMETERS                                  \
    float in_uvCoord[2] = {u, v}                            \
    [[                                                      \
        string maya_attribute_name = "uvCoord",             \
        string maya_attribute_type = "float[]",             \
        string label = "UV Coordinates",                    \
        string page = "UV Coordinates"                      \
    ]],                                                     \
    float in_uvFilterSize[2] = {0.0, 0.0}                   \
    [[                                                      \
        string maya_attribute_name = "uvFilterSize",        \
        string maya_attribute_type = "float[]",             \
        string label = "UV Filter Size",                    \
        string page = "UV Coordinates"                      \
    ]]

float maya_luminance(color in_C)
{
    return as_luminance(in_C, "Rec.601");
}

void maya_colorBalance(
    color in_colorGain,
    color in_colorOffset,
    color in_defaultColor,
    float in_alphaGain,
    float in_alphaOffset,
    int in_invert,
    int in_alphaIsLuminance,
    output color out_outColor,
    output float out_outAlpha)
{
    if (in_invert)
    {
        out_outColor = color(1) - out_outColor;
        out_outAlpha = 1 - out_outAlpha;
    }

    if (in_alphaIsLuminance)
    {
        out_outAlpha = maya_luminance(out_outColor);
    }

    out_outColor = out_outColor * in_colorGain + in_colorOffset;
    out_outAlpha = out_outAlpha * in_alphaGain + in_alphaOffset;
}

float maya_contrast(
    float in_value,
    float in_contrast,
    float in_bias)
{
    return fast_gain(fast_bias(in_value, in_bias), in_contrast);
}

color maya_contrast(
    color in_value,
    color in_contrast,
    color in_bias)
{
    float R = maya_contrast(in_value[0], in_contrast[0], in_bias[0]);
    float G = maya_contrast(in_value[1], in_contrast[1], in_bias[1]);
    float B = maya_contrast(in_value[2], in_contrast[2], in_bias[2]);

    return color(R, G, B);
}

#endif // AS_MAYA_HELPERS_H
