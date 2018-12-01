
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2018 Luis Barrancos, The appleseedhq Organization
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

#pragma once

#define NOISE_CUBE_SIDE     32.0
#define NOISE_CUBE_SHIFT    5
#define NOISE_CUBE_MASK     31
#define NOISE_TABLE_SIZE    65536

#include "appleseed/maya/as_maya_helpers.h"

float filtered_noise(
    point surface_point,
    float current_time,
    float filter_width)
{
    return (1.0 - smoothstep(0.2, 0.75, filter_width)) *
        noise("uperlin", surface_point, current_time);
}

float filtered_snoise(
    point surface_point,
    float current_time,
    float filter_width)
{
    return (1.0 - smoothstep(0.2, 0.75, filter_width)) *
        noise("perlin", surface_point, current_time);
}

float filtered_noise(
    point surface_point,
    float current_time,
    float filter_width,
    string type)
{
    return (1.0 - smoothstep(0.2, 0.75, filter_width)) *
        noise(type, surface_point, current_time);
} 

float filtered_noise(
    float x,
    float y,
    float current_time,
    float filter_width,
    string type)
{
    return (1.0 - smoothstep(0.2, 0.75, filter_width)) *
        noise(type, point(x, y, current_time));
}  

color filtered_noise(
    point surface_point,
    float current_time,
    float filter_width,
    string type)
{
    return (1.0 - smoothstep(0.2, 0.75, filter_width)) *
        (color) noise(type, surface_point, current_time);
} 

color filtered_noise(
    float x,
    float y,
    float current_time,
    float filter_width,
    string type)
{
    return (1.0 - smoothstep(0.2, 0.75, filter_width)) *
        (color) noise(type, point(x, y, current_time));
}

string get_noise_type(int type)
{
    string noise_type = "";

    if (type == 0)
    {
        noise_type = "uperlin";
    }
    else if (type == 1)
    {
        noise_type = "usimplex";
    }
    else if (type == 2)
    {
        noise_type = "value";
    }
    else if (type == 3)
    {
        noise_type = "voronoise";
    }
    else if (type == 4)
    {
        noise_type = "gabor";
    }
    else
    {
#ifdef DEBUG
        string shader_name = "";
        int status = getattribute("shader:shadername", shader_name);

        warning("[DEBUG]: Unknown noise mode %i in %s, %s:%i\n",
                type, shader_name, __FILE__, __LINE__);
#endif
    }
    return noise_type;
}
