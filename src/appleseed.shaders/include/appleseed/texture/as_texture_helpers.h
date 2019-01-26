//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Luis Barrancos, The appleseedhq Organization
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

#include "appleseed/transform/as_transform_helpers.h"

string get_wrap_mode(int wrap_mode)
{   
    if (wrap_mode == 0)
    {
        return "default";
    }
    else if (wrap_mode == 1)
    {
        return "black";
    }
    else if (wrap_mode == 2)
    {
        return "periodic";
    }
    else if (wrap_mode == 3)
    {
        return "clamp";
    }
    else
    {
        return "mirror";
    }
}

void set_wrap_mode(
    int wrap_mode[2],
    output string mode[2])
{
    mode[0] = get_wrap_mode(wrap_mode[0]);
    mode[1] = get_wrap_mode(wrap_mode[1]);
}

void set_wrap_mode(
    int wrap_smode,
    int wrap_tmode,
    output string mode[2])
{
    mode[0] = get_wrap_mode(wrap_smode);
    mode[1] = get_wrap_mode(wrap_tmode);
}

void set_wrap_mode(
    int wrap_mode[3],
    output string mode[3])
{
    mode[0] = get_wrap_mode(wrap_mode[0]);
    mode[1] = get_wrap_mode(wrap_mode[1]);
    mode[2] = get_wrap_mode(wrap_mode[2]);
}

void set_wrap_mode(
    int wrap_smode,
    int wrap_tmode,
    int wrap_rmode,
    output string mode[3])
{
    mode[0] = get_wrap_mode(wrap_smode);
    mode[1] = get_wrap_mode(wrap_tmode);
    mode[2] = get_wrap_mode(wrap_rmode);
}

string get_interpolation_method(int method)
{
    if (method == 0)
    {
        return "smartcubic";
    }
    else if (method == 1)
    {
        return "cubic";
    }
    else if (method == 2)
    {
        return "linear";
    }
    else
    {
        return "closest";
    }
}

color get_projection_color(
    string filename,
    float s_coord,
    float t_coord,
    float width,
    float height,
    float x_offset,
    float y_offset,
    float angle,
    string swrap,
    string twrap,
    int sflip,
    int tflip,
    int backside,
    output float alpha)
{
    float st[2] = {s_coord, t_coord};

    if (angle != 0.0)
    {
        rotate2d(st[0], st[1], angle, st[0], st[1]);
    }

    st[0] = sflip ? 1.0 - mod(st[0], UVWRAP) : mod(st[0], UVWRAP);
    st[1] = tflip ? 1.0 - mod(st[1], UVWRAP) : mod(st[1], UVWRAP);

    st[0] *= width;
    st[1] *= height;
    
    st[0] += x_offset;
    st[1] += y_offset;

    st[0] *= backside;

    return (color) texture(
        filename,
        st[0],
        st[1],
        "swrap", swrap,
        "twrap", twrap,
        "missingcolor", color(0),
        "missingalpha", 1.0,
        "alpha", alpha);
}
