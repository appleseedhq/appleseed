
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2017 Luis Barrancos, The appleseedhq Organization
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
 
#ifndef AS_MAYA_RAMP_HELPERS_H
#define AS_MAYA_RAMP_HELPERS_H

#include "appleseed/math/as_math_helpers.h"
#include "appleseed/maya/as_maya_helpers.h"

// Ref: $MAYA_LOCATION/docs/Nodes/ramp.html

color interpolate_color(
    float value,
    color C1,
    color C2,
    float position1,
    float position2,
    int interpolate)
{
    if (interpolate == 0)
    {
        return C1;
    }
    else
    {
        float delta = (value - position1) / (position2 - position1), weight;

        if (interpolate == 1)
        {
            weight = delta;
        }
        else if (interpolate == 2)
        {
            weight = sqr(delta);
        }
        else if (interpolate == 3)
        {
            weight = 1 - sqr(1 - delta);
        }
        else if (interpolate == 4)
        {
            weight = 0.5 * (cos((delta + 1) * M_PI) + 1);
        }
        else if (interpolate == 5)
        {
            weight = (luminance(C1) < luminance(C2))
                ? sin(delta * M_PI_2)
                : sin((delta - 1) * M_PI_2) + 1;
        }
        else
        {
            weight = (luminance(C1) > luminance(C2))
                ? sin(delta * M_PI_2)
                : sin((delta - 1) * M_PI_2) + 1;
        }
        return mix(C1, C2, weight);
    }
}

float interpolate_value(
    float value,
    float v1,
    float v2,
    float position1,
    float position2,
    int interpolate)
{
    if (interpolate == 0)
    {
        return v1;
    }
    else
    {
        float delta = (value - position1) / (position2 - position1), weight;

        if (interpolate == 1)
        {
            weight = delta;
        }
        else if (interpolate == 2)
        {
            weight = sqr(delta);
        }
        else if (interpolate == 3)
        {
            weight = 1 - sqr(1 - delta);
        }
        else if (interpolate == 4)
        {
            weight = 0.5 * (cos((delta + 1) * M_PI) + 1);
        }
        else if (interpolate == 5)
        {
            weight = (v1 < v2)
                ? sin(delta * M_PI_2)
                : sin((delta - 1) * M_PI_2) + 1;
        }
        else
        {
            weight = (v1 > v2)
                ? sin(delta * M_PI_2)
                : sin((delta - 1) * M_PI_2) + 1;
        }
        return mix(v1, v2, weight);
    }
}

color color_ramp(
    color colors[],
    float positions[],
    int interpolate[],
    float value)
{
    int len = arraylength(positions);

    if (len && len == arraylength(colors))
    {
        int index = -1;
        float lower_bound = -1;
        float upper_bound = 10;
        float position_min = 10;
        color C1 = color(0), C2 = color(0);

        for (int i = 0; i < len; ++i)
        {
            if (positions[i] < 0)
            {
                continue;
            }

            float position = clamp(positions[i], 0, 1);

            if (position < position_min)
            {
                position_min = position;
                index = i;
            }

            if (position <= value && position > lower_bound)
            {
                lower_bound = position;
                C1 = colors[i];
            }

            if (position >= value && position < upper_bound)
            {
                upper_bound = position;
                C2 = colors[i];
            }
        }

        if (lower_bound < position_min)
        {
            lower_bound = position_min;
            C1 = (index >= 0) ? colors[index] : color(0);
        }

        int interpolation =
            (arraylength(interpolate) && index >= 0)
                ? interpolate[index]
                : 0;

        return interpolate_color(
            value,
            C1,
            C2,
            lower_bound,
            upper_bound,
            interpolation);
    }
    else
    {
        return colors[0];
    }
}

float float_ramp(
    float float_value[],
    float positions[],
    int interpolate[],
    float value)
{
    int len = arraylength(positions);

    if (len && len == arraylength(float_value))
    {
        int index = -1;
        float lower_bound = -1;
        float upper_bound = 10.0;
        float position_min = 10.0;
        float v1 = 0.0, v2 = 0.0;

        for (int i = 0; i < len; ++i)
        {
            if (positions[i] < 0.0)
            {
                continue;
            }

            float position = clamp(positions[i], 0.0, 1.0);

            if (position < position_min)
            {
                position_min = position;
                index = i;
            }

            if (position <= value && position > lower_bound)
            {
                lower_bound = position;
                v1 = float_value[i];
            }

            if (position >= value && position < upper_bound)
            {
                upper_bound = position;
                v2 = float_value[i];
            }
        }

        if (lower_bound < position_min)
        {
            lower_bound = position_min;
            v1 = (index >= 0) ? float_value[index] : 0.0;
        }

        int interpolation =
            (arraylength(interpolate) && index >= 0)
                ? interpolate[index]
                : 0;

        return interpolate_value(
            value,
            v1,
            v2,
            lower_bound,
            upper_bound,
            interpolation);
    }
    else
    {
        return float_value[0];
    }
}

color color_ramp(                  
    color colors[],
    float positions[],
    int interpolation,
    float value)
{
    int interpolate[1] = {interpolation};

    return color_ramp(colors, positions, interpolate, value);
}

#endif // AS_MAYA_RAMP_HELPERS_H
