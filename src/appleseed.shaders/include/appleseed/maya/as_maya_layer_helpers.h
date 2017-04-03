
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Luis Barrancos, The appleseedhq Organization
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

// Ref: $MAYA_LOCATION/docs/Nodes/layerTexture.html

#ifndef AS_MAYA_LAYER_HELPERS_H
#define AS_MAYA_LAYER_HELPERS_H

#include "appleseed/math/as_math_helpers.h"
#include "appleseed/maya/as_maya_helpers.h"

#define MAYA_BLENDMODE_NONE          0
#define MAYA_BLENDMODE_OVER          1
#define MAYA_BLENDMODE_IN            2   
#define MAYA_BLENDMODE_OUT           3
#define MAYA_BLENDMODE_ADD           4
#define MAYA_BLENDMODE_SUBTRACT      5
#define MAYA_BLENDMODE_MULTIPLY      6
#define MAYA_BLENDMODE_DIFFERENCE    7
#define MAYA_BLENDMODE_LIGHTEN       8
#define MAYA_BLENDMODE_DARKEN        9
#define MAYA_BLENDMODE_SATURATE      10
#define MAYA_BLENDMODE_DESATURATE    11
#define MAYA_BLENDMODE_ILLUMINATE    12

color maya_blend_texture_layers(
    int blend_mode,
    color foreground_color,
    float foreground_alpha,
    color background_color,
    float background_alpha,
    output float output_alpha)
{
    color output_color = color(0);

    if (blend_mode == MAYA_BLENDMODE_NONE)
    {
        output_color = foreground_color;
        output_alpha = foreground_alpha;
    }
    else if (blend_mode == MAYA_BLENDMODE_OVER)
    {
        output_color = background_color + (foreground_alpha *
            (foreground_color - background_color));

        output_alpha = background_alpha + foreground_alpha -
            (background_alpha * foreground_alpha);
    }
    else if (blend_mode == MAYA_BLENDMODE_IN)
    {
        output_color = background_color * foreground_alpha;
        output_alpha = background_alpha * foreground_alpha;
    }
    else if (blend_mode == MAYA_BLENDMODE_OUT)
    {
        output_color = background_color * (1.0 - foreground_alpha);
        output_alpha = background_alpha * (1.0 - foreground_alpha);
    }
    else if (blend_mode == MAYA_BLENDMODE_ADD)
    {
        output_color = background_color +
            (foreground_color * foreground_alpha);

        output_alpha = background_alpha;
    }
    else if (blend_mode == MAYA_BLENDMODE_SUBTRACT)
    {
        output_color = background_color -
            (foreground_color * foreground_alpha);

        output_alpha = background_alpha;
    }
    else if (blend_mode == MAYA_BLENDMODE_MULTIPLY)
    {
        output_color = background_color *
            (foreground_color * foreground_alpha + 1.0 - foreground_alpha);

        output_alpha = background_alpha;
    }
    else if (blend_mode == MAYA_BLENDMODE_DIFFERENCE)
    {
        output_color =
            abs(foreground_color - background_color) * foreground_alpha +
            background_color * (1.0 - foreground_alpha);

        output_alpha = background_alpha;
    }
    else if (blend_mode == MAYA_BLENDMODE_LIGHTEN)
    {
        output_color =
            max(foreground_color, background_color) * foreground_alpha +
            background_color * (1.0 - foreground_alpha);

        output_alpha = background_alpha;
    }
    else if (blend_mode == MAYA_BLENDMODE_DARKEN)
    {
        output_color =
            min(foreground_color, background_color) * foreground_alpha +
            background_color * (1.0 - foreground_alpha);

        output_alpha = background_alpha;
    }
    else if (blend_mode == MAYA_BLENDMODE_SATURATE)
    {
        output_color = background_color *
            (1.0 + (foreground_color * foreground_alpha));

        output_alpha = background_alpha;
    }
    else if (blend_mode == MAYA_BLENDMODE_DESATURATE)
    {
        output_color = background_color *
            (1.0 - (foreground_color * foreground_alpha));

        output_alpha = background_alpha;
    }
    else if (blend_mode == MAYA_BLENDMODE_ILLUMINATE)
    {
        output_color = background_color *
            (2.0 * foreground_color * foreground_alpha +
             1.0 - foreground_alpha);

        output_alpha = background_alpha;
    }
    else
    {
#ifdef DEBUG
        warning("[WARNING]: Invalid blend mode selected in %s:%i\n",
                __FILE__, __LINE__);
#endif
    }
    return output_color;
}

#endif // !AS_MAYA_LAYER_HELPERS_H
