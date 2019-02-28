
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

int outside_place3d_volume(
    point surface_point,
    int wrap,
    float blend,
    output float blend_factor)
{
    int outside = 0;
    blend_factor = 0.0;

    if (!wrap || blend > 0.0)
    {
        if (!wrap)
        {
            // Default placement box bottom = (-1,-1,-1), top = (1,1,1)
            if (surface_point[0] < -1 || surface_point[0] > 1 ||
                surface_point[1] < -1 || surface_point[1] > 1 ||
                surface_point[2] < -1 || surface_point[2] > 1)
            {
                outside = 1;
            }
            else if (blend > 0.0)
            {
                vector xyz = (1.0 - abs((vector) surface_point)) / blend;
                xyz = min(vector(1), xyz);

                xyz[0] = cos((xyz[0] + 1.0) * M_PI);
                xyz[1] = cos((xyz[1] + 1.0) * M_PI);
                xyz[2] = cos((xyz[2] + 1.0) * M_PI);

                xyz += vector(1);
                xyz *= vector(0.5);

                blend_factor = xyz[0] * xyz[1] * xyz[2];
            }
        }
    }
    return outside;
}
