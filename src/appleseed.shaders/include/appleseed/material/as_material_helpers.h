
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

#ifndef AS_MATERIAL_HELPERS_H
#define AS_MATERIAL_HELPERS_H

#define IMPORTANCE_THRESHOLD    1.0e-5

float microfacet_roughness(float roughness, float depth_scale)
{
    float out = roughness;

    if (depth_scale > 1.0)
    {
        int ray_depth;
        getattribute("path:ray_depth", ray_depth);

        if (ray_depth)
        {
            out = roughness * depth_scale * ray_depth;
        }
    }
    return out;
}

float ior_from_normal_reflectance(float f0)
{
    float sqrt_f0 = sqrt(f0);

    return (sqrt_f0 + 1) / (1 - sqrt_f0);
}

#endif // !AS_MATERIAL_HELPERS_H
