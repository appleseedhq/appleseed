
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

#ifndef APPLESEED_SHADERS_MICROFACET_H
#define APPLESEED_SHADERS_MICROFACET_H

#define APPLESEED_DEFAULT_MDF_DISTRIBUTION "sharp"
#define APPLESEED_DEFAULT_MDF_ROUGHNESS 0.1

#define APPLESEED_MDF_DISTRIBUTION_METADATA                                       \
    string help = "Microfacet distribution to use: Specular, Beckmann or GGX.",   \
    string widget = "popup",                                                      \
    string options = "sharp|beckmann|ggx"

#define APPLESEED_MDF_ROUGHNESS_METADATA     \
    string help = "Roughness",               \
    float min = 0.001,                       \
    float max = 1.0

#define APPLESEED_DEFAULT_ANISOTROPY 0.0

#define APPLESEED_ANISOTROPY_METADATA        \
    string help = "Anisotropy",              \
    float min = -1.0,                        \
    float max = 1.0

// TODO: This needs a better name...
void mdf_roughness(float roughness, float anisotropy, output float ax, output float ay)
{
    float aspect = sqrt(1.0 - fabs(anisotropy) * 0.9);
    float r2 = roughness * roughness;

    ax = max(0.001, r2 / aspect);
    ay = max(0.001, r2 * aspect);

    if (anisotropy < 0)
    {
        // Swap ax and ay.
        float tmp = ax;
        ax = ay;
        ay = tmp;
    }
}

#endif
