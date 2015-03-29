
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

#ifndef APPLESEED_SHADERS_NOISE_H
#define APPLESEED_SHADERS_NOISE_H

#define APPLESEED_DECLARE_GABOR_NOISE_PARAMS                   \
    int AnisotropyMode = 0                                     \
    [[                                                         \
        string widget = "mapper",                              \
        string options = "Isotropic:0|Anisotropic:1|Hybrid:2"  \
    ]],                                                        \
    vector Direction = 0,                                      \
    float Bandwidth = 1.0,                                     \
    float Impulses = 16

#define APPLESEED_GABOR_NOISE_PARAMS   \
    "anisotropic", AnisotropyMode,     \
    "direction", Direction,            \
    "bandwidth", Bandwidth,            \
    "impulses", Impulses

float do_noise4d(string NoiseType, vector P, float Time, int Periodic, vector Period, float TimePeriod)
{
    if (Periodic)
    {
        return pnoise(
            NoiseType,
            P,
            Time,
            Period,
            TimePeriod);
    }
    else
    {
        return noise(
            NoiseType,
            P,
            Time);
    }
}

vector do_noise4d(string NoiseType, vector P, float Time, int Periodic, vector Period, float TimePeriod)
{
    if (Periodic)
    {
        return pnoise(
            NoiseType,
            P,
            Time,
            Period,
            TimePeriod);
    }
    else
    {
        return noise(
            NoiseType,
            P,
            Time);
    }
}

#endif
