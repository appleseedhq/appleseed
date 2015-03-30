
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014 The masked shader writer, The appleseedhq Organization
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

#ifndef APPLESEED_SHADERS_FRESNEL_H
#define APPLESEED_SHADERS_FRESNEL_H

color fresnel_conductor(vector I, normal N, color n, color k)
{
    float CosTheta = abs(dot(I, N));

    color n2 = n * n;
    color k2 = k * k;
    color N2PlusK2 = n2 + k2;
    
    float CosTheta2 = CosTheta * CosTheta;
    color NCosTheta = n * 2.0 * CosTheta;

    color RPar = 
        ((N2PlusK2 * CosTheta2) - NCosTheta + 1) /
        ((N2PlusK2 * CosTheta2) + NCosTheta + 1);

    color RPerp = 
        (N2PlusK2 - NCosTheta + CosTheta2) / 
        (N2PlusK2 + NCosTheta + CosTheta2);

    return (RPar + RPerp) / 2.0;        
}

//
// Reference:
// Ole Gulbrandsen: Artist Friendly Metallic Fresnel
//
// (To Appear in Journal of Computer Graphics Techniques)
//
color artist_friendly_fresnel_conductor(vector I, normal N, color r, color g)
{
    color rr = clamp(r, color(0.0), color(0.99));
    color NMin = (color(1.0) - rr) / (color(1.0) + rr);
    color NMax = (color(1.0) + sqrt(rr)) / (color(1.0) - sqrt(rr));
    color n = mix(NMax, NMin, g);

    color DoubleNPlusOne  = (n + color(1.0)) * (n + color(1.0));
    color DoubleNMinusOne = (n - color(1.0)) * (n - color(1.0));

    color k = sqrt((DoubleNPlusOne * rr - DoubleNMinusOne) / (color(1.0) - rr));

    return fresnel_conductor(I, N, n, k);
}

//
// Some presets for the Artist Friendly Metallic Fresnel.
// From alShaders: https://bitbucket.org/anderslanglands/alshaders/wiki/Home
//

// Aluminium
#define APPLESEED_PRESET_AL_R color(0.914, 0.921, 0.921)
#define APPLESEED_PRESET_AL_G color(0.971, 0.979, 0.989)

// Chrome
#define APPLESEED_PRESET_CR_R color(0.548, .549, .570)
#define APPLESEED_PRESET_CR_G color(0.579, .598, .620)

// Copper
#define APPLESEED_PRESET_CU_R color(0.985, 0.649, 0.546)
#define APPLESEED_PRESET_CU_G color(0.996, 0.918, 0.859)

// Gold
#define APPLESEED_PRESET_AU_R color(0.99, 0.791587, 0.3465)
#define APPLESEED_PRESET_AU_G color(0.99, 0.9801, 0.792)

// Platinum
#define APPLESEED_PRESET_PT_R color(0.679, 0.642, 0.582)
#define APPLESEED_PRESET_PT_G color(0.785, 0.789, 0.783)

// Silver
#define APPLESEED_PRESET_AG_R color(0.97, 0.959109, 0.92441)
#define APPLESEED_PRESET_AG_G color(0.999, 0.999, 0.998)

// Titanium
#define APPLESEED_PRESET_TI_R color(0.55, 0.501692, 0.4477)
#define APPLESEED_PRESET_TI_G color(0.689119, 0.683298, 0.693)

// Tungsten
#define APPLESEED_PRESET_W_R color(0.504, 0.49513, 0.475776)
#define APPLESEED_PRESET_W_G color(0.403, 0.419, 0.422)

#endif
