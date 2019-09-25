
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

#include "appleseed/color/as_colorimetry.h"

// Common color component transfer functions quantities
#define ADOBE_RGB_1998_GAMMA    2.19921875
#define REC709_GAMMA            2.4
#define REC1886_GAMMA           2.4
#define DCIP3_GAMMA             2.6


//
// Reference:
//
//   sRGB
//   https://en.wikipedia.org/wiki/SRGB
//

float sRGB_EOTF(float value)
{
    float linear_out;

    if (value > 0.04045)
    {
        linear_out = pow((value + 0.055) / 1.055, 2.4);
    }
    else if (value > 0)
    {
        linear_out = value / 12.92;
    }
    else
    {
        linear_out = 0;
    }

    return linear_out;
}

float sRGB_OETF(float value)
{
    float prime_out;

    if (value > 0.0031308)
    {
        prime_out = 1.055 * pow(value, 1 / 2.4) - 0.055;
    }
    else if (value > 0)
    {
        prime_out = value * 12.92;
    }
    else
    {
        prime_out = 0;
    }
    return prime_out;
}

color sRGB_EOTF(color value)
{
    return color(
        sRGB_EOTF(value[0]),
        sRGB_EOTF(value[1]),
        sRGB_EOTF(value[2]));
}

color sRGB_OETF(color value)
{
    return color(
        sRGB_OETF(value[0]),
        sRGB_OETF(value[1]),
        sRGB_OETF(value[2]));
}


//
// Reference:
//
//   BT.709: Parameter Values For The HDTV Standards For Production And
//   International Programme Exchange
//   http://www.itu.int/rec/R-REC-BT.709-6-201506-I/en
//

float Rec709_EOTF(float value)
{
    float linear_out;

    if (value > 0.081)
    {
        linear_out = pow((value + 0.099) / 1.099, (1 / 0.45));
    }
    else if (value > 0)
    {
        linear_out = (1.0 / 4.5) * value;
    }
    else
    {
        linear_out = 0;
    }

    return linear_out;
}

float Rec709_OETF(float value)
{
    float prime_out;

    if (value > 0.081)
    {
        prime_out = 1.099 * pow(value, 0.45) - 0.099;
    }
    else if (value > 0)
    {
        prime_out = value * 4.5;
    }
    else
    {
        prime_out = 0;
    }

    return prime_out;
}

color Rec709_EOTF(color value)
{
    return color(
        Rec709_EOTF(value[0]),
        Rec709_EOTF(value[1]),
        Rec709_EOTF(value[2]));
}

color Rec709_OETF(color value)
{
    return color(
        Rec709_OETF(value[0]),
        Rec709_OETF(value[1]),
        Rec709_OETF(value[2]));
}


//
// Reference:
//
//   BT.2020: Parameter Values for Ultra High Definition Television
//   Systems For Production And International Programme Exchange
//   https://www.itu.int/rec/R-REC-BT.2020/en
//

float Rec2020_EOTF(float value, int bitdepth)
{
    float linear_out;

    if (value > 0)
    {
        float alpha, beta;

        if (bitdepth == 10)
        {
            alpha = 1.099;
            beta  = 0.018;
        }
        else
        {
            alpha = 1.0993;
            beta  = 0.0181;
        }

        if (value < beta)
        {
            linear_out = value / 4.5;
        }
        else
        {
            linear_out = pow((value + (alpha - 1)) / alpha, 1 / 0.45);
        }
    }
    else
    {
        linear_out = 0;
    }

    return linear_out;
}

float Rec2020_OETF(float value, int bitdepth)
{
    float prime_out;

    if (value > 0)
    {
        float alpha, beta;

        if (bitdepth == 10)
        {
            alpha = 1.099;
            beta  = 0.018;
        }
        else
        {
            alpha = 1.0993;
            beta  = 0.0181;
        }

        if (value < beta)
        {
            prime_out = value * 4.5;
        }
        else
        {
            prime_out = alpha * pow(value, 0.45) - (alpha - 1);
        }
    }
    else
    {
        prime_out = 0;
    }

    return prime_out;
}

color Rec2020_EOTF(color value, int bitdepth)
{
    return color(
        Rec2020_EOTF(value[0], bitdepth),
        Rec2020_EOTF(value[1], bitdepth),
        Rec2020_EOTF(value[2], bitdepth));
}

color Rec2020_OETF(color value, int bitdepth)
{
    return color(
        Rec2020_OETF(value[0], bitdepth),
        Rec2020_OETF(value[1], bitdepth),
        Rec2020_OETF(value[2], bitdepth));
}

color Rec2020_EOTF(color value)
{
    return Rec2020_EOTF(value, 12);
}

color Rec2020_OETF(color value)
{
    return Rec2020_OETF(value, 12);
}


//
// Reference:
//
//   BT.1886: Reference Electro-Optical Transfer Function For Flat Panel
//   Displays Used In HDTV Studion Production
//   http://www.itu.int/rec/R-REC-BT.1886-0-201103-I
//
// Note:
//
//   The standard specifies black and white luminance levels for the code
//   values 64, 940 (in a 10-bit system), but they can be the luminance values
//   in nits (cd/m^2), or 0.0 and 1.0 for a normalized system. For our use,
//   we expect the luminance domain to be [0,1].
//
// See also:
//
//   EBU Tech.3320: User Requirements For Video Monitors In Television
//   Production, section 5.1.
//   https://tech.ebu.ch/docs/tech/tech3320.pdf
//

float Rec1886_EOTF(float value)
{
    return (value > 0.0) ? pow(value, REC1886_GAMMA) : 0.0;
}

float Rec1886_OETF(float value)
{
    return (value > 0.0) ? pow(value, 1.0 / REC1886_GAMMA) : 0.0;
}

color Rec1886_EOTF(color value)
{
    return color(
        Rec1886_EOTF(value[0]),
        Rec1886_EOTF(value[1]),
        Rec1886_EOTF(value[2]));
}

color Rec1886_OETF(color value)
{
    return color(
        Rec1886_OETF(value[0]),
        Rec1886_OETF(value[1]),
        Rec1886_OETF(value[2]));
}


//
// Reference:
//
//   Gamma function
//   https://en.wikipedia.org/wiki/Gamma_correction
//

float gamma_CCTF(float value, float gamma)
{
    if (value == 0)
    {
        return 0;
    }
    else
    {
        return pow(value, gamma);
    }
}

color gamma_CCTF(color value, float gamma)
{
    return color(
        gamma_CCTF(value[0], gamma),
        gamma_CCTF(value[1], gamma),
        gamma_CCTF(value[2], gamma));
}


//
// Reference:
//
//   Adobe RGB (1998) Color Image Encoding, version 2005-05, May 2005
//   https://www.adobe.com/digitalimag/adobergb.html
//

float AdobeRGB_EOTF(float value)
{
    return gamma_CCTF(value, 1.0 / ADOBE_RGB_1998_GAMMA);
}

float AdobeRGB_OETF(float value)
{
    return gamma_CCTF(value, ADOBE_RGB_1998_GAMMA);
}

color AdobeRGB_EOTF(color value)
{
    return color(
        AdobeRGB_EOTF(value[0]),
        AdobeRGB_EOTF(value[1]),
        AdobeRGB_EOTF(value[2]));
}

color AdobeRGB_OETF(color value)
{
    return color(
        AdobeRGB_OETF(value[0]),
        AdobeRGB_OETF(value[1]),
        AdobeRGB_OETF(value[2]));
}


//
// Reference:
//
//   Digital Cinema System Specification Version 1.1, page 13, colorimetry
//   http://www.dcimovies.com/archives/spec_v1_1//DCI_DCinema_System_Spec_v1_1.pdf
//

color DCIP3_EOTF(color XYZ)
{
    return gamma_CCTF(XYZ, 1.0 / DCIP3_GAMMA);
}

color DCIP3_OETF(color XYZ)
{
    return gamma_CCTF(XYZ, DCIP3_GAMMA);
}
