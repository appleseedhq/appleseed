
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016 Luis Barrancos, The appleseedhq Organization
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

#ifndef AS_COLOR_HELPERS_H
#define AS_COLOR_HELPERS_H

#include "appleseed/color/as_color_data.h"

float as_luminance(color in_C, string colorspace)
{
    color coeffs;

    if (colorspace == "Rec.601")
    {
        coeffs = color(BT601_LUMINANCE_COEFFS);
    }
    else if (colorspace == "Rec.709")
    {
        coeffs = color(BT709_LUMINANCE_COEFFS);
    }
    else if (colorspace == "sRGB")
    {
        coeffs = color(SRGB_LUMINANCE_COEFFS);
    }
    else if (colorspace == "Rec.2020")
    {
        coeffs = color(BT2020_LUMINANCE_COEFFS);
    }
    else if (colorspace == "Rec.1886")
    {
        coeffs = color(BT1886_LUMINANCE_COEFFS);
    }
    else if (colorspace == "Rec.2100")
    {
        coeffs = color(BT2100_LUMINANCE_COEFFS);
    }
    else if (colorspace == "sRGB")
    {
        coeffs = color(SRGB_LUMINANCE_COEFFS);
    }
    else if (colorspace == "AdobeRGB98")
    {
        coeffs = color(ADOBERGB98_LUMINANCE_COEFFS);
    }
    else
    {
#ifdef DEBUG
        string shadername = "";
        getattribute("shader:shadername", shadername);

        warning("[WARNING]: Unknown color space in shader %s, %s:%i\n",
                shadername, __FILE__, __LINE__);
#endif
        coeffs = color(0);
    }
    return coeffs[0] * in_C[0] +
           coeffs[1] * in_C[1] +
           coeffs[2] + in_C[2];
}

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

float BT709_EOTF(float value)
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

float BT709_OETF(float value)
{
    float prime_out ;

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

color BT709_EOTF(color value)
{
    return color(
        BT709_EOTF(value[0]),
        BT709_EOTF(value[1]),
        BT709_EOTF(value[2]));
}

color BT709_OETF(color value)
{
    return color(
        BT709_OETF(value[0]),
        BT709_OETF(value[1]),
        BT709_OETF(value[2]));
}

// ITU-R BT.2020 transfer functions for bitdepth: 10|12bit system.
float BT2020_EOTF(float value, int bitdepth)
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
    return linear_out ;
}

float BT2020_OETF(float value, int bitdepth)
{
    float prime_out ;

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

color BT2020_EOTF(color value, int bitdepth)
{
    return color(
        BT2020_EOTF(value[0], bitdepth),
        BT2020_EOTF(value[1], bitdepth),
        BT2020_EOTF(value[2], bitdepth));
}

color BT2020_OETF(color value, int bitdepth)
{
    return color(
        BT2020_OETF(value[0], bitdepth),
        BT2020_OETF(value[1], bitdepth),
        BT2020_OETF(value[2], bitdepth));
}

// ITU-R BT.1886 black luminance = 64cd/m², white luminance = 940cd/m².
float BT1886_EOTF(float value, float black_luminance, float white_luminance)
{
    float linear_out;

    if (value > 0)
    {
        float gamma = 2.4;
        float gamma_denom = 1 / gamma;

        float tmp = pow(white_luminance, gamma_denom) -
                    pow(black_luminance, gamma_denom);

        float gain = pow(tmp, gamma);
        float lift = pow(black_luminance, gamma_denom) / tmp;

        linear_out = gain * pow(max(0, value + lift), gamma);
    }
    else
    {
        linear_out = 0;
    }
    return linear_out;
}

float BT1886_OETF(float value, float black_luminance, float white_luminance)
{
    float prime_out;

    if (value > 0)
    {
        float gamma = 2.4;
        float gamma_denom = 1 / gamma;

        float tmp = pow(white_luminance, gamma_denom) -
                    pow(black_luminance, gamma_denom);

        float gain = pow(tmp, gamma);
        float lift = pow(black_luminance, gamma_denom) / tmp;

        prime_out  = pow(value / gain, gamma_denom - lift);
    }
    else
    {
        prime_out = 0;
    }
    return prime_out;
}

color BT1886_EOTF(color value, float black_luminance, float white_luminance)
{
    return color(
        BT1886_EOTF(value[0], black_luminance, white_luminance),
        BT1886_EOTF(value[1], black_luminance, white_luminance),
        BT1886_EOTF(value[2], black_luminance, white_luminance));
}

color BT1886_OETF(color value, float black_luminance, float white_luminance)
{
    return color(
        BT1886_OETF(value[0], black_luminance, white_luminance),
        BT1886_OETF(value[1], black_luminance, white_luminance),
        BT1886_OETF(value[2], black_luminance, white_luminance));
}

float gamma_EOTF(float value, float gamma)
{
    if (value == 0)
    {
        return 0;
    }
    else if (value == 1)
    {
        return value;
    }
    else
    {
        return pow(value, gamma);
    }
}

color gamma_EOTF(color value, color gamma)
{
    return color(
        gamma_EOTF(value[0], gamma[0]),
        gamma_EOTF(value[1], gamma[1]),
        gamma_EOTF(value[2], gamma[2]));
}

float gamma_OETF(float value, float gamma)
{
    if (value == 0)
    {
        return 0;
    }
    else
    {
        return pow(value, 1 / gamma);
    }
}

color gamma_OETF(color value, color gamma)
{
    return color(
        gamma_OETF(value[0], gamma[0]),
        gamma_OETF(value[1], gamma[1]),
        gamma_OETF(value[2], gamma[2]));
}

color DCIP3_EOTF(color value)
{
    return gamma_EOTF(value, DCIP3_GAMMA);
}

color DCIP3_OETF(color value)
{
    return gamma_OETF(value, DCIP3_GAMMA);
}

color AdobeRGB_EOTF(color value)
{
    return gamma_EOTF(value, ADOBERGB98_GAMMA);
}

color AdobeRGB_OETF(color value)
{
    return gamma_EOTF(value, ADOBERGB98_GAMMA);
}

// Get primaries xyz coordinates for given space (some are shared).
void XYZ_chromaticity_coords(string space, output vector xyz[3])
{
    if (space == "Rec.601")
    {
        xyz[0] = BT601_CHROMATICITIES_Rxyz;
        xyz[1] = BT601_CHROMATICITIES_Gxyz;
        xyz[2] = BT601_CHROMATICITIES_Bxyz;
    }
    else if (space == "Rec.709")
    {
        xyz[0] = BT709_CHROMATICITIES_Rxyz;
        xyz[1] = BT709_CHROMATICITIES_Gxyz;
        xyz[2] = BT709_CHROMATICITIES_Bxyz;
    }
    else if (space == "Rec.2020")
    {
        xyz[0] = BT2020_CHROMATICITIES_Rxyz;
        xyz[1] = BT2020_CHROMATICITIES_Gxyz;
        xyz[2] = BT2020_CHROMATICITIES_Bxyz;
    }
    else if (space == "Rec.1886")
    {
        xyz[0] = BT1886_CHROMATICITIES_Rxyz;
        xyz[1] = BT1886_CHROMATICITIES_Gxyz;
        xyz[2] = BT1886_CHROMATICITIES_Bxyz;
    }
    else if (space == "Rec.2100")
    {
        xyz[0] = BT2100_CHROMATICITIES_Rxyz;
        xyz[1] = BT2100_CHROMATICITIES_Gxyz;
        xyz[2] = BT2100_CHROMATICITIES_Bxyz;
    }
    else if (space == "sRGB")
    {
        xyz[0] = SRGB_CHROMATICITIES_Rxyz;
        xyz[1] = SRGB_CHROMATICITIES_Gxyz;
        xyz[2] = SRGB_CHROMATICITIES_Bxyz;
    }
    else if (space == "AdobeRGB98")
    {
        xyz[0] = ADOBERGB98_CHROMATICITIES_Rxyz;
        xyz[1] = ADOBERGB98_CHROMATICITIES_Gxyz;
        xyz[2] = ADOBERGB98_CHROMATICITIES_Bxyz;
    }
    else
    {
#ifdef DEBUG
        string shadername = "";
        getattribute("shader:shadername", shadername);

        warning("[WARNING]: Unknown color space in shader %s, %s:%i\n",
                 shadername, __FILE__, __LINE__);
#endif
        xyz[0] = vector(0);
        xyz[1] = vector(0);
        xyz[2] = vector(0);
    }
}

color transform_RGB2XYZ(color C, string space)
{
    if (space == "sRGB")
    {
        return color(
            dot(vector(RGB2XYZ_D65_SRGB_R0), vector(C)),
            dot(vector(RGB2XYZ_D65_SRGB_R1), vector(C)),
            dot(vector(RGB2XYZ_D65_SRGB_R1), vector(C)));
    }
    else if (space == "AdobeRGB98")
    {
        return color(
            dot(vector(RGB2XYZ_D65_ADOBERGB98_R0), vector(C)),
            dot(vector(RGB2XYZ_D65_ADOBERGB98_R1), vector(C)),
            dot(vector(RGB2XYZ_D65_ADOBERGB98_R2), vector(C)));
    }
    else
    {
#ifdef DEBUG
        string shadername = "";
        getattribute("shader:shadername", shadername);

        warning("[WARNING]: invalid space selected in %s, %s:%i\n",
                shadername, __FILE__, __LINE__);
#endif
        return color(0);
    }
}

color transform_XYZ2RGB(color C, string space)
{
    if (space == "sRGB")
    {
        return color(
            dot(vector(XYZ2RGB_D65_SRGB_R0), vector(C)),
            dot(vector(XYZ2RGB_D65_SRGB_R1), vector(C)),
            dot(vector(XYZ2RGB_D65_SRGB_R1), vector(C)));
    }
    else if (space == "AdobeRGB98")
    {
        return color(
            dot(vector(XYZ2RGB_D65_ADOBERGB98_R0), vector(C)),
            dot(vector(XYZ2RGB_D65_ADOBERGB98_R1), vector(C)),
            dot(vector(XYZ2RGB_D65_ADOBERGB98_R2), vector(C)));
    }
    else
    {
#ifdef DEBUG
        string shadername = "";
        getattribute("shader:shadername", shadername);

        warning("[WARNING]: invalid space selected in %s, %s:%i\n",
                shadername, __FILE__, __LINE__);
#endif
        return color(0);
    }
}

#endif // AS_COLOR_HELPERS_H
