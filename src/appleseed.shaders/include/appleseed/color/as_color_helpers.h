
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

#ifndef AS_COLOR_HELPERS_H
#define AS_COLOR_HELPERS_H

#include "appleseed/color/as_chromatic_adaptation.h"
#include "appleseed/color/as_colorimetry.h"

// The luminance coefficients are provided by the Y value, so when the
// white points of the color space differ from the requested white point,
// the RGB->XYZ matrices are adjusted with the Bradford CAT.

float as_luminance_D65(color in_C, string colorspace)
{
    color coeffs = color(0);

    if (colorspace == "Rec.601")
    {
        coeffs = color(REC601_D65_LUMINANCE_COEFFS);
    }
    else if (colorspace == "Rec.709" || colorspace == "sRGB")
    {
        coeffs = color(REC709_D65_LUMINANCE_COEFFS);
    }
    else if (colorspace == "AdobeRGB")
    {
        coeffs = color(ADOBERGB_D65_LUMINANCE_COEFFS);
    }
    else if (colorspace == "Rec.2020")
    {
        coeffs = color(REC2020_D65_LUMINANCE_COEFFS);
    }
    else if (colorspace == "ACES")
    {
        coeffs = color(ACES_D65_LUMINANCE_COEFFS);
    }
    else if (colorspace == "ACEScg")
    {
        coeffs = color(ACESCG_D65_LUMINANCE_COEFFS);
    }
    else if (colorspace == "DCI-P3")
    {
        coeffs = color(DCIP3_D65_LUMINANCE_COEFFS);
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

float as_luminance_D60(color in_C, string colorspace)
{
    color coeffs = color(0);

    if (colorspace == "Rec.601")
    {
        coeffs = color(REC601_D60_LUMINANCE_COEFFS);
    }
    else if (colorspace == "Rec.709" || colorspace == "sRGB")
    {
        coeffs = color(REC709_D65_LUMINANCE_COEFFS);
    }
    else if (colorspace == "AdobeRGB")
    {
        coeffs = color(ADOBERGB_D65_LUMINANCE_COEFFS);
    }
    else if (colorspace == "Rec.2020")
    {
        coeffs = color(REC2020_D65_LUMINANCE_COEFFS);
    }
    else if (colorspace == "ACES")
    {
        coeffs = color(ACES_D65_LUMINANCE_COEFFS);
    }
    else if (colorspace == "ACEScg")
    {
        coeffs = color(ACESCG_D65_LUMINANCE_COEFFS);
    }
    else if (colorspace == "DCI-P3")
    {
        coeffs = color(DCIP3_D65_LUMINANCE_COEFFS);
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

float as_luminance_DCI(color in_C, string colorspace)
{
    color coeffs = color(0);

    if (colorspace == "Rec.601")
    {
        coeffs = color(REC601_DCI_LUMINANCE_COEFFS);
    }
    else if (colorspace == "Rec.709" || colorspace == "sRGB")
    {
        coeffs = color(REC709_DCI_LUMINANCE_COEFFS);
    }
    else if (colorspace == "AdobeRGB")
    {
        coeffs = color(ADOBERGB_DCI_LUMINANCE_COEFFS);
    }
    else if (colorspace == "Rec.2020")
    {
        coeffs = color(REC2020_DCI_LUMINANCE_COEFFS);
    }
    else if (colorspace == "ACES")
    {
        coeffs = color(ACES_DCI_LUMINANCE_COEFFS);
    }
    else if (colorspace == "ACEScg")
    {
        coeffs = color(ACESCG_DCI_LUMINANCE_COEFFS);
    }
    else if (colorspace == "DCI-P3")
    {
        coeffs = color(DCIP3_DCI_LUMINANCE_COEFFS);
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

float as_luminance(color in_C, string colorspace, string illuminant)
{
    color coeffs = color(0);

    if (illuminant == "D60")
    {
        coeffs = as_luminance_D60(in_C, colorspace);
    }
    else if (illuminant == "DCI")
    {
        coeffs = as_luminance_DCI(in_C, colorspace);
    }
    else
    {
        coeffs = as_luminance_D65(in_C, colorspace);
    }
    return coeffs[0] * in_C[0] +
           coeffs[1] * in_C[1] +
           coeffs[2] + in_C[2];
}

float as_luminance(color in_C, string colorspace)
{
    color coeffs = as_luminance_D65(in_C, colorspace);

    return coeffs[0] * in_C[0] +
           coeffs[1] * in_C[1] +
           coeffs[2] * in_C[2];
}

float as_min_component(color in_C)
{
    return min(in_C[0], min(in_C[1], in_C[2]));
}

float as_max_component(color in_C)
{
    return max(in_C[0], max(in_C[1], in_C[2]));
}

#endif // !AS_COLOR_HELPERS_H
