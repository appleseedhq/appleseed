
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
    color coeffs;

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
           coeffs[2] * in_C[2];
}

float as_luminance_D60(color in_C, string colorspace)
{
    color coeffs;

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
           coeffs[2] * in_C[2];
}

float as_luminance_DCI(color in_C, string colorspace)
{
    color coeffs;

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
           coeffs[2] * in_C[2];
}

float as_luminance(color in_C, string colorspace, string illuminant)
{
    float Y;

    if (illuminant == "D60")
    {
        Y = as_luminance_D60(in_C, colorspace);
    }
    else if (illuminant == "D65")
    {
        Y = as_luminance_D65(in_C, colorspace);
    }
    else if (illuminant == "DCI")
    {
        Y = as_luminance_DCI(in_C, colorspace);
    }
    else
    {
#ifdef DEBUG
        string shadername = "";
        getattribute("shader:shadername", shadername);

        warning("[WARNING]: Unsupported illuminant in %s, %s:%i\n",
                shadername, __FILE__, __LINE__);
#endif
        Y = 0;
    }
    return Y;
}

float as_luminance(color in_C, string colorspace)
{
    return as_luminance_D65(in_C, colorspace);
}

//
// Reference:
//
//      https://help.thefoundry.co.uk/nuke/8.0/content/user_guide/merging/merge_operations.html
//      https://support.solidangle.com/display/A5AFMUG/Composite
//      https://renderman.pixar.com/resources/RenderMan_20/PxrBlend.html

color texture_blend(
    color A,
    float A_alpha,
    color B,
    float B_alpha,
    int mode,
    int clamp_output,
    output float out_alpha)
{
    // NUke modes, or prman
    // burn, dodge, darken, darker color

    color out_rgb = color(0);

    if (mode == 0)
    {
        // passthrough A
        out_rgb = A;
        //out_alpha = A_alpha;
    }
    else if (mode == 1)
    {
        // passthrough B
        out_rgb = B;
        //out_alpha = B_alpha;
    }
    else if (mode == 2)
    {
        // Atop: Ab + B(1-a)
        //out_rgb = A * B_alpha + B * (1.0 - A_alpha);
    }
    else if (mode == 3)
    {
        // average: (A+B)/2
        out_rgb = (A + B) / 2.0;
        //out_alpha = (A_alpha + B_alpha) / 2.0;
    }
    else if (mode == 4)
    {
        // color-burn:
        out_rgb = 1.0 - (1.0 - B) / A;
        //out_alpha = 1.0 - (1.0 - B_alpha) / A_alpha;
    }
    else if (mode == 5)
    {
        // color dodge:
        out_rgb = B / (1.0 - A);
        //out_alpha = B_alpha / (1.0 - A_alpha);
    }
    else if (mode == 6)
    {
        // conjoint-over
        out_rgb = (A_alpha > B_alpha)
            ? A + B * (1.0 - A_alpha) / B_alpha
            : A;
    }
    else if (mode == 7)
    {
        // difference
        out_rgb = abs(A - B);
        //out_alpha = abs(A_alpha - B_alpha);
    }
    else if (mode == 8)
    {
        // disjoint over
        out_rgb = (A_alpha + B_alpha < 1.0)
            ? A + B
            : A + B * (1.0 - A_alpha) / B_alpha;
    }
    else if (mode == 9)
    {
        // divide
        out_rgb = (B != 0) ? A / B : color(0);
        //out_alpha = (B_alpha != 0) ? A_alpha / B_alpha : 0.0;
    }
    else if (mode == 10)
    {
        // exclusion
        out_rgb = A + B - 2.0 * A * B;
        //out_alpha = A_alpha + B_alpha - 2.0 * A_alpha * B_alpha;
    }
    else if (mode == 11)
    {
        // from
        out_rgb = B - A;
        //out_alpha = B_alpha - A_alpha;
    }
    else if (mode == 12)
    {
        // geometric
        out_rgb = 2.0 * A * B / (A + B);
        //out_alpha = 2.0 * A_alpha * B_alpha / (A_alpha + B_alpha);
    }
    else if (mode == 13)
    {
        // hard light, if A<0.5 A * B, else screen
        // screen A + B - A * B, if A and B in [0,1] else max(A, B)

        if (max(A) < 0.5)
        {
            out_rgb = A * B;
        }
        else
        {
            out_rgb =
                (min(A) < 0.0 && min(B) < 0.0 && max(A) > 1.0 && max(B) > 1.0)
                ? max(A, B)
                : A + B - A * B;
        }
    }
    else if (mode == 14)
    {
        // hypot diagonal
        out_rgb = color(hypot(A[0], B[0]),
                        hypot(A[1], B[1]),
                        hypot(A[2], B[2]));

        //out_alpha = hypot(A_alpha, B_alpha);
    }
    else if (mode == 15)
    {
        // In
        out_rgb = A * B_alpha;
    }
    else if (mode == 16)
    {
        // Mask
        out_rgb = B * A_alpha;
    }
    else if (mode == 17)
    {
        // Matte
        out_rgb = mix(B, A, A_alpha);
    }
    else if (mode == 18)
    {
        // Max
        out_rgb = max(A, B);
        //out_alpha = max(A_alpha, B_alpha);
    }
    else if (mode == 19)
    {
        // Min
        out_rgb = min(A, B);
        //out_alpha = min(A_alpha, B_alpha);
    }
    else if (mode == 20)
    {
        // Minus
        out_rgb = A - B;
        //out_alpha = A_alpha - B_alpha;
    }
    else if (mode == 21)
    {
        // Multiply
        out_rgb = A * B;
        //out_alpha = A_alpha * B_alpha;
    }
    else if (mode == 22)
    {
        // Out
        out_rgb = A * (1 - B_alpha);
    }
    else if (mode == 23)
    {
        // Over
        out_rgb = A + B * (1.0 - A_alpha);
    }
    else if (mode == 24)
    {
        // Overlay
        //  if B < 0.5 A * B, else screen
        // screen A + B - A * B, if A and B in [0,1] else max(A, B)

        if (max(B) < 0.5)
        {
            out_rgb = A * B;
        }
        else
        {
            out_rgb = (min(A) < 0.0 && min(B) < 0.0 &&
                       max(A) > 1.0 && max(B) > 1.0)
                ? max(A, B)
                : A + B - A * B;
        }

        // Alpha is (or should be) in [0,1].
        // out_alpha = (B_alpha < 0.5)
        //    ? A_alpha * B_alpha
        //    : A_alpha + B_alpha - A_alpha * B_alpha;
    }
    else if (mode == 25)
    {
        // Plus
        out_rgb = A + B;
    }
    else if (mode == 26)
    {
        // Screen
        out_rgb = (min(A) < 0.0 && min(B) < 0.0 &&
                   max(A) > 1.0 && max(B) > 1.0)
            ? max(A, B)
            : A + B - A * B;

        //out_alpha = A_alpha + B_alpha - A_alpha * B_alpha;
    }
    else if (mode == 27)
    {
        // Soft light ????
        ;
    }
    else if (mode == 28)
    {
        // Stencil
        out_rgb = B * (1.0 - A_alpha);
    }
    else if (mode == 29)
    {
        // Under
        out_rgb = A * (1.0 - B_alpha) + B;
    }
    else if (mode == 30)
    {
        // Xor
        out_rgb = A * (1.0 - B_alpha) + B * (1.0 - A_alpha);
    }

    out_alpha = clamp(B_alpha, 0.0, 1.0);

    return (clamp_output) ? clamp(out_rgb, color(0), color(1)) : out_rgb;
}

#endif // !AS_COLOR_HELPERS_H
