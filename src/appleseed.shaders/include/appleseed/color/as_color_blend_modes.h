
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Luis Barrancos, The appleseedhq Organization
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

#define NCOMPS  3

#include "appleseed/color/as_color_helpers.h"
#include "appleseed/color/as_color_transforms.h"
#include "appleseed/math/as_math_helpers.h"


//
// References:
//
//      Common blend modes.
//
//      https://dunnbypaul.net/blends/
//      https://en.wikipedia.org/wiki/Blend_modes
//      https://www.adobe.com/content/dam/Adobe/en/devnet/pdf/pdfs/pdf_reference_archives/blend_modes.pdf
//

float blend_screen(float A, float B)
{
    return (A >= 0.0 && A <= 1.0 && B >= 0.0 && B <= 1.0)
        ? A + B - A * B
        : max(A, B);
}

float blend_overlay(float A, float B)
{
    return (A < 0.5)
        ? 2.0 * A * B
        : (1.0 - 2.0 * (1.0 - A) * (1.0 - B));
}

float blend_color_burn(float A, float B)
{
    return (B > 0.0) ? 1.0 - min(1.0, (1.0 - A) / B) : 0.0;
}

float blend_color_dodge(float A, float B)
{
    return (B < 1.0) ? min(1.0, A / (1.0 - B)) : 1.0;
}

// There is a real zoo of "soft light" equations out there. Use Natron/Nuke's.

float blend_soft_light(float A, float B)
{
    return (A * B < 1.0)
        ? A * (2.0 * B + (A * (1.0 - B * A)))
        : 2.0 * B * A;
}

float blend_hard_light(float A, float B)
{
    return (B <= 0.5) ? A * 2.0 * B : blend_screen(A, 2.0 * B - 1.0);
}

float blend_linear_light(float A, float B)
{
    return (B < 0.5)
        ? max(0.0, A + 2.0 * B - 1.0)
        : min(1.0, A + 2.0 * (B - 0.5));
}

color blendmode_color_burn(color A, color B)
{
    color rgb;
    
    for (int i = 0; i < NCOMPS; ++i)
    {
        rgb[i] = blend_color_burn(A[i], B[i]);
    }
    return rgb;
}

color blendmode_color_dodge(color A, color B)
{
    color rgb;

    for (int i = 0; i < NCOMPS; ++i)
    {
        rgb[i] = blend_color_dodge(A[i], B[i]);
    }
    return rgb;
}

color blendmode_darken(color A, color B)
{
    return min(A, B);
}

color blendmode_exclusion(color A, color B)
{
    color rgb;

    for (int i = 0; i < NCOMPS; ++i)
    {
        rgb[i] = A[i] + B[i] - 2.0 * A[i] * B[i];
    }
    return rgb;
}

color blendmode_hard_light(color A, color B)
{
    color rgb;
    
    for (int i = 0; i < NCOMPS; ++i)
    {
        rgb[i] = blend_hard_light(A[i], B[i]);
    }
    return rgb;
}

color blendmode_lighten(color A, color B)
{
    return max(A, B);
}

color blendmode_overlay(color A, color B)
{
    color rgb;

    for (int i = 0; i < NCOMPS; ++i)
    {
        rgb[i] = blend_overlay(A[i], B[i]);
    }
    return rgb;
}

color blendmode_screen(color A, color B)
{
    color rgb;

    for (int i = 0; i < NCOMPS; ++i)
    {
        rgb[i] = blend_screen(A[i], B[i]);
    }
    return rgb;
}

color blendmode_soft_light(color A, color B)
{
    color rgb;

    for (int i = 0; i < NCOMPS; ++i)
    {
        rgb[i] = blend_soft_light(A[i], B[i]);
    }
    return rgb;
}

color blendmode_vivid_light(color A, color B)
{
    color rgb;

    for (int i = 0; i < NCOMPS; ++i)
    {
        rgb[i] = (B[i] < 0.5)
            ? blend_color_burn(A[i], 2.0 * B[i])
            : blend_color_dodge(A[i], 2.0 * (B[i] - 0.5));
    }
    return rgb;
}

color blendmode_pin_light(color A, color B)
{
    color rgb;

    for (int i = 0; i < NCOMPS; ++i)
    {
        rgb[i] = (B[i] > 0.5)
            ? max(A[i], 2.0 * (B[i] - 0.5))
            : min(A[i], 2.0 * B[i]);
    }
    return rgb;
}

color blendmode_linear_light(color A, color B)
{
    color rgb;

    for (int i = 0; i < NCOMPS; ++i)
    {
        rgb[i] = blend_linear_light(A[i], B[i]); // A+2B-1
    }
    return rgb;
}


//
// Note:
//
//      The "hue", "saturation", "color", "luminosity" blend modes in PS
//      require the LCH color space, but that is LCH_ab (from L*a*b*, not
//      L*u*v*). CIELAB requires CIEXYZ as a connection space, so we
//      should use the working/rendering space RGB primaries and whitepoint,
//      default to ITU-R BT.709/Rec.709 and D65.
//

color blendmode_hue(
    color A,
    color B,
    string colorspace,
    string illuminant)
{
    color A_LCh_ab = transform_linear_RGB_to_CIELCh_ab(
        A,
        colorspace,
        illuminant);

    color B_LCh_ab = transform_linear_RGB_to_CIELCh_ab(
        B,
        colorspace,
        illuminant);

    return transform_CIELCh_ab_to_linear_RGB(
        color(A_LCh_ab[0], B_LCh_ab[1], B_LCh_ab[2]),
        colorspace,
        illuminant);
}

color blendmode_saturation(
    color A,
    color B,
    string colorspace,
    string illuminant)
{
    color A_LCh_ab = transform_linear_RGB_to_CIELCh_ab(
        A,
        colorspace,
        illuminant);

    color B_LCh_ab = transform_linear_RGB_to_CIELCh_ab(
        B,
        colorspace,
        illuminant);

    return transform_CIELCh_ab_to_linear_RGB(
        color(A_LCh_ab[0], B_LCh_ab[1], A_LCh_ab[2]),
        colorspace,
        illuminant);
}

color blendmode_color(
    color A,
    color B,
    string colorspace,
    string illuminant)
{
    color A_LCh_ab = transform_linear_RGB_to_CIELCh_ab(
        A,
        colorspace,
        illuminant);

    color B_LCh_ab = transform_linear_RGB_to_CIELCh_ab(
        B,
        colorspace,
        illuminant);

    return transform_CIELCh_ab_to_linear_RGB(
        color(A_LCh_ab[0], A_LCh_ab[1], B_LCh_ab[2]),
        colorspace,
        illuminant);
}

color blendmode_luminosity(
    color A,
    color B,
    string colorspace,
    string illuminant)
{
    color A_LCh_ab = transform_linear_RGB_to_CIELCh_ab(
        A,
        colorspace,
        illuminant);

    color B_LCh_ab = transform_linear_RGB_to_CIELCh_ab(
        B,
        colorspace,
        illuminant);

    return transform_CIELCh_ab_to_linear_RGB(
        color(B_LCh_ab[0], A_LCh_ab[1], A_LCh_ab[2]),
        colorspace,
        illuminant);
}

color blendmode_hue(color A, color B)
{
    return blendmode_hue(A, B, "Rec.709", "D65");
}

color blendmode_saturation(color A, color B)
{
    return blendmode_saturation(A, B, "Rec.709", "D65");
}

color blendmode_color(color A, color B)
{
    return blendmode_color(A, B, "Rec.709", "D65");
}

color blendmode_luminosity(color A, color B)
{
    return blendmode_luminosity(A, B, "Rec.709", "D65");
}

color blendmode_divide(color A, color B)
{
    color rgb;

    for (int i = 0; i < NCOMPS; ++i)
    {
        rgb[i] = (B[i] > 0.0) ? A[i] / B[i] : 0.0;
    }
    return rgb;
}

color blend_color(string mode, color A, color B)
{
    color rgb;

    if (mode == "Darken")
    {
        rgb = blendmode_darken(A, B);
    }
    else if (mode == "Multiply")
    {
        rgb = A * B;
    }
    else if (mode == "Color Burn")
    {
        rgb = blendmode_color_burn(A, B);
    }
    else if (mode == "Linear Burn")
    {
        rgb = A + B - color(1);
    }
    else if (mode == "Lighten")
    {
        rgb = blendmode_lighten(A, B);
    }
    else if (mode == "Screen")
    {
        rgb = blendmode_screen(A, B);
    }
    else if (mode == "Color Dodge")
    {
        rgb = blendmode_color_dodge(A, B);
    }
    else if (mode == "Linear Dodge")
    {
        rgb = A + B;
    }
    else if (mode == "Overlay")
    {
        rgb = blendmode_overlay(A, B);
    }
    else if (mode == "Soft Light")
    {
        rgb = blendmode_soft_light(A, B);
    }
    else if (mode == "Hard Light")
    {
        rgb = blendmode_hard_light(A, B);
    }
    else if (mode == "Vivid Light")
    {
        rgb = blendmode_vivid_light(A, B);
    }
    else if (mode == "Linear Light")
    {
        rgb = blendmode_linear_light(A, B);
    }
    else if (mode == "Pin Light")
    {
        rgb = blendmode_pin_light(A, B);
    }
    else if (mode == "Difference")
    {
        rgb = abs(A - B);
    }
    else if (mode == "Exclusion")
    {
        rgb = blendmode_exclusion(A, B);
    }
    else if (mode == "Subtract")
    {
        rgb = A - B;
    }
    else if (mode == "Divide")
    {
        rgb = blendmode_divide(A, B);
    }
    else if (mode == "Hue")
    {
        rgb = blendmode_hue(A, B);
    }
    else if (mode == "Saturation")
    {
        rgb = blendmode_saturation(A, B);
    }
    else if (mode == "Color")
    {
        rgb = blendmode_color(A, B);
    }
    else if (mode == "Luminosity")
    {
        rgb = blendmode_luminosity(A, B);
    }
    else
    {
#ifdef DEBUG
        string shadername = "";
        getattribute("shader:shadername", shadername);
        warning("[DEBUG]: Invalid blend mode %s in %s, %s:%d\n",
                mode, shadername, __FILE__, __LINE__);
#endif
        rgb = color(0);
    }
    return rgb;
}


//
// References:
//
//      "Compositing Digital Images", by T.Porter and T.Duff
//      Computer Graphics Volume 18, Number 3, July 1984, pp 253-259
//
//      https://keithp.com/~keithp/porterduff/
//      https://www.svgopen.org/2005/papers/abstractsvgopen/
//

color composite_color_rgba(
    string mode,
    color A,
    float A_alpha,
    color B,
    float B_alpha,
    output float alpha)
{
    color rgb;

    if (mode == "Source") // A (Src)
    {
        rgb = A;
        alpha = A_alpha;
    }
    else if (mode == "Destination") // B (Dst)
    {
        rgb = B;
        alpha = B_alpha;
    }
    else if (mode == "Over") // Src-Over
    {
        rgb = A + B * (1 - A_alpha);
        alpha = A_alpha + B_alpha - A_alpha * B_alpha;
    }
    else if (mode == "Under") // Dst-Over
    {
        rgb = B + A * (1 - B_alpha);
        alpha = A_alpha + B_alpha - A_alpha * B_alpha;
    }
    else if (mode == "In") // Src-In
    {
        rgb = A * B_alpha;
        alpha = A_alpha * B_alpha;
    }
    else if (mode == "Mask") // Dst-In
    {
        rgb = B * A_alpha;
        alpha = A_alpha * B_alpha;
    }
    else if (mode == "Out") // Src-Out
    {
        rgb = A * (1.0 - B_alpha);
        alpha = A_alpha * (1.0 - B_alpha);
    }
    else if (mode == "Stencil") // Dst-Out
    {
        rgb = B * (1.0 - A_alpha);
        alpha = B_alpha * (1.0 - A_alpha);
    }
    else if (mode == "Atop") // Src-Atop
    {
        rgb = A * B_alpha + B * (1.0 - A_alpha);
        alpha = B_alpha;
    }
    else if (mode == "Dst-Atop") //
    {
        rgb = B * A_alpha + A * (1.0 - B_alpha);
        alpha = A_alpha;
    }
    else if (mode == "Xor") // Xor
    {
        rgb = A * (1.0 - B_alpha) + B * (1.0 - A_alpha);
        alpha = A_alpha + B_alpha - 2.0 * A_alpha * B_alpha;
    }
    else if (mode == "Matte")
    {
        rgb = mix(A, B, A_alpha);
        alpha = mix(A_alpha, B_alpha, A_alpha);
    }
    else
    {
#ifdef DEBUG
        string shadername = "";
        getattribute("shader:shadername", shadername);
        warning("[DEBUG]: Unknown composite mode %s in %s, %s:%d\n",
                shadername, mode, __FILE__, __LINE__);
#endif
        rgb = color(0);
        alpha = 0.0;
    }
    return rgb;
}
