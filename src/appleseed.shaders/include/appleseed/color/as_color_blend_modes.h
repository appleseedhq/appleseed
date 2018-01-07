
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
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

#ifndef AS_COLOR_BLEND_MODES_H
#define AS_COLOR_BLEND_MODES_H

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
//      Notice that the photoshop layer order (or Natron, Nuke) is swapped.
//      A layer A input to Natron, Nuke Merge node A, and layer B to input B,
//      with merge mode set to "hard light", will produce the equivalent in
//      Photoshop of layer B at the base, and layer A on top.
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

// There is a real zoo of "soft light" equations. Use Natron/Nuke's.

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

color blendmode_hue(color A, color B)
{
    color A_HSL = transform_RGB_to_HSL(A);
    color B_HSL = transform_RGB_to_HSL(B);
    return transform_HSL_to_RGB(color(B_HSL[0], B_HSL[1], A_HSL[2]));
}

color blendmode_saturation(color A, color B)
{
    color A_HSL = transform_RGB_to_HSL(A);
    color B_HSL = transform_RGB_to_HSL(B);
    return transform_HSL_to_RGB(color(A_HSL[0], B_HSL[1], A_HSL[2]));
}

color blendmode_lightness(color A, color B)
{
    color A_HSL = transform_RGB_to_HSL(A);
    float B_lum = as_luminance_D65(B, "Rec.601");
    return transform_HSL_to_RGB(color(A_HSL[0], A_HSL[1], B_lum)); // luminosity
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
        rgb = min(A, B);
    }
    if (mode == "Multiply")
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
        rgb = max(A, B);
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
    else if (mode == "Lightness")
    {
        rgb = blendmode_lightness(A, B);
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

color composite_color_rgb(
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
    }
    else
    {
#ifdef DEBUG
        string shadername = "";
        getattribute("shader:shadername", shadername);
        warning("[DEBUG]: Unknown blend mode %s in %s, %s:%d\n",
                shadername, mode, __FILE__, __LINE__);
#endif
        rgb = color(0);
        alpha = 0.0;
    }
    return rgb;
}

//
// Reference:
//
//      http://natron.readthedocs.io/en/master/plugins/net.sf.openfx.MergePlugin.html
//      Some operations can be applied to alpha, others default to "alpha
//      masking" mode, A.a + B.a - A.a * B.a.
//

color rgba_atop(
    color A,
    color B,
    float A_alpha,
    float B_alpha,
    int masking,
    output float alpha)
{
    alpha = A_alpha;
    return A * B_alpha + B * (1.0 - A_alpha);
}

color rgba_average(
    color A,
    color B,
    float A_alpha,
    float B_alpha,
    int masking,
    output float alpha)
{
    alpha = (masking)
        ? A_alpha + B_alpha - A_alpha * B_alpha
        : (A_alpha + B_alpha) / 2.0;

    return (A + B) / 2.0;
}

color rgba_color_burn(
    color A,
    color B,
    float A_alpha,
    float B_alpha,
    int masking,
    output float alpha)
{
    if (masking)
    {
        alpha = A_alpha + B_alpha - A_alpha * B_alpha;
    }
    else
    {
        alpha = (A_alpha > 0.0) ? 1.0 - (1.0 - B_alpha) / A_alpha : 0.0;
    }
    return blendmode_color_burn(A, B);
}

color rgba_color_dodge(
    color A,
    color B,
    float A_alpha,
    float B_alpha,
    int masking,
    output float alpha)
{
    if (masking)
    {
        alpha = A_alpha + B_alpha - A_alpha * B_alpha;
    }
    else
    {
        alpha = (A_alpha != 1.0) ? B_alpha / (1.0 - A_alpha) : 0.0;
    }
    return blendmode_color_dodge(A, B);
}

color rgba_copy(
    color A,
    color B,
    float A_alpha,
    float B_alpha,
    int masking,
    output float alpha)
{
    alpha = B_alpha;
    return A;
}

color rgba_difference(
    color A,
    color B,
    float A_alpha,
    float B_alpha,
    int masking,
    output float alpha)
{
    alpha = (masking)
        ? A_alpha + B_alpha - A_alpha * B_alpha
        : abs(A_alpha - B_alpha);

    return abs(A - B);
}

color rgba_divide(
    color A,
    color B,
    float A_alpha,
    float B_alpha,
    int masking,
    output float alpha)
{
    if (masking)
    {
        alpha = A_alpha + B_alpha - A_alpha * B_alpha;
    }
    else
    {
        alpha = (B_alpha != 0.0) ? A_alpha / B_alpha : 0.0;
    }
    return blendmode_divide(A, B);
}

color rgba_exclusion(
    color A,
    color B,
    float A_alpha,
    float B_alpha,
    int masking,
    output float alpha)
{
    alpha = (masking)
        ? A_alpha + B_alpha - A_alpha * B_alpha
        : A_alpha + B_alpha - 2.0 * A_alpha * B_alpha;

    return blendmode_exclusion(A, B);
}

color rgba_from(
    color A,
    color B,
    float A_alpha,
    float B_alpha,
    int masking,
    output float alpha)
{
    alpha = (masking)
        ? A_alpha + B_alpha - A_alpha * B_alpha
        : B_alpha - A_alpha;

    return B - A;
}

color rgba_grain_extract(
    color A,
    color B,
    float A_alpha,
    float B_alpha,
    int masking,
    output float alpha)
{
    alpha = (masking)
        ? A_alpha + B_alpha - A_alpha * B_alpha
        : B_alpha - A_alpha + 0.5;

    return B - A + 0.5;
}

color rgba_grain_merge(
    color A,
    color B,
    float A_alpha,
    float B_alpha,
    int masking,
    output float alpha)
{
    alpha = (masking)
        ? A_alpha + B_alpha - A_alpha * B_alpha
        : B_alpha + A_alpha - 0.5;

    return B + A - 0.5;
}

color rgba_hard_light(
    color A,
    color B,
    float A_alpha,
    float B_alpha,
    int masking,
    output float alpha)
{
    if (masking)
    {
        alpha = A_alpha + B_alpha - A_alpha * B_alpha;
    }
    else
    {
        alpha = (A_alpha < 0.5)
            ? A_alpha * B_alpha
            : blend_screen(A_alpha, B_alpha);
    }
    return blendmode_hard_light(A, B);
}

color rgba_hue(
    color A,
    color B,
    float A_alpha,
    float B_alpha,
    int masking,
    output float alpha)
{
    alpha = A_alpha + B_alpha - A_alpha * B_alpha;
    return blendmode_hue(A, B);
}

color rgba_in(
    color A,
    color B,
    float A_alpha,
    float B_alpha,
    int masking,
    output float alpha)
{
    alpha = A_alpha * B_alpha;
    return A * B_alpha;
}

color rgba_linear_light(
    color A,
    color B,
    float A_alpha,
    float B_alpha,
    int masking,
    output float alpha)
{
    alpha = (B_alpha > 0.5)
        ? max(0.0, A_alpha + (2.0 * B_alpha - 0.5))
        : A_alpha + 2.0 * B_alpha - 1.0;

    return blendmode_linear_light(A, B);
}

color rgba_mask(
    color A,
    color B,
    float A_alpha,
    float B_alpha,
    int masking,
    output float alpha)
{
    alpha = A_alpha * B_alpha;
    return B * A_alpha;
}

color rgba_matte(
    color A,
    color B,
    float A_alpha,
    float B_alpha,
    int masking,
    output float alpha)
{
    alpha = A_alpha + B_alpha - A_alpha * B_alpha;
    return mix(B, A, A_alpha);
}

color rgba_max(
    color A,
    color B,
    float A_alpha,
    float B_alpha,
    int masking,
    output float alpha)
{
    alpha = (masking)
        ? A_alpha + B_alpha - A_alpha * B_alpha
        : max(A_alpha, B_alpha);

    return max(A, B);
}

color rgba_min(
    color A,
    color B,
    float A_alpha,
    float B_alpha,
    int masking,
    output float alpha)
{
    alpha = (masking)
        ? A_alpha + B_alpha - A_alpha * B_alpha
        : min(A_alpha, B_alpha);

    return min(A, B);
}

color rgba_minus(
    color A,
    color B,
    float A_alpha,
    float B_alpha,
    int masking,
    output float alpha)
{
    alpha = (masking)
        ? A_alpha + B_alpha - A_alpha * B_alpha
        : A_alpha - B_alpha;

    return A - B;
}

color rgba_multiply(
    color A,
    color B,
    float A_alpha,
    float B_alpha,
    int masking,
    output float alpha)
{
    alpha = (masking)
        ? A_alpha + B_alpha - A_alpha * B_alpha
        : A_alpha * B_alpha;

    return (min(A) < 0.0 && min(B) < 0.0) ? A : A * B;
}

color rgba_out(
    color A,
    color B,
    float A_alpha,
    float B_alpha,
    int masking,
    output float alpha)
{
    alpha = A_alpha * (1.0 - B_alpha);
    return A * (1.0 - B_alpha);
}

color rgba_over(
    color A,
    color B,
    float A_alpha,
    float B_alpha,
    int masking,
    output float alpha)
{
    alpha = A_alpha + B_alpha - A_alpha * B_alpha;
    return A + B * (1.0 - A_alpha);
}

color rgba_overlay(
    color A,
    color B,
    float A_alpha,
    float B_alpha,
    int masking,
    output float alpha)
{
    alpha = (masking || B_alpha > 0.5)
        ? A_alpha + B_alpha - A_alpha * B_alpha
        : A_alpha * B_alpha;

    return blendmode_overlay(A, B);
}

color rgba_pin_light(
    color A,
    color B,
    float A_alpha,
    float B_alpha,
    int masking,
    output float alpha)
{
    if (masking)
    {
        alpha = A_alpha + B_alpha - A_alpha * B_alpha;
    }
    else
    {
        alpha = (B_alpha > 0.5)
            ? max(A_alpha, 2.0 * (B_alpha - 0.5))
            : min(A_alpha, 2.0 * B_alpha);
    }

    return blendmode_pin_light(A, B);
}

color rgba_plus(
    color A,
    color B,
    float A_alpha,
    float B_alpha,
    int masking,
    output float alpha)
{
    alpha = (masking)
        ? A_alpha + B_alpha - A_alpha * B_alpha
        : A_alpha + B_alpha;

    return A + B;
}

color rgba_saturation(
    color A,
    color B,
    float A_alpha,
    float B_alpha,
    int masking,
    output float alpha)
{
    alpha = A_alpha + B_alpha - A_alpha * B_alpha;
    return blendmode_saturation(A, B);
}

color rgba_screen(
    color A,
    color B,
    float A_alpha,
    float B_alpha,
    int masking,
    output float alpha)
{
    alpha = A_alpha + B_alpha - A_alpha * B_alpha;
    return blendmode_screen(A, B);
}

color rgba_soft_light(
    color A,
    color B,
    float A_alpha,
    float B_alpha,
    int masking,
    output float alpha)
{
    if (masking)
    {
        alpha = A_alpha + B_alpha - A_alpha * B_alpha;
    }
    else
    {
        alpha = (B_alpha < 0.5)
            ? (1.0 - 2.0 * B_alpha) * sqr(A_alpha) +
                     2.0 * A_alpha * B_alpha
            : (2.0 * B_alpha - 1.0) * sqrt(A_alpha) +
                     2.0 * A_alpha * (1.0 - B_alpha);
    }
    return blendmode_soft_light(A, B);
}

color rgba_stencil(
    color A,
    color B,
    float A_alpha,
    float B_alpha,
    int masking,
    output float alpha)
{
    alpha = B_alpha * (1.0 - A_alpha);
    return B * (1.0 - A_alpha);
}

color rgba_under(
    color A,
    color B,
    float A_alpha,
    float B_alpha,
    int masking,
    output float alpha)
{
    alpha = A_alpha * (1.0 - B_alpha) + B_alpha;
    return A * (1.0 - B_alpha) + B;
}

color rgba_vivid_light(
    color A,
    color B,
    float A_alpha,
    float B_alpha,
    int masking,
    output float alpha)
{
    if (masking)
    {
        alpha = A_alpha + B_alpha - A_alpha * B_alpha;
    }
    else if (B_alpha > 0.5)
    {
        alpha = 1.0 - (1.0 - A_alpha) / (2.0 * (B_alpha - 0.5));
    }
    else if (B_alpha < 0.5)
    {
        alpha = A_alpha / (1.0 - 2.0 * B_alpha);
    }
    else
    {
        alpha = 0.0;
    }
    return blendmode_vivid_light(A, B);
}

color rgba_xor(
    color A,
    color B,
    float A_alpha,
    float B_alpha,
    int masking,
    output float alpha)
{
    alpha = (1.0 - B_alpha) + (1.0 - A_alpha);
    return A * (1.0 - B_alpha) + B * (1.0 - A_alpha);
}


color layer_color(
    string mode,
    color A,
    float A_alpha,
    color B,
    float B_alpha,
    int masking,
    output float alpha)
{
    color rgb = color(0);

    if (mode == "Atop")
    {
        rgb = rgba_atop(A, B, A_alpha, B_alpha, masking, alpha);
    }
    else if (mode == "Average")
    {
        rgb = rgba_average(A, B, A_alpha, B_alpha, masking, alpha);
    }
    else if (mode == "Color Burn")
    {
        rgb = rgba_color_burn(A, B, A_alpha, B_alpha, masking, alpha);
    }
    else if (mode == "Color Dodge")
    {
        rgb = rgba_color_dodge(A, B, A_alpha, B_alpha, masking, alpha);
    }
    else if (mode == "Copy")
    {
        rgb = rgba_copy(A, B, A_alpha, B_alpha, masking, alpha);
    }
    else if (mode == "Difference")
    {
        rgb = rgba_difference(A, B, A_alpha, B_alpha, masking, alpha);
    }
    else if (mode == "Divide")
    {
        rgb = rgba_divide(A, B, A_alpha, B_alpha, masking, alpha);
    }
    else if (mode == "Exclusion")
    {
        rgb = rgba_exclusion(A, B, A_alpha, B_alpha, masking, alpha);
    }
    else if (mode == "From")
    {
        rgb = rgba_from(A, B, A_alpha, B_alpha, masking, alpha);
    }
    else if (mode == "Grain Extract")
    {
        rgb = rgba_grain_extract(A, B, A_alpha, B_alpha, masking, alpha);
    }
    else if (mode == "Grain Merge")
    {
        rgb = rgba_grain_merge(A, B, A_alpha, B_alpha, masking, alpha);
    }
    else if (mode == "Hard Light")
    {
        rgb = rgba_hard_light(A, B, A_alpha, B_alpha, masking, alpha);
    }
    else if (mode == "Hue")
    {
        rgb = rgba_hue(A, B, A_alpha, B_alpha, masking, alpha);
    }
    else if (mode == "In")
    {
        rgb = rgba_in(A, B, A_alpha, B_alpha, masking, alpha);
    }
    else if (mode == "Linear Light")
    {
        rgb = rgba_linear_light(A, B, A_alpha, B_alpha, masking, alpha);
    }
    else if (mode == "Mask")
    {
        rgb = rgba_mask(A, B, A_alpha, B_alpha, masking, alpha);
    }
    else if (mode == "Matte")
    {
        rgb = rgba_matte(A, B, A_alpha, B_alpha, masking, alpha);
    }
    else if (mode == "Max")
    {
        rgb = rgba_max(A, B, A_alpha, B_alpha, masking, alpha);
    }
    else if (mode == "Min")
    {
        rgb = rgba_min(A, B, A_alpha, B_alpha, masking, alpha);
    }
    else if (mode == "Minus")
    {
        rgb = rgba_minus(A, B, A_alpha, B_alpha, masking, alpha);
    }
    else if (mode == "Multiply")
    {
        rgb = rgba_multiply(A, B, A_alpha, B_alpha, masking, alpha);
    }
    else if (mode == "Out")
    {
        rgb = rgba_out(A, B, A_alpha, B_alpha, masking, alpha);
    }
    else if (mode == "Over")
    {
        rgb = rgba_over(A, B, A_alpha, B_alpha, masking, alpha);
    }
    else if (mode == "Overlay")
    {
        rgb = rgba_overlay(A, B, A_alpha, B_alpha, masking, alpha);
    }
    else if (mode == "Pin Light")
    {
        rgb = rgba_pin_light(A, B, A_alpha, B_alpha, masking, alpha);
    }
    else if (mode == "Plus")
    {
        rgb = rgba_plus(A, B, A_alpha, B_alpha, masking, alpha);
    }
    else if (mode == "Saturation")
    {
        rgb = rgba_saturation(A, B, A_alpha, B_alpha, masking, alpha);
    }
    else if (mode == "Screen")
    {
        rgb = rgba_screen(A, B, A_alpha, B_alpha, masking, alpha);
    }
    else if (mode == "Soft Light")
    {
        rgb = rgba_soft_light(A, B, A_alpha, B_alpha, masking, alpha);
    }
    else if (mode == "Stencil")
    {
        rgb = rgba_stencil(A, B, A_alpha, B_alpha, masking, alpha);
    }
    else if (mode == "Under")
    {
        rgb = rgba_under(A, B, A_alpha, B_alpha, masking, alpha);
    }
    else if (mode == "Vivid Light")
    {
        rgb = rgba_vivid_light(A, B, A_alpha, B_alpha, masking, alpha);
    }
    else if (mode == "Xor")
    {
        rgb = rgba_xor(A, B, A_alpha, B_alpha, masking, alpha);
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
        alpha = 0.0;
    }
    return rgb;
}

#endif // !AS_COLOR_BLEND_MODES_H
