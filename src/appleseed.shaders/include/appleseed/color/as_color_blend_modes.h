
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

#include "appleseed/color/as_color_transforms.h"
#include "appleseed/math/as_math_helpers.h"

//
// References:
//
//      Common blend modes.
//
//      https://dunnbypaul.net/blends/
//      https://en.wikipedia.org/wiki/Blend_modes
//

float blend_screen(float A, float B)
{
    if (B < 0.5)
    {
        return A * B;
    }
    else
    {
        return (A <= 1.0 || B <= 1.0) ? A + B - A * B : max(A, B);
    }
}

float blend_color_burn(float A, float B)
{
    return (A > 0.0) ? 1.0 - (1.0 - B) / A : 0.0;
}

float blend_color_dodge(float A, float B)
{
    return (A != 1.0) ? B / (1.0 - A) : 0.0;
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
        rgb[i] = (A[i] < 0.5) ? A[i] * B[i] : blend_screen(A[i], B[i]);
    }
    return rgb;
}

color blendmode_overlay(color A, color B)
{
    color rgb;

    for (int i = 0; i < NCOMPS; ++i)
    {
        rgb[i] = (B[i] < 0.5) ? A[i] * B[i] : blend_screen(A[i], B[i]);
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
        rgb[i] = (B[i] < 0.5)
            ? (1.0 - 2.0 * B[i]) * sqr(A[i]) + 2.0 * A[i] * B[i]
            : (2.0 * B[i] - 1.0) * sqrt(A[i]) + 2.0 * A[i] * (1.0 - B[i]);
    }
    return rgb;
}

color blendmode_vivid_light(color A, color B)
{
    color rgb;

    for (int i = 0; i < NCOMPS; ++i)
    {
        rgb[i] = (B[i] > 0.5)
            ? blend_color_burn(A[i], 2.0 * (B[i] - 0.5))
            : blend_color_dodge(A[i], 2.0 * B[i]);
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
        rgb[i] = (B[i] > 0.5)
            ? max(0.0, A[i] + (2.0 * B[i] - 0.5))
            : A[i] + 2.0 * B[i] - 1.0;
    }
    return rgb;
}

color blendmode_hue(color A, color B)
{
    color A_HSV = transform_RGB_to_HSV(A);
    color B_HSV = transform_RGB_to_HSV(B);
    return transform_HSV_to_RGB(color(A_HSV[0], B_HSV[1], B_HSV[2]));
}

color blendmode_saturation(color A, color B)
{
    color A_HSV = transform_RGB_to_HSV(A);
    color B_HSV = transform_RGB_to_HSV(B);
    return transform_HSV_to_RGB(color(B_HSV[0], A_HSV[1], B_HSV[2]));
}

color blendmode_color(color A, color B)
{
    color A_HSL = transform_RGB_to_HSL(A);
    color B_HSL = transform_RGB_to_HSL(B);
    return transform_HSL_to_RGB(color(A_HSL[0], A_HSL[1], B_HSL[2]));
}

color blendmode_luminosity(color A, color B)
{
    color A_HSL = transform_RGB_to_HSL(A);
    color B_HSL = transform_RGB_to_HSL(B);
    return transform_HSL_to_RGB(color(B_HSL[0], B_HSL[1], A_HSL[2]));
}

//
// References:
//
//      Natrox Merge Plugin modes:
//      http://natron.readthedocs.io/en/master/plugins/net.sf.openfx.MergePlugin.html
//
//      Nuke Merge Modes:
//      https://help.thefoundry.co.uk/nuke/content/reference_guide/merge_nodes/merge.html
//

color blendmode_hypot(color A, color B)
{
    return color(hypot(A[0], B[0]),
                 hypot(A[1], B[1]),
                 hypot(A[2], B[2]));
}

color blendmode_reflect(color A, color B)
{
    color rgb;

    for (int i = 0; i < NCOMPS; ++i)
    {
        rgb[i] = (B[i] != 1.0)
            ? min(1.0, sqr(A[i]) / (1.0 - B[i]))
            : 1.0;
    }
    return rgb;
}

color blendmode_conjoint_over(
    color A,
    float A_alpha,
    color B,
    float B_alpha)
{
    if (B_alpha > 0.0)
    {
        return (A_alpha > B_alpha)
            ? A
            : A + B * (1.0 - A_alpha / B_alpha);
    }
    else
    {
        return (A_alpha > B_alpha) ? A : color(0);
    }
}

color blendmode_disjoint_over(
    color A,
    float A_alpha,
    color B,
    float B_alpha)
{
    if (B_alpha > 0.0)
    {
        return (A_alpha + B_alpha < 1.0)
            ? A + B
            : A + B * (1.0 - A_alpha) / B_alpha;
    }
    else
    {
        return (A_alpha < 1.0) ? A + B : color(0);
    }
}

color blendmode_freeze(color A, color B)
{
    color rgb;

    for (int i = 0; i < NCOMPS; ++i)
    {
        rgb[i] = (B[i] > 0.0) ? 1.0 - sqrt(1.0 - A[i]) : 0.0;
    }
    return rgb;
}

//
// Reference:
//
//      https://en.wikipedia.org/wiki/Blend_modes
//

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
        for (int i = 0; i < NCOMPS; ++i)
        {
            rgb[i] = (B[i] != 0.0) ? A[i] / B[i] : 0.0;
        }
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
        warning("[DEBUG]: Invalid blend mode %d in %s, %s:%d\n",
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
//

color composite_color(
    string mode,
    color A,
    float A_alpha,
    color B,
    float B_alpha)
{
    color out_rgb;
    
    if (mode == "Copy")
    {
        out_rgb = A;
    }
    else if (mode == "Over")
    {
        out_rgb = A + B * (1.0 - A_alpha);
    }
    else if (mode == "Under")
    {
        out_rgb = A * (1.0 - B) + B;
    }
    else if (mode == "In")
    {
        out_rgb = A * B_alpha;
    }
    else if (mode == "Mask")
    {
        out_rgb = B * A_alpha;
    }
    else if (mode == "Out")
    {
        out_rgb = A * (1.0 - B_alpha);
    }
    else if (mode == "Stencil")
    {
        out_rgb = B * (1.0 - A_alpha);
    }
    else if (mode == "Atop")
    {
        out_rgb = A * B_alpha + B * (1.0 - A_alpha);
    }
    else if (mode == "Xor")
    {
        out_rgb = A * (1.0 - B_alpha) + B * (1.0 - A_alpha);
    }
    else if (mode == "Conjoint Over")
    {
        out_rgb = blendmode_conjoint_over(A, A_alpha, B, B_alpha);
    }
    else if (mode == "Disjoint Over")
    {
        out_rgb = blendmode_disjoint_over(A, A_alpha, B, B_alpha);
    }
    else if (mode == "Matte")
    {
        out_rgb = mix(A, B, A_alpha);
    }
    else
    {
#ifdef DEBUG
        string shadername = "";
        getattribute("shader:shadername", shadername);
        warning("[DEBUG]: Unknown blend mode %d in %s, %s:%d\n",
                shadername, mode, __FILE__, __LINE__);
#endif
        out_rgb = color(0);
    }
    return out_rgb;
}

//
// Reference:
//
//      http://natron.readthedocs.io/en/master/plugins/net.sf.openfx.MergePlugin.html
//      Some operations can be applied to alpha, others default to "alpha
//      masking" mode, A.a + B.a - A.a * B.a.
//

color layer_color(
    string mode,
    color A,
    float A_alpha,
    color B,
    float B_alpha,
    int alpha_masking,
    output float out_alpha)
{
    color out_rgb = color(0);
    out_alpha = A_alpha + B_alpha - A_alpha * B_alpha;

    if (mode == "Atop")
    {
        out_rgb = A * B_alpha + B * (1.0 - A_alpha);
        out_alpha = A_alpha;
    }
    else if (mode == "Average")
    {
        out_rgb = (A + B) / 2.0;

        out_alpha = (alpha_masking)
            ? A_alpha + B_alpha - A_alpha * B_alpha
            : (A_alpha + B_alpha) / 2.0;
    }
    else if (mode == "Color")
    {
        out_rgb = blendmode_color(A, B);
        out_alpha = A_alpha + B_alpha - A_alpha * B_alpha;
    }
    else if (mode == "Color Burn")
    {
        out_rgb = blendmode_color_burn(A, B);

        if (alpha_masking)
        {
            out_alpha = A_alpha + B_alpha - A_alpha * B_alpha;
        }
        else
        {
            out_alpha = (A_alpha > 0.0)
                ? 1.0 - (1.0 - B_alpha) / A_alpha
                : 0.0;
        }
    }
    else if (mode == "Color Dodge")
    {
        out_rgb = blendmode_color_dodge(A, B);

        if (alpha_masking)
        {
            out_alpha = A_alpha + B_alpha - A_alpha * B_alpha;
        }
        else
        {
            out_alpha = (A_alpha != 1.0)
                ? B_alpha / (1.0 - A_alpha)
                : 0.0;
        }
    }
    else if (mode == "Conjoint Over")
    {
        out_rgb = blendmode_conjoint_over(A, A_alpha, B, B_alpha);
        out_alpha = A_alpha + B_alpha - A_alpha * B_alpha;
    }
    else if (mode == "Copy")
    {
        out_rgb = A;
        out_alpha = B_alpha;
    }
    else if (mode == "Difference")
    {
        out_rgb = abs(A - B);
        
        out_alpha = (alpha_masking)
            ? A_alpha + B_alpha - A_alpha * B_alpha
            : abs(A_alpha - B_alpha);
    }
    else if (mode == "Disjoint Over")
    {
        out_rgb = blendmode_disjoint_over(A, A_alpha, B, B_alpha);
        out_alpha = A_alpha + B_alpha - A_alpha * B_alpha;
    }
    else if (mode == "Divide")
    {
        out_rgb = (B != 0) ? A / B : color(0);
        
        if (alpha_masking)
        {
            out_alpha = A_alpha + B_alpha - A_alpha * B_alpha;
        }
        else
        {
            out_alpha = (B_alpha != 0) ? A_alpha / B_alpha : 0.0;
        }
    }
    else if (mode == "Exclusion")
    {
        out_rgb = blendmode_exclusion(A, B);

        out_alpha = (alpha_masking)
            ? A_alpha + B_alpha - A_alpha * B_alpha
            : A_alpha + B_alpha - 2.0 * A_alpha * B_alpha;
    }
    else if (mode == "Freeze")
    {
        out_rgb = blendmode_freeze(A, B);

        if (alpha_masking)
        {
            out_alpha = A_alpha + B_alpha - A_alpha * B_alpha;
        }
        else
        {
            out_alpha = (B_alpha > 0.0)
                ? 1.0 - sqrt(1.0 - A_alpha) / B_alpha
                : 0.0;
        }
    }
    else if (mode == "From")
    {
        out_rgb = B - A;

        out_alpha = (alpha_masking)
            ? A_alpha + B_alpha - A_alpha * B_alpha
            : B_alpha - A_alpha;
    }
    else if (mode == "Geometric")
    {
        out_rgb = (A * B != 0) ? 2.0 * A * B / (A + B) : color(0);

        if (alpha_masking)
        {
            out_alpha = A_alpha + B_alpha - A_alpha * B_alpha;
        }
        else
        {
            out_alpha = (A_alpha * B_alpha != 0)
                ? 2.0 * A_alpha * B_alpha / (A_alpha + B_alpha)
                : 0.0;
        }
    }
    else if (mode == "Grain Extract")
    {
        out_rgb = B - A + 0.5;

        out_alpha = (alpha_masking)
            ? A_alpha + B_alpha - A_alpha * B_alpha
            : B_alpha - A_alpha + 0.5;
    }
    else if (mode == "Grain Merge")
    {
        out_rgb = B + A - 0.5;

        out_alpha = (alpha_masking)
            ? A_alpha + B_alpha - A_alpha * B_alpha
            : B_alpha + A_alpha - 0.5;
    }
    else if (mode == "Hard Light")
    {
        out_rgb = blendmode_hard_light(A, B);

        if (alpha_masking)
        {
            out_alpha = A_alpha + B_alpha - A_alpha * B_alpha;
        }
        else
        {
            out_alpha = (A_alpha < 0.5)
                ? A_alpha * B_alpha
                : blend_screen(A_alpha, B_alpha);
        }
    }
    else if (mode == "Hue")
    {
        out_rgb = blendmode_hue(A, B);
        out_alpha = A_alpha + B_alpha - A_alpha * B_alpha;
    }
    else if (mode == "Hypot")
    {
        out_rgb = blendmode_hypot(A, B);

        out_alpha = (alpha_masking)
            ? A_alpha + B_alpha - A_alpha * B_alpha
            : hypot(A_alpha, B_alpha);
    }
    else if (mode == "In")
    {
        out_rgb = A * B_alpha;
        out_alpha = A_alpha * B_alpha;
    }
    else if (mode == "Linear Light")
    {
        out_rgb = blendmode_linear_light(A, B);

        out_alpha = (B_alpha > 0.5)
            ? max(0.0, A_alpha + (2.0 * B_alpha - 0.5))
            : A_alpha + 2.0 * B_alpha - 1.0;
    }
    else if (mode == "Luminosity")
    {
        out_rgb = blendmode_luminosity(A, B);
        out_alpha = A_alpha + B_alpha - A_alpha * B_alpha;
    }
    else if (mode == "Mask")
    {
        out_rgb = B * A_alpha;
        out_alpha = A_alpha * B_alpha;
    }
    else if (mode == "Matte")
    {
        out_rgb = mix(B, A, A_alpha);
        out_alpha = A_alpha + B_alpha - A_alpha * B_alpha;
    }
    else if (mode == "Max")
    {
        out_rgb = max(A, B);

        out_alpha = (alpha_masking)
            ? A_alpha + B_alpha - A_alpha * B_alpha
            : max(A_alpha, B_alpha);
    }
    else if (mode == "Min")
    {
        out_rgb = min(A, B);

        out_alpha = (alpha_masking)
            ? A_alpha + B_alpha - A_alpha * B_alpha
            : min(A_alpha, B_alpha);
    }
    else if (mode == "Minus")
    {
        out_rgb = A - B;

        out_alpha = (alpha_masking)
            ? A_alpha + B_alpha - A_alpha * B_alpha
            : A_alpha - B_alpha;
    }
    else if (mode == "Multiply")
    {
        out_rgb = (min(A) < 0.0 && min(B) < 0.0) ? A : A * B;

        out_alpha = (alpha_masking)
            ? A_alpha + B_alpha - A_alpha * B_alpha
            : A_alpha * B_alpha;
    }
    else if (mode == "Out")
    {
        out_rgb = A * (1.0 - B_alpha);
        out_alpha = A_alpha * (1.0 - B_alpha); // reversed?
    }
    else if (mode == "Over")
    {
        out_rgb = A + B * (1.0 - A_alpha);
        out_alpha = A_alpha + B_alpha - A_alpha * B_alpha;
    }
    else if (mode == "Overlay")
    {
        out_rgb = blendmode_overlay(A, B);

        if (alpha_masking)
        {
            out_alpha = A_alpha + B_alpha - A_alpha * A_alpha;
        }
        else
        {
            out_alpha = (B_alpha < 0.5)
                ? A_alpha * B_alpha
                : A_alpha + B_alpha - A_alpha * B_alpha;
        }
    }
    else if (mode == "Pin Light")
    {
        out_rgb = blendmode_pin_light(A, B);

        if (alpha_masking)
        {
            out_alpha = A_alpha + B_alpha - A_alpha * B_alpha;
        }
        else
        {
            out_alpha = (B_alpha > 0.5)
                ? max(A_alpha, 2.0 * (B_alpha - 0.5))
                : min(A_alpha, 2.0 * B_alpha);
        }
    }
    else if (mode == "Plus")
    {
        out_rgb = A + B;

        out_alpha = (alpha_masking)
            ? A_alpha + B_alpha - A_alpha * B_alpha
            : A_alpha + B_alpha;
    }
    else if (mode == "Reflect")
    {
        out_rgb = blendmode_reflect(A, B);

        if (alpha_masking)
        {
            out_alpha = A_alpha + B_alpha - A_alpha * B_alpha;
        }
        else
        {
            out_alpha = (B_alpha != 1.0)
                ? min(1.0, sqr(A_alpha) / (1.0 - B_alpha))
                : 1.0;
        }       
    }
    else if (mode == "Saturation")
    {
        out_rgb = blendmode_saturation(A, B);
        out_alpha = A_alpha + B_alpha - A_alpha * B_alpha;
    }
    else if (mode == "Screen")
    {
        out_rgb = blendmode_screen(A, B);
        out_alpha = A_alpha + B_alpha - A_alpha * B_alpha;
    }
    else if (mode == "Soft Light")
    {
        out_rgb = blendmode_soft_light(A, B);

        if (alpha_masking)
        {
            out_alpha = A_alpha + B_alpha - A_alpha * B_alpha;
        }
        else
        {
            out_alpha = (B_alpha < 0.5)
                ? (1.0 - 2.0 * B_alpha) * sqr(A_alpha) +
                         2.0 * A_alpha * B_alpha
                : (2.0 * B_alpha - 1.0) * sqrt(A_alpha) +
                         2.0 * A_alpha * (1.0 - B_alpha);
        }
    }
    else if (mode == "Stencil")
    {
        out_rgb = B * (1.0 - A_alpha);
        out_alpha = B_alpha * (1.0 - A_alpha); // reversed?
    }
    else if (mode == "Under")
    {
        out_rgb = A * (1.0 - B_alpha) + B;
        out_alpha = A_alpha * (1.0 - B_alpha) + B_alpha;
    }
    else if (mode == "Vivid Light")
    {
        out_rgb = blendmode_vivid_light(A, B);

        if (alpha_masking)
        {
            out_alpha = A_alpha + B_alpha - A_alpha * B_alpha;
        }
        else if (B_alpha > 0.5)
        {
            out_alpha = 1.0 - (1.0 - A_alpha) / (2.0 * (B_alpha - 0.5));
        }
        else if (B_alpha < 0.5)
        {
            out_alpha = A_alpha / (1.0 - 2.0 * B_alpha);
        }
        else
        {
            out_alpha = 0.0;
        }
    }
    else if (mode == "Xor")
    {
        out_rgb = A * (1.0 - B_alpha) + B * (1.0 - A_alpha);
        out_alpha = (1.0 - B_alpha) + (1.0 - A_alpha);   
    }
    else
    {
#ifdef DEBUG
        string shadername = "";
        getattribute("shader:shadername", shadername);
        warning("[DEBUG]: Invalid blend mode %d in %s, %s:%d\n",
                mode, shadername, __FILE__, __LINE__);
#endif
        out_rgb = color(0);
        out_alpha = 0.0;
    }
    return out_rgb;
}

#endif // !AS_COLOR_BLEND_MODES_H
