
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

//
// References:
//
//      "Compositing Digital Images", by T.Porter and T.Duff
//      Computer Graphics Volume 18, Number 3, July 1984, pp 253-259
//
//      https://keithp.com/~keithp/porterduff/
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

//
// Reference:
//
//      https://en.wikipedia.org/wiki/Blend_modes
//      http://natron.readthedocs.io/en/master/plugins/net.sf.openfx.MergePlugin.html
//

color color_blend_modes(int mode, color A, color B)
{
    color rgb;

    if (mode == 0)
    {
        rgb = min(A, B);
    }
    if (mode == 1)
    {
        rgb = A * B;
    }
    else if (mode == 2)
    {
        rgb = blendmode_color_burn(A, B);
    }
    else if (mode == 3)
    {
        rgb = A + B - color(1);
    }
    else if (mode == 4)
    {
        rgb = max(A, B);
    }
    else if (mode == 5)
    {
        rgb = blendmode_screen(A, B);
    }
    else if (mode == 6)
    {
        rgb = blendmode_color_dodge(A, B);
    }
    else if (mode == 7)
    {
        rgb = A + B;
    }
    else if (mode == 8)
    {
        rgb = blendmode_overlay(A, B);
    }
    else if (mode == 9)
    {
        rgb = blendmode_soft_light(A, B);
    }
    else if (mode == 10)
    {
        rgb = blendmode_hard_light(A, B);
    }
    else if (mode == 11)
    {
        rgb = blendmode_vivid_light(A, B);
    }
    else if (mode == 12)
    {
        rgb = blendmode_linear_light(A, B);
    }
    else if (mode == 13)
    {
        rgb = blendmode_pin_light(A, B);
    }
    else if (mode == 14)
    {
        rgb = abs(A - B);
    }
    else if (mode == 15)
    {
        rgb = blendmode_exclusion(A, B);
    }
    else if (mode == 16)
    {
        rgb = A - B;
    }
    else if (mode == 17)
    {
        for (int i = 0; i < NCOMPS; ++i)
        {
            rgb[i] = (B[i] != 0.0) ? A[i] / B[i] : 0.0;
        }
    }
    else if (mode == 18)
    {
        color A_HSV = transform_RGB_to_HSV(A);
        color B_HSV = transform_RGB_to_HSV(B);
        rgb = transform_HSV_to_RGB(A_HSV[0], B_HSV[1], B_HSV[2]);
    }
    else if (mode == 19)
    {
        color A_HSV = transform_RGB_to_HSV(A);
        color B_HSV = transform_RGB_to_HSV(B);
        rgb = transform_HSV_to_RGB(B[0], A[1], B[2]);
    }
    else if (mode == 20)
    {
        color A_HSL = transform_RGB_to_HSL(A);
        color B_HSL = transform_RGB_to_HSL(B);
        rgb = transform_HSL_to_RGB(A[0], A[1], B[2]);
    }
    else if (mode == 21)
    {
        color A_HSL = transform_RGB_to_HSL(A);
        color B_HSL = transform_RGB_to_HSL(B);
        rgb = transform_HSL_to_RGB(B[0], B[1], A[2]);
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
// Main blend function blend modes:
//
//      0   Atop
//      1   Average
//      2   Color Burn
//      3   Color Dodge
//      4   Conjoint Over
//      5   Difference
//      6   Disjoin Over
//      7   Divide
//      8   Exclusion
//      9   From
//      10  Geometric
//      11  Hard Light
//      12  Hypot
//      13  In
//      14  Linear Light
//      15  Mask
//      16  Matte
//      17  Max
//      18  Min
//      19  Minus
//      20  Multiply
//      21  Out
//      22  Over
//      23  Overlay
//      24  Pin Light
//      25  Plus
//      26  Reflect
//      27  Screen
//      28  Soft Light
//      29  Stencil
//      30  Under
//      31  Vivid Light
//      32  Xor
//

color blend_color(
    color A,
    float A_alpha,
    color B,
    float B_alpha,
    int mode,
    int clamp_output,
    output float out_alpha)
{
    color out_rgb = color(0);
    out_alpha = B_alpha;

    if (mode == 0)
    {
        out_rgb = A * B_alpha + B * (1.0 - A_alpha);
        out_alpha = A_alpha + B_alpha;
    }
    else if (mode == 1)
    {
        out_rgb = (A + B) / 2.0;
        out_alpha = (A_alpha + B_alpha) / 2.0;
    }
    else if (mode == 2)
    {
        out_rgb = blendmode_color_burn(A, B);
        out_alpha = (A_alpha > 0.0) ? 1.0 - (1.0 - B_alpha) / A_alpha : 0.0;
    }
    else if (mode == 3)
    {
        out_rgb = blendmode_color_dodge(A, B);
        out_alpha = (A_alpha != 1.0) ? B_alpha / (1.0 - A_alpha) : 0.0;
    }
    else if (mode == 4)
    {
        out_rgb = blendmode_conjoint_over(A, A_alpha, B, B_alpha);
    }
    else if (mode == 5)
    {
        out_rgb = abs(A - B);
        out_alpha = abs(A_alpha - B_alpha);
    }
    else if (mode == 6)
    {
        out_rgb = blendmode_disjoint_over(A, A_alpha, B, B_alpha);
    }
    else if (mode == 7)
    {
        out_rgb = (B != 0) ? A / B : color(0);
        out_alpha = (B_alpha != 0) ? A_alpha / B_alpha : 0.0;
    }
    else if (mode == 8)
    {
        out_rgb = blendmode_exclusion(A, B);
        out_alpha = A_alpha + B_alpha - 2.0 * A_alpha * B_alpha;
    }
    else if (mode == 9)
    {
        out_rgb = B - A;
        out_alpha = B_alpha - A_alpha;
    }
    else if (mode == 10)
    {
        out_rgb = (A * B != 0) ? 2.0 * A * B / (A + B) : color(0);
        out_alpha = (A_alpha * B_alpha != 0)
            ? 2.0 * A_alpha * B_alpha / (A_alpha + B_alpha)
            : 0.0;
    }
    else if (mode == 11)
    {
        out_rgb = blendmode_hard_light(A, B);
        out_alpha = (A_alpha < 0.5)
            ? A_alpha * B_alpha
            : blend_screen(A_alpha, B_alpha);
    }
    else if (mode == 12)
    {
        out_rgb = blendmode_hypot(A, B);
        out_alpha = hypot(A_alpha, B_alpha);
    }
    else if (mode == 13)
    {
        out_rgb = A * B_alpha;
        out_alpha = B_alpha; // or A_alpha * B_alpha?
    }
    else if (mode == 14)
    {
        out_rgb = blendmode_linear_light(A, B);
        // alpha is what?
    }
    else if (mode == 15)
    {
        out_rgb = B * A_alpha;
        out_alpha = A_alpha; // or A_alpha * B_alpha*
    }
    else if (mode == 16)
    {
        out_rgb = mix(B, A, A_alpha);
        // alpha is what?
    }
    else if (mode == 17)
    {
        out_rgb = max(A, B);
        out_alpha = max(A_alpha, B_alpha);
    }
    else if (mode == 18)
    {
        out_rgb = min(A, B);
        out_alpha = min(A_alpha, B_alpha);
    }
    else if (mode == 19)
    {
        out_rgb = A - B;
        out_alpha = A_alpha - B_alpha;
    }
    else if (mode == 20)
    {
        out_rgb = (min(A) < 0.0 && min(B) < 0.0) ? A : A * B;
        out_alpha = A_alpha * B_alpha;
    }
    else if (mode == 21)
    {
        out_rgb = A * (1.0 - B_alpha);
        // alpha is what? A.a * (1-B.a)? Or just B.a?
    }
    else if (mode == 22)
    {
        out_rgb = A + B * (1.0 - A_alpha);
        // alpha is what? A.a + B.a(1 - B.a) ?
    }
    else if (mode == 23)
    {
        out_rgb = blendmode_overlay(A, B);
        out_alpha = (B_alpha < 0.5)
            ? A_alpha * B_alpha
            : A_alpha + B_alpha - A_alpha * B_alpha;
    }
    else if (mode == 24)
    {
        out_rgb = blendmode_pin_light(A, B);
        // alpha is what?
    }
    else if (mode == 25)
    {
        out_rgb = A + B;
        out_alpha = A_alpha + B_alpha;
    }
    else if (mode == 26)
    {
        out_rgb = blendmode_reflect(A, B);
        // alpha is what?
    }
    else if (mode == 27)
    {
        out_rgb = blendmode_screen(A, B);
        out_alpha = A_alpha + B_alpha - A_alpha * B_alpha;
    }
    else if (mode == 28)
    {
        out_rgb = blendmode_soft_light(A, B);
        out_alpha = (1.0 - 2.0 * B_alpha) *
            sqr(A_alpha) + 2.0 * B_alpha + A_alpha;
    }
    else if (mode == 29)
    {
        out_rgb = B * (1.0 - A_alpha);
        // alpha is what? B.a(1 - A.a) ? Or just A.a?
    }
    else if (mode == 30)
    {
        out_rgb = A * (1.0 - B_alpha) + B;
        // alpha is what? A.a(1 - B.a)?
    }
    else if (mode == 31)
    {
        out_rgb = blendmode_vivid_light(A, B);
        // alpha is what?
    }
    else if (mode == 32)
    {
        out_rgb = A * (1.0 - B_alpha) + B * (1.0 - A_alpha);
        // alpha is what? (1-B.a) + (1-A.a) ?
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

    out_alpha = clamp(out_alpha, 0.0, 1.0);

    return (clamp_output) ? clamp(out_rgb, color(0), color(1)) : out_rgb;
}

#endif // !AS_COLOR_BLEND_MODES_H
