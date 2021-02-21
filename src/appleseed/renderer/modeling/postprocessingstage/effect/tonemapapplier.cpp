
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2020 Tiago Chaves, The appleseedhq Organization
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

// Interface header.
#include "tonemapapplier.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/colorspace.h"
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/math/scalar.h"

// Standard headers.
#include <cmath>

using namespace foundation;

namespace renderer
{

//
// ToneMapApplier class implementation.
//

void ToneMapApplier::release()
{
    delete this;
}

void ToneMapApplier::apply(
    Image&              image,
    const std::size_t   tile_x,
    const std::size_t   tile_y) const
{
    assert(tile_x < image.properties().m_tile_count_x);
    assert(tile_y < image.properties().m_tile_count_y);

    Tile& tile = image.tile(tile_x, tile_y);
    const std::size_t tile_width = tile.get_width();
    const std::size_t tile_height = tile.get_height();
    const Vector2u tile_offset(
        tile_x * image.properties().m_tile_width,
        tile_y * image.properties().m_tile_height);

    for (std::size_t y = 0; y < tile_height; ++y)
    {
        for (std::size_t x = 0; x < tile_width; ++x)
        {
            Color4f pixel;
            tile.get_pixel(x, y, pixel);
            pixel.unpremultiply_in_place();

            tone_map(pixel.rgb());

            pixel.premultiply_in_place();
            tile.set_pixel(x, y, pixel);
        }
    }
}

//
// LinearApplier class implementation.
//

LinearApplier::LinearApplier()
{
}

void LinearApplier::tone_map(Color3f& color) const
{
    // Do nothing.
}

//
// AcesNarkowiczApplier class implementation.
//

constexpr float AcesNarkowiczApplier::DefaultExposureBias;

AcesNarkowiczApplier::AcesNarkowiczApplier(
    const float     exposure_bias)
  : m_exposure_bias(exposure_bias)
{
}

void AcesNarkowiczApplier::tone_map(Color3f& color) const
{
    //
    // Apply Krzysztof Narkowicz's fitting of the ACES curve.
    //
    // Note that we add an exposure_bias factor to account for the following:
    //   "Curve was manually fitted (max fit error: 0.0138) to be more precise in the blacks [...]
    //    Additionally, data was pre-exposed, so 1 on input maps to ~0.8 on output and resulting
    //    image's brightness is more consistent with the one without any tone mapping curve at all.
    //    For the original ACES curve just multiply input (x) by 0.6."
    //
    // Reference:
    //
    //   https://knarkowicz.wordpress.com/2016/01/06/aces-filmic-tone-mapping-curve/
    //

    color *= m_exposure_bias;

    const Color3f a(2.51f);
    const Color3f b(0.03f);
    const Color3f c(2.43f);
    const Color3f d(0.59f);
    const Color3f e(0.14f);
    color =
        (color * (a * color + b)) /
        (color * (c * color + d) + e);
}

//
// AcesUnrealApplier class implementation.
//

AcesUnrealApplier::AcesUnrealApplier()
{
}

void AcesUnrealApplier::tone_map(Color3f& color) const
{
    //
    // Apply the color grading curve from Unreal 3,
    // adapted by Romain Guy to be close to the ACES curve.
    //
    // Note: gamma 2.2 correction is baked in.
    //
    // References:
    //
    //   https://www.shadertoy.com/view/llXyWr
    //   https://twitter.com/romainguy/status/707008672434507776
    //

    color = color / (color + Color3f(0.155f)) * 1.019f;

    // Decode 2.2 gamma correction.
    color = Color3f(
        std::pow(color.r, 2.2f),
        std::pow(color.g, 2.2f),
        std::pow(color.b, 2.2f));
}

//
// FilmicHejlApplier class implementation.
//

FilmicHejlApplier::FilmicHejlApplier()
{
}

void FilmicHejlApplier::tone_map(Color3f& color) const
{
    //
    // Apply a filmic tone mapper developed by Jim Hejl and Richard Burgess-Dawson at EA.
    //
    // Note: gamma 2.2 correction is baked in.
    //
    // References:
    //
    //   http://filmicworlds.com/blog/filmic-tonemapping-operators/
    //   https://de.slideshare.net/hpduiker/filmic-tonemapping-for-realtime-rendering-siggraph-2010-color-course
    //

    color = component_wise_max(Color3f(0.0f), color - Color3f(0.004f));
    color =
        (color * (6.2f * color + Color3f(0.5f))) /
        (color * (6.2f * color + Color3f(1.7f)) + Color3f(0.06f));

    // Decode 2.2 gamma correction.
    color = Color3f(
        std::pow(color.r, 2.2f),
        std::pow(color.g, 2.2f),
        std::pow(color.b, 2.2f));
}

//
// FilmicPiecewiseApplier class implementation.
//

constexpr float FilmicPiecewiseApplier::DefaultToeStrength;
constexpr float FilmicPiecewiseApplier::DefaultToeLength;
constexpr float FilmicPiecewiseApplier::DefaultShoulderStrength;
constexpr float FilmicPiecewiseApplier::DefaultShoulderLength;
constexpr float FilmicPiecewiseApplier::DefaultShoulderAngle;

FilmicPiecewiseApplier::FilmicPiecewiseApplier(
    const float     toe_strength,
    const float     toe_length,
    const float     shoulder_strength,
    const float     shoulder_length,
    const float     shoulder_angle)
  : m_x0(0.5f * std::pow(toe_length, 2.2f))
  , m_y0(m_x0 * (1.0f - toe_strength))
  , m_x1(m_x0 + (1.0f - shoulder_length) * (1.0f - m_y0))
  , m_y1(m_y0 + (1.0f - shoulder_length) * (1.0f - m_y0))
  , m_W(m_x0 - m_y0 + std::exp2(shoulder_strength))
  , m_rcp_W(safe_rcp(m_W, default_eps<float>()))
  , m_overshoot_x(2.0f * m_W * shoulder_angle * shoulder_strength)
  , m_overshoot_y(0.5f * shoulder_angle * shoulder_strength)
  , m_segments()
{
    //
    // Initialize the parameters that describe the power curves that (piecewisely)
    // define the overall tone mapping curve developed by John Hable.
    //
    // Note: since we don't apply gamma correction, many of the original equations
    // from "Piecewise Power Curves" were simplified, as we assume gamma equal 1.0.
    //

    // Normalize params to 1.0 range (note that we store the unnormalized x values,
    // which changes how we choose the correct segment in PiecewiseApplier::eval_at).
    const float x0 = m_x0 * m_rcp_W;
    const float x1 = m_x1 * m_rcp_W;
    const float overshoot_x = m_overshoot_x * m_rcp_W;

    // Convert to slope–intercept form (y = mx + b).
    const float m = x1 == x0 ? 1.0f : (m_y1 - m_y0) / (x1 - x0);
    const float b = m_y0 - m * x0;

    // Linear (middle) section
    {
        //
        // The base function of the linear (mid) section is y = mx + b, which we can rewrite as:
        //    y = exp(ln(m) + ln(x + b/m))
        //
        // and our evaluation function is f(x) = y0 * scale_y + offset_y, where:
        //    x0 = (x - offset_x) * scale_x
        //    y0 = exp(lnA + B * log(x0))
        //

        //
        // Thus, we have:
        //    x0 = (x - (-b/m)) * 1.0f ,            offset_x = -b/m, scale_x = 1.0f
        //       = x + b/m
        //
        //    y0 = exp(log(m) + 1.0f * log(x0)) ,   lnA = log(m), B = 1.0f
        //       = exp(log(m) + log(x0))
        //
        //    f(x) = y0 * 1.0f + 0.0f ,             offset_y = 0.0f, scale_y = 1.0f
        //         = y0
        //         = exp(log(m) + log(x + b/m))
        //

        const float B = 1.0f;
        const float lnA = std::log(m);
        m_segments[Segment::LINEAR] = { -b/m, 0.0f, 1.0f, 1.0f, lnA, B };
    }

    // Toe (start) section
    {
        //
        // The following equations find values for A and B such that:
        //    f(x) = exp(lnA + Bln(x))
        //
        // with:
        //    f(0)   = 0; not really a constraint (handled in PowerCurve::eval)
        //    f(x0)  = y0
        //    f'(x0) = m
        //

        const float B = (m * x0) / m_y0;
        const float lnA = std::log(m_y0) - B * std::log(x0);
        m_segments[Segment::TOE] = { 0.0f, 0.0f, 1.0f, 1.0f, lnA, B };
    }

    // Shoulder (end) section
    {
        //
        // For the shoulder, we can do the same thing as the toe, except flip it horizontally and vertically.
        //
        // However, instead of having the curve end at (W, 1.0), it will end at (W, 1.0) + (overshoot_x, overshoot_y).
        // By changing how far we overshoot, it will increase the angle and give us more range at the end of our shoulder.
        // Thus, we apply a 1/W scale to the overall function at the end to make sure we hit 1.0 at our chosen white point.
        //

        const float offset_x = 1.0f + overshoot_x;
        const float offset_y = 1.0f + m_overshoot_y;

        const float x0 = offset_x - x1;
        const float y0 = offset_y - m_y1;

        const float B = (m * x0) / y0;
        const float lnA = std::log(y0) - B * std::log(x0);
        m_segments[Segment::SHOULDER] = { offset_x, offset_y, -1.0f, -1.0f, lnA, B };
    }

    // Normalize y values so that we hit 1.0 at our white point.
    const float rcp_white_scale = 1.0f / m_segments[Segment::SHOULDER].eval(1.0f);

    m_segments[Segment::TOE].scale_y *= rcp_white_scale;
    m_segments[Segment::TOE].offset_y *= rcp_white_scale;
    m_segments[Segment::LINEAR].scale_y *= rcp_white_scale;
    m_segments[Segment::LINEAR].offset_y *= rcp_white_scale;
    m_segments[Segment::SHOULDER].scale_y *= rcp_white_scale;
    m_segments[Segment::SHOULDER].offset_y *= rcp_white_scale;
}

inline float FilmicPiecewiseApplier::PowerCurve::eval(const float x) const
{
    const float x0 = scale_x * (x - offset_x);

    if (x0 > 0.0f)
        return offset_y + scale_y * std::exp(lnA + B * std::log(x0));
    else
        return offset_y;
}

float FilmicPiecewiseApplier::eval_at(const float x) const
{
    const float norm_x = x * m_rcp_W;

    //
    // Note: since we don't store normalized values for the x coordinates
    // (i.e. we are not dividing them by m_W in the constructor), we shouldn't
    // use norm_x in the comparisons with m_x0 and m_x1, as in the original code.
    //

    if (x < m_x0)
        return m_segments[Segment::TOE].eval(norm_x);
    else if (x < m_x1)
        return m_segments[Segment::LINEAR].eval(norm_x);
    else
        return m_segments[Segment::SHOULDER].eval(norm_x);
}

void FilmicPiecewiseApplier::tone_map(Color3f& color) const
{
    //
    // Apply John Hable's filmic tone mapper, described by three power curve segments.
    //
    // Reference:
    //
    //   http://filmicworlds.com/blog/filmic-tonemapping-with-piecewise-power-curves/
    //   https://github.com/johnhable/fw-public
    //

    color = Color3f(
        eval_at(color.r),
        eval_at(color.g),
        eval_at(color.b));
}

//
// FilmicUnchartedApplier class implementation.
//

constexpr float FilmicUnchartedApplier::DefaultA;
constexpr float FilmicUnchartedApplier::DefaultB;
constexpr float FilmicUnchartedApplier::DefaultC;
constexpr float FilmicUnchartedApplier::DefaultD;
constexpr float FilmicUnchartedApplier::DefaultE;
constexpr float FilmicUnchartedApplier::DefaultF;
constexpr float FilmicUnchartedApplier::DefaultW;
constexpr float FilmicUnchartedApplier::DefaultExposureBias;

FilmicUnchartedApplier::FilmicUnchartedApplier(
    const float     A,
    const float     B,
    const float     C,
    const float     D,
    const float     E,
    const float     F,
    const float     W,
    const float     exposure_bias)
  : m_A(A)
  , m_B(B)
  , m_C(C)
  , m_D(D)
  , m_E(E)
  , m_F(F)
  , m_W(W)
  , m_exposure_bias(exposure_bias)
{
}

inline float FilmicUnchartedApplier::uncharted_tone_map(const float x) const
{
    return ((x * (m_A * x + m_C * m_B) + m_D * m_E) /
            (x * (m_A * x + m_B) + m_D * m_F)) - m_E / m_F;
}

inline Color3f FilmicUnchartedApplier::uncharted_tone_map(const Color3f& x) const
{
    return ((x * (m_A * x + Color3f(m_C * m_B)) + Color3f(m_D * m_E)) /
            (x * (m_A * x + Color3f(m_B)) + Color3f(m_D * m_F))) - Color3f(m_E / m_F);
}

void FilmicUnchartedApplier::tone_map(Color3f& color) const
{
    //
    // Apply a filmic tone mapper developed by John Hable for Uncharted 2.
    // ((x * (A * x + C * B) + D * E) / (x * (A * x + B) + D * F)) - E / F
    //
    // References:
    //
    //   http://filmicworlds.com/blog/filmic-tonemapping-operators/
    //   http://filmicworlds.com/blog/filmic-tonemapping-with-piecewise-power-curves/
    //   https://www.slideshare.net/ozlael/hable-john-uncharted2-hdr-lighting (see slides 141-142)
    //

    color *= m_exposure_bias;

    color = uncharted_tone_map(color);

    const Color3f white_scale(1.0f / uncharted_tone_map(m_W));
    color *= white_scale;
}

//
// ReinhardApplier class implementation.
//

constexpr bool ReinhardApplier::DefaultUseLuminance;

ReinhardApplier::ReinhardApplier(const bool use_luminance)
  : m_use_luminance(use_luminance)
{
}

void ReinhardApplier::tone_map(Color3f& color) const
{
    //
    // Apply Reinhard's simple tone mapping operator (Equation 3).
    //
    // Reference:
    //
    //   http://www.cmap.polytechnique.fr/~peyre/cours/x2005signal/hdr_photographic.pdf
    //

    if (m_use_luminance)
    {
        color /= 1.0f + luminance(color);

        //
        // Note: this is equivalent to the following:
        //
        // const float L = luminance(color);     // world luminance
        // const float Ld = L / (1.0f + L);      // display luminance
        // color *= Ld / L;
        //
    }
    else
    {
        color /= Color3f(1.0f) + color;
    }
}

//
// ReinhardExtendedApplier class implementation.
//

constexpr float ReinhardExtendedApplier::DefaultMaxWhite;
constexpr bool ReinhardExtendedApplier::DefaultUseLuminance;

ReinhardExtendedApplier::ReinhardExtendedApplier(
    const float max_white,
    const bool use_luminance)
  : m_max_white(max_white)
  , m_use_luminance(use_luminance)
{
}

void ReinhardExtendedApplier::tone_map(Color3f& color) const
{
    //
    // Apply Reinhard's extended tone mapping operator (Equation 4).
    //
    // Note that we use Lwhite = Lmax to avoid burn-out, where:
    //   * Lwhite is the smallest luminance that will be mapped to pure white
    //   * Lmax is the maximum luminance in the scene
    //
    // As mentioned in the paper (on Section 3.1, page 3):
    //   "This function is a blend between Equation 3 and a linear mapping [...]
    //    If Lwhite value is set to the maximum luminance in the scene Lmax or higher,
    //    no burn-out will occur. If it is set to infinity, then the function reverts to Equation 3.
    //    By default we set Lwhite to the maximum luminance in the scene. If this default is applied
    //    to scenes that have a low dynamic range (i.e., Lmax < 1), the effect is a subtle contrast enhancement"
    //
    // Reference:
    //
    //   http://www.cmap.polytechnique.fr/~peyre/cours/x2005signal/hdr_photographic.pdf
    //

    if (m_use_luminance)
    {
        const float L = luminance(color);
        const float Ld = (L * (1.0f + L / (m_max_white * m_max_white))) / (1.0f + L);
        color *= Ld / L;
    }
    else
    {
        color = (color * (Color3f(1.0f) + color / (m_max_white * m_max_white))) /
                (Color3f(1.0f) + color);
    }
}

}   // namespace renderer
