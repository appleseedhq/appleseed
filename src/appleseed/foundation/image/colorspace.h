
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz
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

#ifndef APPLESEED_FOUNDATION_IMAGE_COLORSPACE_H
#define APPLESEED_FOUNDATION_IMAGE_COLORSPACE_H

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/spectrum.h"
#include "foundation/math/fastmath.h"
#include "foundation/math/scalar.h"
#include "foundation/math/spline.h"
#include "foundation/platform/compiler.h"
#ifdef APPLESEED_FOUNDATION_USE_SSE
#include "foundation/platform/sse.h"
#endif
#include "foundation/utility/otherwise.h"

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstddef>

namespace foundation
{

//
// HSV <-> linear RGB transformations.
//
// Reference:
//
//   http://en.wikipedia.org/wiki/HLS_color_space
//
// Note:
//
//   If hsv is a three-component color expressed in the HSV color space, then:
//
//     hsv[0] is the Hue in [0, 360)
//     hsv[1] is the Saturation in [0, 1]
//     hsv[2] is the Value in [0, 1]
//

// Convert a color from the HSV color space to the linear RGB color space.
template <typename T>
Color<T, 3> hsv_to_linear_rgb(const Color<T, 3>& hsv);

// Convert a color from the linear RGB color space to the HSV color space.
template <typename T>
Color<T, 3> linear_rgb_to_hsv(const Color<T, 3>& linear_rgb);


//
// HSL <-> linear RGB transformations.
//
// Reference:
//
//   http://en.wikipedia.org/wiki/HLS_color_space
//
// Note:
//
//   If hsl is a three-component color expressed in the HSL color space, then:
//
//     hsl[0] is the Hue in [0, 360)
//     hsl[1] is the Saturation in [0, 1]
//     hsl[2] is the Lightness in [0, 1]
//

// Convert a color from the HSL color space to the linear RGB color space.
template <typename T>
Color<T, 3> hsl_to_linear_rgb(const Color<T, 3>& hsl);

// Convert a color from the linear RGB color space to the HSL color space.
template <typename T>
Color<T, 3> linear_rgb_to_hsl(const Color<T, 3>& linear_rgb);


//
// CIE XYZ <-> linear RGB transformations.
//
// References:
//
//   http://en.wikipedia.org/wiki/SRGB_color_space
//   http://www.poynton.com/notes/colour_and_gamma/ColorFAQ.txt
//

// Convert a color from the CIE XYZ color space to the linear RGB color space.
template <typename T>
Color<T, 3> ciexyz_to_linear_rgb(const Color<T, 3>& xyz);

// Convert a color from the linear RGB color space to the CIE XYZ color space.
template <typename T>
Color<T, 3> linear_rgb_to_ciexyz(const Color<T, 3>& linear_rgb);


//
// Linear RGB <-> sRGB transformations.
//
// Reference:
//
//   http://en.wikipedia.org/wiki/SRGB_color_space
//

// Convert a color component from the linear RGB color space to the sRGB color space.
template <typename T>
T linear_rgb_to_srgb(const T c);

// Convert a color component from the sRGB color space to the linear RGB color space.
template <typename T>
T srgb_to_linear_rgb(const T c);

// Convert a color from the linear RGB color space to the sRGB color space.
template <typename T>
Color<T, 3> linear_rgb_to_srgb(const Color<T, 3>& linear_rgb);

// Convert a color from the sRGB color space to the linear RGB color space.
template <typename T>
Color<T, 3> srgb_to_linear_rgb(const Color<T, 3>& srgb);

// Variants of the above functions using a fast approximation of the power function.
float fast_linear_rgb_to_srgb(const float c);
float fast_srgb_to_linear_rgb(const float c);
Color3f fast_linear_rgb_to_srgb(const Color3f& linear_rgb);
Color3f fast_srgb_to_linear_rgb(const Color3f& srgb);


//
// Enumeration of all supported color spaces.
//

enum ColorSpace
{
    ColorSpaceLinearRGB = 0,    // linear RGB
    ColorSpaceSRGB,             // sRGB
    ColorSpaceCIEXYZ,           // CIE XYZ
    ColorSpaceSpectral          // spectral
};


//
// Transform a 3-component color from one color space to another.
//

template <typename T>
Color<T, 3> transform_color(
    const Color<T, 3>&          color,
    const ColorSpace            from,
    const ColorSpace            to);


//
// Compute the relative luminance of a linear RGB triplet as defined
// in the ITU-R Recommendation BT.709 (Rec. 709):
//
//   Y = 0.212671 R + 0.715160 G + 0.072169 B
//
// This is equivalent to transforming the linear RGB triplet to the
// CIE XYZ color space and keeping the Y component.
//
// References:
//
//   http://en.wikipedia.org/wiki/Rec._709
//   http://en.wikipedia.org/wiki/Luminance_(relative)
//

template <typename T>
T luminance(const Color<T, 3>& linear_rgb);


//
// Standard illuminants.
//
// References:
//
//   http://en.wikipedia.org/wiki/Standard_illuminant
//   http://cvrl.ioo.ucl.ac.uk/
//

// Daylight illuminants.
extern const Spectrum31f IlluminantCIED65;                  // CIE D65

// Incandescent lighting illuminants.
extern const Spectrum31f IlluminantCIEA;                    // CIE A (black body radiator at 2856 K)


//
// Color Matching Functions (CMF).
//
// References:
//
//   http://en.wikipedia.org/wiki/CIE_1931_color_space
//   http://cvrl.ioo.ucl.ac.uk/
//

// XYZ color matching functions.
extern const Spectrum31f XYZCMFCIE19312Deg[3];              // CIE 1931 2-deg
extern const Spectrum31f XYZCMFCIE1931Judd2Deg[3];          // CIE 1931 2-deg, modified by Judd (1951)
extern const Spectrum31f XYZCMFCIE1931JuddVos2Deg[3];       // CIE 1931 2-deg, modified by Judd (1951) and Vos (1978)
extern const Spectrum31f XYZCMFCIE196410Deg[3];             // CIE 1964 10-deg (recommended)

// RGB color matching functions.
extern const Spectrum31f RGBCMFStilesBurch19552Def[3];      // Stiles and Burch (1955) 2-deg
extern const Spectrum31f RGBCMFStilesBurch195910Def[3];     // Stiles and Burch (1959) 10-deg (recommended)


//
// Lighting conditions, defined as a set of color matching functions and an illuminant.
//

class LightingConditions
{
  public:
    Color3f                     m_cmf[31];                  // precomputed values of (cmf[0], cmf[1], cmf[2]) * illuminant

    LightingConditions(
        const Spectrum31f&      illuminant,                 // illuminant
        const Spectrum31f       cmf[3]);                    // color matching functions
};


//
// Spectrum <-> CIE XYZ transformations.
//

// Convert a spectrum to a color in the CIE XYZ color space.
template <typename T, typename Spectrum>
Color<T, 3> spectrum_to_ciexyz(
    const LightingConditions&   lighting,
    const Spectrum&             spectrum);

// Convert a color in the CIE XYZ color space to a spectrum.
template <typename T, typename Spectrum>
void ciexyz_to_spectrum(
    const LightingConditions&   lighting,
    const Color<T, 3>&          xyz,
    Spectrum&                   spectrum);


//
// Linear RGB to spectrum transformation.
//
// The spectrum to linear RGB transformation can be obtained by first
// converting the spectrum to the CIE XYZ color space, then converting
// the resulting CIE XYZ color to the linear RGB color space.
//

// Convert a color in the linear RGB color space to a spectrum.
template <typename T, typename Spectrum>
void linear_rgb_to_spectrum(
    const LightingConditions&   lighting,
    const Color<T, 3>&          linear_rgb,
    Spectrum&                   spectrum);


//
// Spectrum <-> Spectrum transformation.
//

// Convert a spectrum defined over a given set of wavelengths
// to a spectrum defined over a different set of wavelengths.
template <typename T>
void spectrum_to_spectrum(
    const size_t                input_count,
    const T                     input_wavelength[],
    const T                     input_spectrum[],
    const size_t                output_count,
    const T                     output_wavelength[],
    T                           output_spectrum[],
    T                           working_storage[] = 0);


//
// HSV <-> linear RGB transformations implementation.
//

template <typename T>
inline Color<T, 3> hsv_to_linear_rgb(const Color<T, 3>& hsv)
{
    // Compute chroma.
    const T c = hsv[1] * hsv[2];

    // Compute value.
    Color<T, 3> linear_rgb(hsv[2] - c);

    // Compute RGB color.
    const T hprime = hsv[0] * T(1.0 / 60.0);
    const T x = c * (T(1.0) - std::abs(mod(hprime, T(2.0)) - T(1.0)));
    switch (truncate<int>(hprime))
    {
      case 0: linear_rgb[0] += c; linear_rgb[1] += x; break;
      case 1: linear_rgb[0] += x; linear_rgb[1] += c; break;
      case 2: linear_rgb[1] += c; linear_rgb[2] += x; break;
      case 3: linear_rgb[1] += x; linear_rgb[2] += c; break;
      case 4: linear_rgb[0] += x; linear_rgb[2] += c; break;
      case 5: linear_rgb[0] += c; linear_rgb[2] += x; break;
      assert_otherwise;
    }

    return linear_rgb;
}

template <typename T>
inline Color<T, 3> linear_rgb_to_hsv(const Color<T, 3>& linear_rgb)
{
    // Compute chroma.
    const T max_val = max_value(linear_rgb);
    const T min_val = min_value(linear_rgb);
    const T c = max_val - min_val;
    
    // Special case for zero chroma.
    if (c == T(0.0))
        return Color<T, 3>(0.0);

    // Compute hue.
    const T hprime =
        max_val == linear_rgb[0] ? mod((linear_rgb[1] - linear_rgb[2]) / c, T(6.0)) :
        max_val == linear_rgb[1] ? (linear_rgb[2] - linear_rgb[0]) / c + T(2.0) :
                                   (linear_rgb[0] - linear_rgb[1]) / c + T(4.0);
    const T hue = hprime * T(60.0);

    // Compute saturation and value.
    const T saturation = c / max_val;
    const T value = max_val;

    return Color<T, 3>(hue, saturation, value);
}


//
// HSL <-> linear RGB transformations implementation.
//

template <typename T>
inline Color<T, 3> hsl_to_linear_rgb(const Color<T, 3>& hsl)
{
    // Compute chroma.
    const T c = hsl[1] * (T(1.0) - std::abs(hsl[2] + hsl[2] - T(1.0)));

    // Compute lightness.
    Color<T, 3> linear_rgb(hsl[2] - T(0.5) * c);

    // Compute RGB color.
    const T hprime = hsl[0] * T(1.0 / 60.0);
    const T x = c * (T(1.0) - std::abs(mod(hprime, T(2.0)) - T(1.0)));
    switch (truncate<int>(hprime))
    {
      case 0: linear_rgb[0] += c; linear_rgb[1] += x; break;
      case 1: linear_rgb[0] += x; linear_rgb[1] += c; break;
      case 2: linear_rgb[1] += c; linear_rgb[2] += x; break;
      case 3: linear_rgb[1] += x; linear_rgb[2] += c; break;
      case 4: linear_rgb[0] += x; linear_rgb[2] += c; break;
      case 5: linear_rgb[0] += c; linear_rgb[2] += x; break;
      assert_otherwise;
    }

    return linear_rgb;
}

template <typename T>
inline Color<T, 3> linear_rgb_to_hsl(const Color<T, 3>& linear_rgb)
{
    // Compute chroma.
    const T max_val = max_value(linear_rgb);
    const T min_val = min_value(linear_rgb);
    const T c = max_val - min_val;
    
    // Special case for zero chroma.
    if (c == T(0.0))
        return Color<T, 3>(0.0);

    // Compute hue.
    const T hprime =
        max_val == linear_rgb[0] ? mod((linear_rgb[1] - linear_rgb[2]) / c, T(6.0)) :
        max_val == linear_rgb[1] ? (linear_rgb[2] - linear_rgb[0]) / c + T(2.0) :
                                   (linear_rgb[0] - linear_rgb[1]) / c + T(4.0);
    const T hue = hprime * T(60.0);

    // Compute lightness and saturation.
    const T lightness = T(0.5) * (min_val + max_val);
    const T saturation = c / (T(1.0) - std::abs(lightness + lightness - T(1.0)));

    return Color<T, 3>(hue, saturation, lightness);
}


//
// CIE XYZ <-> linear RGB transformations implementation.
//

template <typename T>
inline Color<T, 3> ciexyz_to_linear_rgb(const Color<T, 3>& xyz)
{
    return Color<T, 3>(
        T( 3.240479) * xyz[0] + T(-1.537150) * xyz[1] + T(-0.498535) * xyz[2],
        T(-0.969256) * xyz[0] + T( 1.875991) * xyz[1] + T( 0.041556) * xyz[2],
        T( 0.055648) * xyz[0] + T(-0.204043) * xyz[1] + T( 1.057311) * xyz[2]);
}

template <typename T>
inline Color<T, 3> linear_rgb_to_ciexyz(const Color<T, 3>& linear_rgb)
{
    return Color<T, 3>(
        T(0.412453) * linear_rgb[0] + T(0.357580) * linear_rgb[1] + T(0.180423) * linear_rgb[2],
        T(0.212671) * linear_rgb[0] + T(0.715160) * linear_rgb[1] + T(0.072169) * linear_rgb[2],
        T(0.019334) * linear_rgb[0] + T(0.119193) * linear_rgb[1] + T(0.950227) * linear_rgb[2]);
}


//
// Linear RGB <-> sRGB transformations.
//

template <typename T>
inline T linear_rgb_to_srgb(const T c)
{
    return c <= T(0.0031308)
        ? T(12.92) * c
        : T(1.055) * std::pow(c, T(1.0 / 2.4)) - T(0.055);
}

template <typename T>
inline T srgb_to_linear_rgb(const T c)
{
    return c <= T(0.04045)
        ? T(1.0 / 12.92) * c
        : std::pow((c + T(0.055)) * T(1.0 / 1.055), T(2.4));
}

template <typename T>
inline Color<T, 3> linear_rgb_to_srgb(const Color<T, 3>& linear_rgb)
{
    return Color<T, 3>(
        linear_rgb_to_srgb(linear_rgb[0]),
        linear_rgb_to_srgb(linear_rgb[1]),
        linear_rgb_to_srgb(linear_rgb[2]));
}

template <typename T>
inline Color<T, 3> srgb_to_linear_rgb(const Color<T, 3>& srgb)
{
    return Color<T, 3>(
        srgb_to_linear_rgb(srgb[0]),
        srgb_to_linear_rgb(srgb[1]),
        srgb_to_linear_rgb(srgb[2]));
}

inline float fast_linear_rgb_to_srgb(const float c)
{
    return c <= 0.0031308f
        ? 12.92f * c
        : 1.055f * fast_pow_refined(c, 1.0f / 2.4f) - 0.055f;
}

inline float fast_srgb_to_linear_rgb(const float c)
{
    return c <= 0.04045f
        ? (1.0f / 12.92f) * c
        : fast_pow_refined((c + 0.055f) * (1.0f / 1.055f), 2.4f);
}

#ifdef APPLESEED_FOUNDATION_USE_SSE

inline Color3f fast_linear_rgb_to_srgb(const Color3f& linear_rgb)
{
    SSE_ALIGN float transfer[4] =
    {
        linear_rgb[0],
        linear_rgb[1],
        linear_rgb[2],
        linear_rgb[2]
    };

    sse4f c = loadps(transfer);

    // Compute y = pow(c, 1.0f / 2.4f), see foundation/math/fastmath.h for details.
    const sse4f K = set1ps(127.0f);
    sse4f x = _mm_cvtepi32_ps(_mm_castps_si128(c));
    x = mulps(x, set1ps(0.1192092896e-6f));
    x = subps(x, K);
    sse4f z = subps(x, floorps(x));
    z = subps(z, mulps(z, z));
    z = mulps(z, set1ps(0.346607f));
    x = addps(x, z);
    x = mulps(x, set1ps(1.0f / 2.4f));
    sse4f y = subps(x, floorps(x));
    y = subps(y, mulps(y, y));
    y = mulps(y, set1ps(0.33971f));
    y = subps(addps(x, K), y);
    y = mulps(y, set1ps(8388608.0f));
    y = _mm_castsi128_ps(_mm_cvtps_epi32(y));

    // Compute both outcomes of the branch.
    const sse4f a = mulps(set1ps(12.92f), c);
    const sse4f b = subps(mulps(set1ps(1.055f), y), set1ps(0.055f));

    // Interleave them based on the actual comparison.
    const sse4f mask = cmpleps(c, set1ps(0.0031308f));
    c = addps(andps(mask, a), andnotps(mask, b));

    storeps(transfer, c);

    return Color3f(transfer[0], transfer[1], transfer[2]);
}

#else

inline Color3f fast_linear_rgb_to_srgb(const Color3f& linear_rgb)
{
    return Color3f(
        fast_linear_rgb_to_srgb(linear_rgb[0]),
        fast_linear_rgb_to_srgb(linear_rgb[1]),
        fast_linear_rgb_to_srgb(linear_rgb[2]));
}

#endif  // APPLESEED_FOUNDATION_USE_SSE

inline Color3f fast_srgb_to_linear_rgb(const Color3f& srgb)
{
    return Color3f(
        fast_srgb_to_linear_rgb(srgb[0]),
        fast_srgb_to_linear_rgb(srgb[1]),
        fast_srgb_to_linear_rgb(srgb[2]));
}


//
// Transform a 3-component color from one color space to another.
//

template <typename T>
Color<T, 3> transform_color(
    const Color<T, 3>&          color,
    const ColorSpace            from,
    const ColorSpace            to)
{
    switch (from)
    {
      case ColorSpaceLinearRGB:
        switch (to)
        {
          case ColorSpaceLinearRGB:     return color;
          case ColorSpaceSRGB:          return linear_rgb_to_srgb(color);
          case ColorSpaceCIEXYZ:        return linear_rgb_to_ciexyz(color);
          assert_otherwise;
        }

      case ColorSpaceSRGB:
        switch (to)
        {
          case ColorSpaceLinearRGB:     return srgb_to_linear_rgb(color);
          case ColorSpaceSRGB:          return color;
          case ColorSpaceCIEXYZ:        return linear_rgb_to_ciexyz(srgb_to_linear_rgb(color));
          assert_otherwise;
        }

      case ColorSpaceCIEXYZ:
        switch (to)
        {
          case ColorSpaceLinearRGB:     return ciexyz_to_linear_rgb(color);
          case ColorSpaceSRGB:          return linear_rgb_to_srgb(ciexyz_to_linear_rgb(color));
          case ColorSpaceCIEXYZ:        return color;
          assert_otherwise;
        }

      assert_otherwise;
    }

    // Keep the compiler happy.
    return color;
}


//
// Relative luminance function implementation.
//

template <typename T>
inline T luminance(const Color<T, 3>& linear_rgb)
{
    return
        T(0.212671) * linear_rgb[0] +
        T(0.715160) * linear_rgb[1] +
        T(0.072169) * linear_rgb[2];
}


//
// Spectrum <-> CIE XYZ transformations implementation.
//

template <typename T, typename Spectrum>
Color<T, 3> spectrum_to_ciexyz(
    const LightingConditions&   lighting,
    const Spectrum&             spectrum)
{
    // todo: fix to handle arbitrary numbers of samples.
    assert(Spectrum::Samples == 31);

    T x = T(0.0);
    T y = T(0.0);
    T z = T(0.0);

    for (size_t w = 0; w < 31; ++w)
    {
        const T val = spectrum[w];
        x += lighting.m_cmf[w][0] * val;
        y += lighting.m_cmf[w][1] * val;
        z += lighting.m_cmf[w][2] * val;
    }

    return Color<T, 3>(x, y, z);
}

template <typename T, typename Spectrum>
void ciexyz_to_spectrum(
    const LightingConditions&   lighting,
    const Color<T, 3>&          xyz,
    Spectrum&                   spectrum)
{
    // todo: fix to handle arbitrary number of samples.
    assert(Spectrum::Samples == 31);

    linear_rgb_to_spectrum(
        lighting,
        ciexyz_to_linear_rgb(xyz),
        spectrum);
}


//
// Linear RGB to spectrum transformation implementation.
//

template <typename T, typename Spectrum>
void linear_rgb_to_spectrum(
    const LightingConditions&   lighting,
    const Color<T, 3>&          linear_rgb,
    Spectrum&                   spectrum)
{
    // todo: fix to handle arbitrary number of samples.
    assert(Spectrum::Samples == 31);

    typedef typename Spectrum::ValueType ValueType;

    // todo: implement a better algorithm.
    for (size_t w = 0;  w < 10; ++w)
        spectrum[w] = static_cast<ValueType>(linear_rgb[2]);
    for (size_t w = 10; w < 20; ++w)
        spectrum[w] = static_cast<ValueType>(linear_rgb[1]);
    for (size_t w = 20; w < 31; ++w)
        spectrum[w] = static_cast<ValueType>(linear_rgb[0]);
}


//
// Spectrum <-> Spectrum transformation implementation.
//

template <typename T>
void spectrum_to_spectrum(
    const size_t                input_count,
    const T                     input_wavelength[],
    const T                     input_spectrum[],
    const size_t                output_count,
    const T                     output_wavelength[],
    T                           output_spectrum[],
    T                           working_storage[])
{
    const bool own_memory = (working_storage == 0);

    if (own_memory)
        working_storage = new T[input_count];

    compute_cardinal_spline_tangents(
        input_count,                    // [in]  knot count
        input_wavelength,               // [in]  knot x
        &input_spectrum[0],             // [in]  knot y
        T(0.0),                         // [in]  tension
        working_storage);               // [out] knot derivatives

    cubic_hermite_spline(
        input_count,                    // [in]  knot count
        input_wavelength,               // [in]  knot x
        &input_spectrum[0],             // [in]  knot y
        working_storage,                // [in]  knot derivatives
        output_count,                   // [in]  point count
        output_wavelength,              // [in]  point x
        &output_spectrum[0]);           // [out] point y

    if (own_memory)
        delete [] working_storage;
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_IMAGE_COLORSPACE_H
