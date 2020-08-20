
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/regularspectrum.h"
#include "foundation/math/fastmath.h"
#include "foundation/math/matrix.h"
#include "foundation/math/scalar.h"
#include "foundation/math/spline.h"
#include "foundation/platform/compiler.h"
#ifdef APPLESEED_USE_SSE
#include "foundation/platform/sse.h"
#endif
#include "foundation/utility/otherwise.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cmath>
#include <cstddef>

namespace foundation
{

//
// Enumeration of all supported color spaces.
//

enum ColorSpace
{
    ColorSpaceLinearRGB,        // linear RGB
    ColorSpaceSRGB,             // sRGB
    ColorSpaceHSV,              // Hue Saturation Value (HSV, or HSB)
    ColorSpaceHLS,              // Hue Lightness Saturation (HLS, or HSL)
    ColorSpaceCIEXYZ,           // CIE XYZ
    ColorSpaceCIExyY,           // CIE xyY
    ColorSpaceSpectral          // spectral
};

// Return a string identifying a color space.
APPLESEED_DLLSYMBOL const char* color_space_name(const ColorSpace color_space);


//
// Transform a tristimulus value from one 3D color space to another 3D color space.
//

template <typename T>
Color<T, 3> transform_color(
    const Color<T, 3>&          color,
    const ColorSpace            from,
    const ColorSpace            to);


//
// Basis vectors to convert the CIE xy chromaticity of a D series (daylight) illuminant to a spectrum.
//
// Reference:
//
//   http://en.wikipedia.org/wiki/Standard_illuminant#Illuminant_series_D
//

extern const RegularSpectrum31f DaylightS0;                     // mean spectral radiant power
extern const RegularSpectrum31f DaylightS1;                     // first characteristic vector (yellow-blue variation)
extern const RegularSpectrum31f DaylightS2;                     // second characteristic vector (pink-green variation)


//
// Standard illuminants.
//
// References:
//
//   http://en.wikipedia.org/wiki/Standard_illuminant
//   http://cvrl.ioo.ucl.ac.uk/
//

// Daylight illuminants.
extern const RegularSpectrum31f IlluminantCIED65;               // CIE D65

// Incandescent lighting illuminants.
extern const RegularSpectrum31f IlluminantCIEA;                 // CIE A (black body radiator at 2856 K)


//
// Color Matching Functions (CMF).
//
// References:
//
//   http://en.wikipedia.org/wiki/CIE_1931_color_space
//   http://cvrl.ioo.ucl.ac.uk/
//

// XYZ color matching functions.
extern const RegularSpectrum31f XYZCMFCIE19312Deg[3];           // CIE 1931 2-deg
extern const RegularSpectrum31f XYZCMFCIE1931Judd2Deg[3];       // CIE 1931 2-deg, modified by Judd (1951)
extern const RegularSpectrum31f XYZCMFCIE1931JuddVos2Deg[3];    // CIE 1931 2-deg, modified by Judd (1951) and Vos (1978)
extern const RegularSpectrum31f XYZCMFCIE196410Deg[3];          // CIE 1964 10-deg (recommended)

// RGB color matching functions.
extern const RegularSpectrum31f RGBCMFStilesBurch19552Deg[3];   // Stiles and Burch (1955) 2-deg
extern const RegularSpectrum31f RGBCMFStilesBurch195910Deg[3];  // Stiles and Burch (1959) 10-deg (recommended)


//
// Basis spectra for RGB-to-spectrum conversion.
//

// Basis spectra for reflectance conversions.
extern const RegularSpectrum31f RGBToSpectrumWhiteReflectance;
extern const RegularSpectrum31f RGBToSpectrumCyanReflectance;
extern const RegularSpectrum31f RGBToSpectrumMagentaReflectance;
extern const RegularSpectrum31f RGBToSpectrumYellowReflectance;
extern const RegularSpectrum31f RGBToSpectrumRedReflectance;
extern const RegularSpectrum31f RGBToSpectrumGreenReflectance;
extern const RegularSpectrum31f RGBToSpectrumBlueReflectance;

// Basis spectra for illuminance conversions.
extern const RegularSpectrum31f RGBToSpectrumWhiteIlluminance;
extern const RegularSpectrum31f RGBToSpectrumCyanIlluminance;
extern const RegularSpectrum31f RGBToSpectrumMagentaIlluminance;
extern const RegularSpectrum31f RGBToSpectrumYellowIlluminance;
extern const RegularSpectrum31f RGBToSpectrumRedIlluminance;
extern const RegularSpectrum31f RGBToSpectrumGreenIlluminance;
extern const RegularSpectrum31f RGBToSpectrumBlueIlluminance;


//
// Lighting conditions, defined as a set of color matching functions and an illuminant.
//

class LightingConditions
{
  public:
    APPLESEED_SIMD4_ALIGN Color4f   m_cmf_reflectance[32];      // precomputed normalized values of (cmf[0], cmf[1], cmf[2]) * illuminant
    APPLESEED_SIMD4_ALIGN Color4f   m_cmf_illuminance[32];      // precomputed normalized values of cmf

    LightingConditions();                                       // leaves the object uninitialized

    LightingConditions(
        const RegularSpectrum31f&   illuminant,                 // illuminant
        const RegularSpectrum31f    cmf[3]);                    // color matching functions
};


//
// HSV <-> linear RGB transformations.
//
// References:
//
//   http://en.literateprograms.org/RGB_to_HSV_color_space_conversion_%28C%29
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
//   http://graphics.stanford.edu/courses/cs148-10-summer/docs/2010--kerr--cie_xyz.pdf
//

// Convert a color from the CIE XYZ color space to the linear RGB color space.
template <typename T>
Color<T, 3> ciexyz_to_linear_rgb(const Color<T, 3>& xyz);

template <typename T>
Color<T, 3> ciexyz_to_linear_rgb(
    const Color<T, 3>&      xyz,
    const Matrix<T, 3, 3>&  xyz_to_rgb);

// Convert a color from the linear RGB color space to the CIE XYZ color space.
template <typename T>
Color<T, 3> linear_rgb_to_ciexyz(const Color<T, 3>& linear_rgb);

template <typename T>
Color<T, 3> linear_rgb_to_ciexyz(
    const Color<T, 3>&      linear_rgb,
    const Matrix<T, 3, 3>&  rgb_to_xyz);

//
// CIE XYZ <-> CIE xyY transformations.
//
// Reference:
//
//   http://en.wikipedia.org/wiki/CIE_1931_color_space#The_CIE_xy_chromaticity_diagram_and_the_CIE_xyY_color_space
//

// Convert a color from the CIE XYZ color space to the CIE xyY color space.
template <typename T>
Color<T, 3> ciexyz_to_ciexyy(const Color<T, 3>& xyz);

// Convert a color from the CIE xyY color space to the CIE XYZ color space.
template <typename T>
Color<T, 3> ciexyy_to_ciexyz(const Color<T, 3>& xyy);


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
#ifdef APPLESEED_USE_SSE
inline __m128 fast_linear_rgb_to_srgb(const __m128 linear_rgb);
#endif
Color3f fast_linear_rgb_to_srgb(const Color3f& linear_rgb);
Color3f fast_srgb_to_linear_rgb(const Color3f& srgb);

// Variants of the above functions using an even faster approximation of the power function.
float faster_linear_rgb_to_srgb(const float c);
float faster_srgb_to_linear_rgb(const float c);
#ifdef APPLESEED_USE_SSE
inline __m128 faster_linear_rgb_to_srgb(const __m128 linear_rgb);
#endif
Color3f faster_linear_rgb_to_srgb(const Color3f& linear_rgb);
Color3f faster_srgb_to_linear_rgb(const Color3f& srgb);


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
// Spectrum <-> CIE XYZ transformations.
//

// Convert a spectrum to a color in the CIE XYZ color space.
template <typename T, typename SpectrumType>
Color<T, 3> spectral_reflectance_to_ciexyz(
    const LightingConditions&   lighting,
    const SpectrumType&         spectrum);

template <typename T, typename SpectrumType>
Color<T, 3> spectral_illuminance_to_ciexyz(
    const LightingConditions&   lighting,
    const SpectrumType&         spectrum);

// Convert a spectrum to a color in the CIE XYZ color space using the CIE D65 illuminant
// and the CIE 1964 10-deg color matching functions.
APPLESEED_DLLSYMBOL void spectral_reflectance_to_ciexyz_standard(
    const float                 spectrum[],
    float                       ciexyz[3]);

// Convert a reflectance in the CIE XYZ color space to a spectrum.
template <typename T, typename SpectrumType>
void ciexyz_reflectance_to_spectrum(
    const Color<T, 3>&          xyz,
    SpectrumType&               spectrum);

// Convert an illuminance in the CIE XYZ color space to a spectrum.
template <typename T, typename SpectrumType>
void ciexyz_illuminance_to_spectrum(
    const Color<T, 3>&          xyz,
    SpectrumType&               spectrum);


//
// Convert the CIE xy chromaticity of a D series (daylight) illuminant to a spectrum.
//

template <typename T, typename SpectrumType>
void daylight_ciexy_to_spectrum(
    const T                     x,
    const T                     y,
    SpectrumType&               spectrum);


//
// Linear RGB to spectrum transformation.
//
// The spectrum to linear RGB transformation can be obtained by first
// converting the spectrum to the CIE XYZ color space, then converting
// the resulting CIE XYZ color to the linear RGB color space.
//
// Reference:
//
//   An RGB to Spectrum Conversion for Reflectances, Brian Smits, University of Utah
//   http://www.cs.utah.edu/~bes/papers/color/
//

// Convert a linear RGB reflectance value to a spectrum, without any clamping.
template <typename T, typename SpectrumType>
void linear_rgb_reflectance_to_spectrum_unclamped(
    const Color<T, 3>&          linear_rgb,
    SpectrumType&               spectrum);

// Convert a linear RGB reflectance value to a spectrum, and clamp the result to [0, infinity)^N.
template <typename T, typename SpectrumType>
void linear_rgb_reflectance_to_spectrum(
    const Color<T, 3>&          linear_rgb,
    SpectrumType&               spectrum);

// Convert a linear RGB illuminance value to a spectrum, without any clamping.
template <typename T, typename SpectrumType>
void linear_rgb_illuminance_to_spectrum_unclamped(
    const Color<T, 3>&          linear_rgb,
    SpectrumType&               spectrum);

// Convert a linear RGB illuminance value to a spectrum, and clamp the result to [0, infinity)^N.
template <typename T, typename SpectrumType>
void linear_rgb_illuminance_to_spectrum(
    const Color<T, 3>&          linear_rgb,
    SpectrumType&               spectrum);


//
// Spectrum <-> Spectrum transformation.
//

// Resample a spectrum from one set of wavelengths to another.
template <typename T>
void spectrum_to_spectrum(
    const size_t                input_count,
    const T                     input_wavelength[],
    const T                     input_spectrum[],
    const size_t                output_count,
    const T                     output_wavelength[],
    T                           output_spectrum[],
    T                           working_storage[] = nullptr);


//
// Transform a tristimulus value from one 3D color space to another 3D color space.
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
    // Compute value.
    const T value = max_value(linear_rgb);

    // Value is zero: return black.
    if (value == T(0.0))
        return Color<T, 3>(0.0, 0.0, 0.0);

    // Compute chroma.
    const T chroma = value - min_value(linear_rgb);

    // Chroma is zero: return gray.
    if (chroma == T(0.0))
        return Color<T, 3>(0.0, 0.0, value);

    // Compute hue.
    const T h =
        value == linear_rgb[0] ? (linear_rgb[1] - linear_rgb[2]) / chroma :
        value == linear_rgb[1] ? (linear_rgb[2] - linear_rgb[0]) / chroma + T(2.0) :
                                 (linear_rgb[0] - linear_rgb[1]) / chroma + T(4.0);
    const T hue = mod(h * T(60.0), T(360.0));

    // Compute saturation.
    const T saturation = chroma / value;

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
      case 6: // fallthrough
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
    return
        clamp_low(
            Color<T, 3>(
                T( 3.240479) * xyz[0] + T(-1.537150) * xyz[1] + T(-0.498535) * xyz[2],
                T(-0.969256) * xyz[0] + T( 1.875991) * xyz[1] + T( 0.041556) * xyz[2],
                T( 0.055648) * xyz[0] + T(-0.204043) * xyz[1] + T( 1.057311) * xyz[2]),
            T(0.0));
}

template <typename T>
inline Color<T, 3> ciexyz_to_linear_rgb(
    const Color<T, 3>&      xyz,
    const Matrix<T, 3, 3>&  xyz_to_rgb)
{
    return clamp_low(xyz_to_rgb * xyz, T(0.0));
}

template <typename T>
inline Color<T, 3> linear_rgb_to_ciexyz(const Color<T, 3>& linear_rgb)
{
    return
        clamp_low(
            Color<T, 3>(
                T(0.412453) * linear_rgb[0] + T(0.357580) * linear_rgb[1] + T(0.180423) * linear_rgb[2],
                T(0.212671) * linear_rgb[0] + T(0.715160) * linear_rgb[1] + T(0.072169) * linear_rgb[2],
                T(0.019334) * linear_rgb[0] + T(0.119193) * linear_rgb[1] + T(0.950227) * linear_rgb[2]),
            T(0.0));
}

template <typename T>
inline Color<T, 3> linear_rgb_to_ciexyz(
    const Color<T, 3>&      linear_rgb,
    const Matrix<T, 3, 3>&  rgb_to_xyz)
{
    return clamp_low(rgb_to_xyz * linear_rgb, T(0.0));
}

//
// CIE XYZ <-> CIE xyY transformations implementation.
//

template <typename T>
inline Color<T, 3> ciexyz_to_ciexyy(const Color<T, 3>& xyz)
{
    const T rcp_sum = T(1.0) / (xyz[0] + xyz[1] + xyz[2]);
    return Color<T, 3>(xyz[0] * rcp_sum, xyz[1] * rcp_sum, xyz[1]);
}

template <typename T>
inline Color<T, 3> ciexyy_to_ciexyz(const Color<T, 3>& xyy)
{
    const T y = xyy[2] / xyy[1];
    return Color<T, 3>(y * xyy[0], xyy[2], y * (T(1.0) - xyy[0] - xyy[1]));
}


//
// Linear RGB <-> sRGB transformations implementation.
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
        : 1.055f * fast_pow(c, 1.0f / 2.4f) - 0.055f;
}

inline float fast_srgb_to_linear_rgb(const float c)
{
    return c <= 0.04045f
        ? (1.0f / 12.92f) * c
        : fast_pow((c + 0.055f) * (1.0f / 1.055f), 2.4f);
}

#ifdef APPLESEED_USE_SSE

inline __m128 fast_linear_rgb_to_srgb(const __m128 linear_rgb)
{
    // Apply 2.4 gamma correction.
    const __m128 y = fast_pow(linear_rgb, _mm_set1_ps(1.0f / 2.4f));

    // Compute both outcomes of the branch.
    const __m128 a = _mm_mul_ps(_mm_set1_ps(12.92f), linear_rgb);
    const __m128 b = _mm_sub_ps(_mm_mul_ps(_mm_set1_ps(1.055f), y), _mm_set1_ps(0.055f));

    // Interleave them based on the comparison result.
    const __m128 mask = _mm_cmple_ps(linear_rgb, _mm_set1_ps(0.0031308f));
    return _mm_add_ps(_mm_and_ps(mask, a), _mm_andnot_ps(mask, b));
}

inline Color3f fast_linear_rgb_to_srgb(const Color3f& linear_rgb)
{
    APPLESEED_SIMD4_ALIGN float transfer[4] =
    {
        linear_rgb[0],
        linear_rgb[1],
        linear_rgb[2],
        linear_rgb[2]
    };

    _mm_store_ps(transfer, fast_linear_rgb_to_srgb(_mm_load_ps(transfer)));

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

#endif  // APPLESEED_USE_SSE

inline Color3f fast_srgb_to_linear_rgb(const Color3f& srgb)
{
    return Color3f(
        fast_srgb_to_linear_rgb(srgb[0]),
        fast_srgb_to_linear_rgb(srgb[1]),
        fast_srgb_to_linear_rgb(srgb[2]));
}

inline float faster_linear_rgb_to_srgb(const float c)
{
    return c <= 0.0031308f
        ? 12.92f * c
        : 1.055f * faster_pow(c, 1.0f / 2.4f) - 0.055f;
}

inline float faster_srgb_to_linear_rgb(const float c)
{
    return c <= 0.04045f
        ? (1.0f / 12.92f) * c
        : faster_pow((c + 0.055f) * (1.0f / 1.055f), 2.4f);
}

#ifdef APPLESEED_USE_SSE

inline __m128 faster_linear_rgb_to_srgb(const __m128 linear_rgb)
{
    // Apply 2.4 gamma correction.
    const __m128 y = faster_pow(linear_rgb, _mm_set1_ps(1.0f / 2.4f));

    // Compute both outcomes of the branch.
    const __m128 a = _mm_mul_ps(_mm_set1_ps(12.92f), linear_rgb);
    const __m128 b = _mm_sub_ps(_mm_mul_ps(_mm_set1_ps(1.055f), y), _mm_set1_ps(0.055f));

    // Interleave them based on the comparison result.
    const __m128 mask = _mm_cmple_ps(linear_rgb, _mm_set1_ps(0.0031308f));
    return _mm_add_ps(_mm_and_ps(mask, a), _mm_andnot_ps(mask, b));
}

inline Color3f faster_linear_rgb_to_srgb(const Color3f& linear_rgb)
{
    APPLESEED_SIMD4_ALIGN float transfer[4] =
    {
        linear_rgb[0],
        linear_rgb[1],
        linear_rgb[2],
        linear_rgb[2]
    };

    _mm_store_ps(transfer, faster_linear_rgb_to_srgb(_mm_load_ps(transfer)));

    return Color3f(transfer[0], transfer[1], transfer[2]);
}

#else

inline Color3f faster_linear_rgb_to_srgb(const Color3f& linear_rgb)
{
    return Color3f(
        faster_linear_rgb_to_srgb(linear_rgb[0]),
        faster_linear_rgb_to_srgb(linear_rgb[1]),
        faster_linear_rgb_to_srgb(linear_rgb[2]));
}

#endif  // APPLESEED_USE_SSE

inline Color3f faster_srgb_to_linear_rgb(const Color3f& srgb)
{
    return Color3f(
        faster_srgb_to_linear_rgb(srgb[0]),
        faster_srgb_to_linear_rgb(srgb[1]),
        faster_srgb_to_linear_rgb(srgb[2]));
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

template <typename T, typename SpectrumType>
inline Color<T, 3> spectrum_to_ciexyz(
    const Color4f               cmf[32],
    const SpectrumType&         spectrum)
{
    static_assert(
        SpectrumType::Samples == 31,
        "foundation::spectrum_to_ciexyz() expects 31-channel spectra");

    T x = T(0.0);
    T y = T(0.0);
    T z = T(0.0);

    for (size_t w = 0; w < 31; ++w)
    {
        const T val = spectrum[w];
        x += cmf[w][0] * val;
        y += cmf[w][1] * val;
        z += cmf[w][2] * val;
    }

    return Color<T, 3>(x, y, z);
}

#ifdef APPLESEED_USE_SSE
template <>
inline Color3f spectrum_to_ciexyz<float, RegularSpectrum31f>(
    const Color4f               cmf[32],
    const RegularSpectrum31f&   spectrum)
{
    __m128 xyz1 = _mm_setzero_ps();
    __m128 xyz2 = _mm_setzero_ps();
    __m128 xyz3 = _mm_setzero_ps();
    __m128 xyz4 = _mm_setzero_ps();

    for (size_t w = 0; w < 8; ++w)
    {
        xyz1 = _mm_add_ps(xyz1, _mm_mul_ps(_mm_set1_ps(spectrum[4 * w + 0]), _mm_load_ps(&cmf[4 * w + 0][0])));
        xyz2 = _mm_add_ps(xyz2, _mm_mul_ps(_mm_set1_ps(spectrum[4 * w + 1]), _mm_load_ps(&cmf[4 * w + 1][0])));
        xyz3 = _mm_add_ps(xyz3, _mm_mul_ps(_mm_set1_ps(spectrum[4 * w + 2]), _mm_load_ps(&cmf[4 * w + 2][0])));
        xyz4 = _mm_add_ps(xyz4, _mm_mul_ps(_mm_set1_ps(spectrum[4 * w + 3]), _mm_load_ps(&cmf[4 * w + 3][0])));
    }

    xyz1 = _mm_add_ps(xyz1, xyz2);
    xyz3 = _mm_add_ps(xyz3, xyz4);
    xyz1 = _mm_add_ps(xyz1, xyz3);

    APPLESEED_SIMD4_ALIGN float transfer[4];
    _mm_store_ps(transfer, xyz1);

    return Color3f(transfer[0], transfer[1], transfer[2]);
}
#endif  // APPLESEED_USE_SSE

template <typename T, typename SpectrumType>
inline Color<T, 3> spectral_reflectance_to_ciexyz(
    const LightingConditions&   lighting,
    const SpectrumType&         spectrum)
{
    return spectrum_to_ciexyz<T, SpectrumType>(lighting.m_cmf_reflectance, spectrum);
}

template <typename T, typename SpectrumType>
inline Color<T, 3> spectral_illuminance_to_ciexyz(
    const LightingConditions&   lighting,
    const SpectrumType&         spectrum)
{
    return spectrum_to_ciexyz<T, SpectrumType>(lighting.m_cmf_illuminance, spectrum);
}

template <typename T, typename SpectrumType>
void ciexyz_reflectance_to_spectrum(
    const Color<T, 3>&          xyz,
    SpectrumType&               spectrum)
{
    linear_rgb_reflectance_to_spectrum(
        ciexyz_to_linear_rgb(xyz),
        spectrum);
}

template <typename T, typename SpectrumType>
void ciexyz_illuminance_to_spectrum(
    const Color<T, 3>&          xyz,
    SpectrumType&               spectrum)
{
    linear_rgb_illuminance_to_spectrum(
        ciexyz_to_linear_rgb(xyz),
        spectrum);
}


//
// Convert the CIE xy chromaticity of a D series (daylight) illuminant to a spectrum.
//

template <>
inline void daylight_ciexy_to_spectrum<float, RegularSpectrum31f>(
    const float                 x,
    const float                 y,
    RegularSpectrum31f&         spectrum)
{
    const float rcp_m = 1.0f / (0.0241f + 0.2562f * x - 0.7341f * y);
    const float m1 = (-1.3515f - 1.7703f * x + 5.9114f * y) * rcp_m;
    const float m2 = (0.0300f - 31.4424f * x + 30.0717f * y) * rcp_m;

    spectrum = DaylightS0;

    RegularSpectrum31f s1 = DaylightS1;
    s1 *= m1;
    spectrum += s1;

    RegularSpectrum31f s2 = DaylightS2;
    s2 *= m2;
    spectrum += s2;
}


//
// Linear RGB to spectrum transformation implementation.
//

namespace impl
{
    template <typename T, typename SpectrumType>
    void linear_rgb_to_spectrum_approximation(
        const Color<T, 3>&      linear_rgb,
        SpectrumType&           spectrum)
    {
        for (size_t w = 0;  w < 10; ++w)
            spectrum[w] = static_cast<T>(linear_rgb[2]);

        for (size_t w = 10; w < 20; ++w)
            spectrum[w] = static_cast<T>(linear_rgb[1]);

        for (size_t w = 20; w < 31; ++w)
            spectrum[w] = static_cast<T>(linear_rgb[0]);
    }

    template <typename T, typename SpectrumType>
    void linear_rgb_to_spectrum(
        const Color<T, 3>&      linear_rgb,
        const SpectrumType&     white,
        const SpectrumType&     cyan,
        const SpectrumType&     magenta,
        const SpectrumType&     yellow,
        const SpectrumType&     red,
        const SpectrumType&     green,
        const SpectrumType&     blue,
        SpectrumType&           spectrum)
    {
        const T r = linear_rgb[0];
        const T g = linear_rgb[1];
        const T b = linear_rgb[2];
        SpectrumType tmp;

        if (r <= g && r <= b)
        {
            spectrum = white;
            spectrum *= r;

            if (g <= b)
            {
                tmp = cyan;
                tmp *= g - r;
                spectrum += tmp;

                tmp = blue;
                tmp *= b - g;
                spectrum += tmp;
            }
            else
            {
                tmp = cyan;
                tmp *= b - r;
                spectrum += tmp;

                tmp = green;
                tmp *= g - b;
                spectrum += tmp;
            }
        }
        else if (g <= r && g <= b)
        {
            spectrum = white;
            spectrum *= g;

            if (r <= b)
            {
                tmp = magenta;
                tmp *= r - g;
                spectrum += tmp;

                tmp = blue;
                tmp *= b - r;
                spectrum += tmp;
            }
            else
            {
                tmp = magenta;
                tmp *= b - g;
                spectrum += tmp;

                tmp = red;
                tmp *= r - b;
                spectrum += tmp;
            }
        }
        else
        {
            spectrum = white;
            spectrum *= b;

            if (r <= g)
            {
                tmp = yellow;
                tmp *= r - b;
                spectrum += tmp;

                tmp = green;
                tmp *= g - r;
                spectrum += tmp;
            }
            else
            {
                tmp = yellow;
                tmp *= g - b;
                spectrum += tmp;

                tmp = red;
                tmp *= r - g;
                spectrum += tmp;
            }
        }
    }
}

template <typename T, typename SpectrumType>
void linear_rgb_reflectance_to_spectrum_unclamped(
    const Color<T, 3>&          linear_rgb,
    SpectrumType&               spectrum)
{
    impl::linear_rgb_to_spectrum(
        linear_rgb,
        RGBToSpectrumWhiteReflectance,
        RGBToSpectrumCyanReflectance,
        RGBToSpectrumMagentaReflectance,
        RGBToSpectrumYellowReflectance,
        RGBToSpectrumRedReflectance,
        RGBToSpectrumGreenReflectance,
        RGBToSpectrumBlueReflectance,
        spectrum);
}

template <typename T, typename SpectrumType>
inline void linear_rgb_reflectance_to_spectrum(
    const Color<T, 3>&          linear_rgb,
    SpectrumType&               spectrum)
{
    linear_rgb_reflectance_to_spectrum_unclamped(linear_rgb, spectrum);
    clamp_low_in_place(spectrum, T(0.0));
}

template <typename T, typename SpectrumType>
void linear_rgb_illuminance_to_spectrum_unclamped(
    const Color<T, 3>&          linear_rgb,
    SpectrumType&               spectrum)
{
    impl::linear_rgb_to_spectrum(
        linear_rgb,
        RGBToSpectrumWhiteIlluminance,
        RGBToSpectrumCyanIlluminance,
        RGBToSpectrumMagentaIlluminance,
        RGBToSpectrumYellowIlluminance,
        RGBToSpectrumRedIlluminance,
        RGBToSpectrumGreenIlluminance,
        RGBToSpectrumBlueIlluminance,
        spectrum);
}

template <typename T, typename SpectrumType>
inline void linear_rgb_illuminance_to_spectrum(
    const Color<T, 3>&          linear_rgb,
    SpectrumType&               spectrum)
{
    linear_rgb_illuminance_to_spectrum_unclamped(linear_rgb, spectrum);
    clamp_low_in_place(spectrum, T(0.0));
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
    const bool own_memory = (working_storage == nullptr);

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
        delete[] working_storage;
}

}   // namespace foundation
