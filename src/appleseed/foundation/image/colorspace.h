
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "foundation/math/spline.h"
#include "foundation/utility/otherwise.h"

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstddef>

namespace foundation
{

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
// in the Rec. 709:
//
//   Y = 0.2126 R + 0.7152 G + 0.0722 B.
//
// This is equivalent to transforming the linear RGB triplet to the
// CIE XYZ color space, and keeping the Y component.
//
// References:
//
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
    // Public members.
    Color3f                     m_cmf[31];                  // precomputed values of (cmf[0], cmf[1], cmf[2]) * illuminant

    // Constructor.
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
// CIE XYZ <-> linear RGB transformations implementation.
//

// Convert a color from the CIE XYZ color space to the linear RGB color space.
template <typename T>
inline Color<T, 3> ciexyz_to_linear_rgb(const Color<T, 3>& xyz)
{
    return Color<T, 3>(
        T( 3.240479) * xyz[0] + T(-1.537150) * xyz[1] + T(-0.498535) * xyz[2],
        T(-0.969256) * xyz[0] + T( 1.875991) * xyz[1] + T( 0.041556) * xyz[2],
        T( 0.055648) * xyz[0] + T(-0.204043) * xyz[1] + T( 1.057311) * xyz[2]);
}

// Convert a color from the linear RGB color space to the CIE XYZ color space.
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

// Convert a color component from the linear RGB color space to the sRGB color space.
template <typename T>
inline T linear_rgb_to_srgb(const T c)
{
    return c <= T(0.0031308)
        ? T(12.92) * c
        : T(1.055) * std::pow(c, T(1.0 / 2.4)) - T(0.055);
}

// Convert a color component from the sRGB color space to the linear RGB color space.
template <typename T>
inline T srgb_to_linear_rgb(const T c)
{
    return c <= T(0.04045)
        ? T(1.0 / 12.92) * c
        : std::pow((c + T(0.055)) * T(1.0 / 1.055), T(2.4));
}

// Convert a color from the linear RGB color space to the sRGB color space.
template <typename T>
inline Color<T, 3> linear_rgb_to_srgb(const Color<T, 3>& linear_rgb)
{
    return Color<T, 3>(
        linear_rgb_to_srgb(linear_rgb[0]),
        linear_rgb_to_srgb(linear_rgb[1]),
        linear_rgb_to_srgb(linear_rgb[2]));
}

// Convert a color from the sRGB color space to the linear RGB color space.
template <typename T>
inline Color<T, 3> srgb_to_linear_rgb(const Color<T, 3>& srgb)
{
    return Color<T, 3>(
        srgb_to_linear_rgb(srgb[0]),
        srgb_to_linear_rgb(srgb[1]),
        srgb_to_linear_rgb(srgb[2]));
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

// Convert a spectrum to a color in the CIE XYZ color space.
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

// Convert a color in the CIE XYZ color space to a spectrum.
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

// Convert a color in the linear RGB color space to a spectrum.
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
