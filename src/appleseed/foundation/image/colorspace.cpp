
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

// Interface header.
#include "colorspace.h"

namespace foundation
{

const char* color_space_name(const ColorSpace color_space)
{
    switch (color_space)
    {
      case ColorSpaceLinearRGB: return "linear_rgb";
      case ColorSpaceSRGB:      return "srgb";
      case ColorSpaceHSV:       return "hsv";
      case ColorSpaceHLS:       return "hls";
      case ColorSpaceCIEXYZ:    return "ciexyz";
      case ColorSpaceCIExyY:    return "ciexyy";
      case ColorSpaceSpectral:  return "spectral";
      assert_otherwise;
    }

    // Keep the compiler happy.
    return "";
}


//
// Basis vectors to convert the CIE xy chromaticity of a D series (daylight) illuminant to a spectrum.
//

namespace
{
    static const float DaylightS0Amplitudes[31] =
    {
         94.8f,             // 400 nm
        104.8f,             // 410 nm
        105.9f,             // 420 nm
         96.8f,             // 430 nm
        113.9f,             // 440 nm
        125.6f,             // 450 nm
        125.5f,             // 460 nm
        121.3f,             // 470 nm
        121.3f,             // 480 nm
        113.5f,             // 490 nm
        113.1f,             // 500 nm
        110.8f,             // 510 nm
        106.5f,             // 520 nm
        108.8f,             // 530 nm
        105.3f,             // 540 nm
        104.4f,             // 550 nm
        100.0f,             // 560 nm
         96.0f,             // 570 nm
         95.1f,             // 580 nm
         89.1f,             // 590 nm
         90.5f,             // 600 nm
         90.3f,             // 610 nm
         88.4f,             // 620 nm
         84.0f,             // 630 nm
         85.1f,             // 640 nm
         81.9f,             // 650 nm
         82.6f,             // 660 nm
         84.9f,             // 670 nm
         81.3f,             // 680 nm
         71.9f,             // 690 nm
         74.3f              // 700 nm
    };

    static const float DaylightS1Amplitudes[31] =
    {
         43.4f,             // 400 nm
         46.3f,             // 410 nm
         43.9f,             // 420 nm
         37.1f,             // 430 nm
         36.7f,             // 440 nm
         35.9f,             // 450 nm
         32.6f,             // 460 nm
         27.9f,             // 470 nm
         24.3f,             // 480 nm
         20.1f,             // 490 nm
         16.2f,             // 500 nm
         13.2f,             // 510 nm
          8.6f,             // 520 nm
          6.1f,             // 530 nm
          4.2f,             // 540 nm
          1.9f,             // 550 nm
          0.0f,             // 560 nm
         -1.6f,             // 570 nm
         -3.5f,             // 580 nm
         -3.5f,             // 590 nm
         -5.8f,             // 600 nm
         -7.2f,             // 610 nm
         -8.6f,             // 620 nm
         -9.5f,             // 630 nm
        -10.9f,             // 640 nm
        -10.7f,             // 650 nm
        -12.0f,             // 660 nm
        -14.0f,             // 670 nm
        -13.6f,             // 680 nm
        -12.0f,             // 690 nm
        -13.3f              // 700 nm
    };

    static const float DaylightS2Amplitudes[31] =
    {
        -1.1f,              // 400 nm
        -0.5f,              // 410 nm
        -0.7f,              // 420 nm
        -1.2f,              // 430 nm
        -2.6f,              // 440 nm
        -2.9f,              // 450 nm
        -2.8f,              // 460 nm
        -2.6f,              // 470 nm
        -2.6f,              // 480 nm
        -1.8f,              // 490 nm
        -1.5f,              // 500 nm
        -1.3f,              // 510 nm
        -1.2f,              // 520 nm
        -1.0f,              // 530 nm
        -0.5f,              // 540 nm
        -0.3f,              // 550 nm
         0.0f,              // 560 nm
         0.2f,              // 570 nm
         0.5f,              // 580 nm
         2.1f,              // 590 nm
         3.2f,              // 600 nm
         4.1f,              // 610 nm
         4.7f,              // 620 nm
         5.1f,              // 630 nm
         6.7f,              // 640 nm
         7.3f,              // 650 nm
         8.6f,              // 660 nm
         9.8f,              // 670 nm
        10.2f,              // 680 nm
         8.3f,              // 690 nm
         9.6f               // 700 nm
    };
}

const RegularSpectrum31f DaylightS0(RegularSpectrum31f::from_array(DaylightS0Amplitudes));
const RegularSpectrum31f DaylightS1(RegularSpectrum31f::from_array(DaylightS1Amplitudes));
const RegularSpectrum31f DaylightS2(RegularSpectrum31f::from_array(DaylightS2Amplitudes));


//
// Standard illuminants.
//

namespace
{
    const float IlluminantCIED65Tab[31] =
    {
         82.754900f,        // 400 nm
         91.486000f,        // 410 nm
         93.431800f,        // 420 nm
         86.682300f,        // 430 nm
        104.865000f,        // 440 nm
        117.008000f,        // 450 nm
        117.812000f,        // 460 nm
        114.861000f,        // 470 nm
        115.923000f,        // 480 nm
        108.811000f,        // 490 nm
        109.354000f,        // 500 nm
        107.802000f,        // 510 nm
        104.790000f,        // 520 nm
        107.689000f,        // 530 nm
        104.405000f,        // 540 nm
        104.046000f,        // 550 nm
        100.000000f,        // 560 nm
         96.334200f,        // 570 nm
         95.788000f,        // 580 nm
         88.685600f,        // 590 nm
         90.006200f,        // 600 nm
         89.599100f,        // 610 nm
         87.698700f,        // 620 nm
         83.288600f,        // 630 nm
         83.699200f,        // 640 nm
         80.026800f,        // 650 nm
         80.214600f,        // 660 nm
         82.277800f,        // 670 nm
         78.284200f,        // 680 nm
         69.721300f,        // 690 nm
         71.609100f         // 700 nm
    };

    const float IlluminantCIEATab[31] =
    {
         14.708000f,        // 400 nm
         17.675300f,        // 410 nm
         20.995000f,        // 420 nm
         24.670900f,        // 430 nm
         28.702700f,        // 440 nm
         33.085900f,        // 450 nm
         37.812100f,        // 460 nm
         42.869300f,        // 470 nm
         48.242300f,        // 480 nm
         53.913200f,        // 490 nm
         59.861100f,        // 500 nm
         66.063500f,        // 510 nm
         72.495900f,        // 520 nm
         79.132600f,        // 530 nm
         85.947000f,        // 540 nm
         92.912000f,        // 550 nm
        100.000000f,        // 560 nm
        107.184000f,        // 570 nm
        114.436000f,        // 580 nm
        121.731000f,        // 590 nm
        129.043000f,        // 600 nm
        136.346000f,        // 610 nm
        143.618000f,        // 620 nm
        150.836000f,        // 630 nm
        157.979000f,        // 640 nm
        165.028000f,        // 650 nm
        171.963000f,        // 660 nm
        178.769000f,        // 670 nm
        185.429000f,        // 680 nm
        191.931000f,        // 690 nm
        198.261000f         // 700 nm
    };
}

// Daylight illuminants.
const RegularSpectrum31f IlluminantCIED65(RegularSpectrum31f::from_array(IlluminantCIED65Tab));

// Incandescent lighting illuminants.
const RegularSpectrum31f IlluminantCIEA(RegularSpectrum31f::from_array(IlluminantCIEATab));


//
// Color Matching Functions (CMF).
//

namespace
{
    // XYZ color matching functions -- CIE 1931 2-deg.
    const float XYZCMFCIE19312DegTabX[31] =
    {
        0.014310000000f,    // 400 nm
        0.043510000000f,    // 410 nm
        0.134380000000f,    // 420 nm
        0.283900000000f,    // 430 nm
        0.348280000000f,    // 440 nm
        0.336200000000f,    // 450 nm
        0.290800000000f,    // 460 nm
        0.195360000000f,    // 470 nm
        0.095640000000f,    // 480 nm
        0.032010000000f,    // 490 nm
        0.004900000000f,    // 500 nm
        0.009300000000f,    // 510 nm
        0.063270000000f,    // 520 nm
        0.165500000000f,    // 530 nm
        0.290400000000f,    // 540 nm
        0.433449900000f,    // 550 nm
        0.594500000000f,    // 560 nm
        0.762100000000f,    // 570 nm
        0.916300000000f,    // 580 nm
        1.026300000000f,    // 590 nm
        1.062200000000f,    // 600 nm
        1.002600000000f,    // 610 nm
        0.854449900000f,    // 620 nm
        0.642400000000f,    // 630 nm
        0.447900000000f,    // 640 nm
        0.283500000000f,    // 650 nm
        0.164900000000f,    // 660 nm
        0.087400000000f,    // 670 nm
        0.046770000000f,    // 680 nm
        0.022700000000f,    // 690 nm
        0.011359160000f     // 700 nm
    };
    const float XYZCMFCIE19312DegTabY[31] =
    {
        0.000396000000f,    // 400 nm
        0.001210000000f,    // 410 nm
        0.004000000000f,    // 420 nm
        0.011600000000f,    // 430 nm
        0.023000000000f,    // 440 nm
        0.038000000000f,    // 450 nm
        0.060000000000f,    // 460 nm
        0.090980000000f,    // 470 nm
        0.139020000000f,    // 480 nm
        0.208020000000f,    // 490 nm
        0.323000000000f,    // 500 nm
        0.503000000000f,    // 510 nm
        0.710000000000f,    // 520 nm
        0.862000000000f,    // 530 nm
        0.954000000000f,    // 540 nm
        0.994950100000f,    // 550 nm
        0.995000000000f,    // 560 nm
        0.952000000000f,    // 570 nm
        0.870000000000f,    // 580 nm
        0.757000000000f,    // 590 nm
        0.631000000000f,    // 600 nm
        0.503000000000f,    // 610 nm
        0.381000000000f,    // 620 nm
        0.265000000000f,    // 630 nm
        0.175000000000f,    // 640 nm
        0.107000000000f,    // 650 nm
        0.061000000000f,    // 660 nm
        0.032000000000f,    // 670 nm
        0.017000000000f,    // 680 nm
        0.008210000000f,    // 690 nm
        0.004102000000f     // 700 nm
    };
    const float XYZCMFCIE19312DegTabZ[31] =
    {
        0.067850010000f,    // 400 nm
        0.207400000000f,    // 410 nm
        0.645600000000f,    // 420 nm
        1.385600000000f,    // 430 nm
        1.747060000000f,    // 440 nm
        1.772110000000f,    // 450 nm
        1.669200000000f,    // 460 nm
        1.287640000000f,    // 470 nm
        0.812950100000f,    // 480 nm
        0.465180000000f,    // 490 nm
        0.272000000000f,    // 500 nm
        0.158200000000f,    // 510 nm
        0.078249990000f,    // 520 nm
        0.042160000000f,    // 530 nm
        0.020300000000f,    // 540 nm
        0.008749999000f,    // 550 nm
        0.003900000000f,    // 560 nm
        0.002100000000f,    // 570 nm
        0.001650001000f,    // 580 nm
        0.001100000000f,    // 590 nm
        0.000800000000f,    // 600 nm
        0.000340000000f,    // 610 nm
        0.000190000000f,    // 620 nm
        0.000049999990f,    // 630 nm
        0.000020000000f,    // 640 nm
        0.000000000000f,    // 650 nm
        0.000000000000f,    // 660 nm
        0.000000000000f,    // 670 nm
        0.000000000000f,    // 680 nm
        0.000000000000f,    // 690 nm
        0.000000000000f     // 700 nm
    };

    // XYZ color matching functions -- CIE 1931 2-deg, modified by Judd (1951).
    const float XYZCMFCIE1931Judd2DegTabX[31] =
    {
        0.0611f,            // 400 nm
        0.1267f,            // 410 nm
        0.2285f,            // 420 nm
        0.3081f,            // 430 nm
        0.3312f,            // 440 nm
        0.2888f,            // 450 nm
        0.2323f,            // 460 nm
        0.1745f,            // 470 nm
        0.0920f,            // 480 nm
        0.0318f,            // 490 nm
        0.0048f,            // 500 nm
        0.0093f,            // 510 nm
        0.0636f,            // 520 nm
        0.1668f,            // 530 nm
        0.2926f,            // 540 nm
        0.4364f,            // 550 nm
        0.5970f,            // 560 nm
        0.7642f,            // 570 nm
        0.9159f,            // 580 nm
        1.0225f,            // 590 nm
        1.0544f,            // 600 nm
        0.9922f,            // 610 nm
        0.8432f,            // 620 nm
        0.6327f,            // 630 nm
        0.4404f,            // 640 nm
        0.2787f,            // 650 nm
        0.1619f,            // 660 nm
        0.0858f,            // 670 nm
        0.0459f,            // 680 nm
        0.0222f,            // 690 nm
        0.0113f             // 700 nm
    };
    const float XYZCMFCIE1931Judd2DegTabY[31] =
    {
        0.0045f,            // 400 nm
        0.0093f,            // 410 nm
        0.0175f,            // 420 nm
        0.0273f,            // 430 nm
        0.0379f,            // 440 nm
        0.0468f,            // 450 nm
        0.0600f,            // 460 nm
        0.0910f,            // 470 nm
        0.1390f,            // 480 nm
        0.2080f,            // 490 nm
        0.3230f,            // 500 nm
        0.5030f,            // 510 nm
        0.7100f,            // 520 nm
        0.8620f,            // 530 nm
        0.9540f,            // 540 nm
        0.9950f,            // 550 nm
        0.9950f,            // 560 nm
        0.9520f,            // 570 nm
        0.8700f,            // 580 nm
        0.7570f,            // 590 nm
        0.6310f,            // 600 nm
        0.5030f,            // 610 nm
        0.3810f,            // 620 nm
        0.2650f,            // 630 nm
        0.1750f,            // 640 nm
        0.1070f,            // 650 nm
        0.0610f,            // 660 nm
        0.0320f,            // 670 nm
        0.0170f,            // 680 nm
        0.0082f,            // 690 nm
        0.0041f             // 700 nm
    };
    const float XYZCMFCIE1931Judd2DegTabZ[31] =
    {
        0.2799f,            // 400 nm
        0.5835f,            // 410 nm
        1.0622f,            // 420 nm
        1.4526f,            // 430 nm
        1.6064f,            // 440 nm
        1.4717f,            // 450 nm
        1.2880f,            // 460 nm
        1.1133f,            // 470 nm
        0.7552f,            // 480 nm
        0.4461f,            // 490 nm
        0.2644f,            // 500 nm
        0.1541f,            // 510 nm
        0.0763f,            // 520 nm
        0.0412f,            // 530 nm
        0.0200f,            // 540 nm
        0.0088f,            // 550 nm
        0.0039f,            // 560 nm
        0.0020f,            // 570 nm
        0.0016f,            // 580 nm
        0.0011f,            // 590 nm
        0.0007f,            // 600 nm
        0.0003f,            // 610 nm
        0.0002f,            // 620 nm
        0.0001f,            // 630 nm
        0.0000f,            // 640 nm
        0.0000f,            // 650 nm
        0.0000f,            // 660 nm
        0.0000f,            // 670 nm
        0.0000f,            // 680 nm
        0.0000f,            // 690 nm
        0.0000f             // 700 nm
    };

    // XYZ color matching functions -- CIE 1931 2-deg, modified by Judd (1951) and Vos (1978).
    const float XYZCMFCIE1931JuddVos2DegTabX[31] =
    {
        3.798100e-002f,     // 400 nm
        9.994100e-002f,     // 410 nm
        2.294800e-001f,     // 420 nm
        3.109500e-001f,     // 430 nm
        3.333600e-001f,     // 440 nm
        2.888200e-001f,     // 450 nm
        2.327600e-001f,     // 460 nm
        1.747600e-001f,     // 470 nm
        9.194400e-002f,     // 480 nm
        3.173100e-002f,     // 490 nm
        4.849100e-003f,     // 500 nm
        9.289900e-003f,     // 510 nm
        6.379100e-002f,     // 520 nm
        1.669200e-001f,     // 530 nm
        2.926900e-001f,     // 540 nm
        4.363500e-001f,     // 550 nm
        5.974800e-001f,     // 560 nm
        7.642500e-001f,     // 570 nm
        9.163500e-001f,     // 580 nm
        1.023000e+000f,     // 590 nm
        1.055000e+000f,     // 600 nm
        9.923900e-001f,     // 610 nm
        8.434600e-001f,     // 620 nm
        6.328900e-001f,     // 630 nm
        4.406200e-001f,     // 640 nm
        2.786200e-001f,     // 650 nm
        1.616100e-001f,     // 660 nm
        8.575300e-002f,     // 670 nm
        4.583400e-002f,     // 680 nm
        2.218700e-002f,     // 690 nm
        1.109800e-002f      // 700 nm
    };
    const float XYZCMFCIE1931JuddVos2DegTabY[31] =
    {
        2.800000e-003f,     // 400 nm
        7.400000e-003f,     // 410 nm
        1.750000e-002f,     // 420 nm
        2.730000e-002f,     // 430 nm
        3.790000e-002f,     // 440 nm
        4.680000e-002f,     // 450 nm
        6.000000e-002f,     // 460 nm
        9.098000e-002f,     // 470 nm
        1.390200e-001f,     // 480 nm
        2.080200e-001f,     // 490 nm
        3.230000e-001f,     // 500 nm
        5.030000e-001f,     // 510 nm
        7.100000e-001f,     // 520 nm
        8.620000e-001f,     // 530 nm
        9.540000e-001f,     // 540 nm
        9.949500e-001f,     // 550 nm
        9.950000e-001f,     // 560 nm
        9.520000e-001f,     // 570 nm
        8.700000e-001f,     // 580 nm
        7.570000e-001f,     // 590 nm
        6.310000e-001f,     // 600 nm
        5.030000e-001f,     // 610 nm
        3.810000e-001f,     // 620 nm
        2.650000e-001f,     // 630 nm
        1.750000e-001f,     // 640 nm
        1.070000e-001f,     // 650 nm
        6.100000e-002f,     // 660 nm
        3.200000e-002f,     // 670 nm
        1.700000e-002f,     // 680 nm
        8.210000e-003f,     // 690 nm
        4.102000e-003f      // 700 nm
    };
    const float XYZCMFCIE1931JuddVos2DegTabZ[31] =
    {
        1.740900e-001f,     // 400 nm
        4.605300e-001f,     // 410 nm
        1.065800e+000f,     // 420 nm
        1.467200e+000f,     // 430 nm
        1.616600e+000f,     // 440 nm
        1.471700e+000f,     // 450 nm
        1.291700e+000f,     // 460 nm
        1.113800e+000f,     // 470 nm
        7.559600e-001f,     // 480 nm
        4.466900e-001f,     // 490 nm
        2.643700e-001f,     // 500 nm
        1.544500e-001f,     // 510 nm
        7.658500e-002f,     // 520 nm
        4.136600e-002f,     // 530 nm
        2.004200e-002f,     // 540 nm
        8.782300e-003f,     // 550 nm
        4.049300e-003f,     // 560 nm
        2.277100e-003f,     // 570 nm
        1.806600e-003f,     // 580 nm
        1.234800e-003f,     // 590 nm
        9.056400e-004f,     // 600 nm
        4.288500e-004f,     // 610 nm
        2.559800e-004f,     // 620 nm
        9.769400e-005f,     // 630 nm
        5.116500e-005f,     // 640 nm
        2.423800e-005f,     // 650 nm
        1.190600e-005f,     // 660 nm
        5.600600e-006f,     // 670 nm
        2.791200e-006f,     // 680 nm
        1.313500e-006f,     // 690 nm
        6.476700e-007f      // 700 nm
    };

    // XYZ color matching functions -- CIE 1964 10-deg.
    const float XYZCMFCIE196410DegTabX[31] =
    {
        0.019109700000f,    // 400 nm
        0.084736000000f,    // 410 nm
        0.204492000000f,    // 420 nm
        0.314679000000f,    // 430 nm
        0.383734000000f,    // 440 nm
        0.370702000000f,    // 450 nm
        0.302273000000f,    // 460 nm
        0.195618000000f,    // 470 nm
        0.080507000000f,    // 480 nm
        0.016172000000f,    // 490 nm
        0.003816000000f,    // 500 nm
        0.037465000000f,    // 510 nm
        0.117749000000f,    // 520 nm
        0.236491000000f,    // 530 nm
        0.376772000000f,    // 540 nm
        0.529826000000f,    // 550 nm
        0.705224000000f,    // 560 nm
        0.878655000000f,    // 570 nm
        1.014160000000f,    // 580 nm
        1.118520000000f,    // 590 nm
        1.123990000000f,    // 600 nm
        1.030480000000f,    // 610 nm
        0.856297000000f,    // 620 nm
        0.647467000000f,    // 630 nm
        0.431567000000f,    // 640 nm
        0.268329000000f,    // 650 nm
        0.152568000000f,    // 660 nm
        0.081260600000f,    // 670 nm
        0.040850800000f,    // 680 nm
        0.019941300000f,    // 690 nm
        0.009576880000f     // 700 nm
    };
    const float XYZCMFCIE196410DegTabY[31] =
    {
        0.002004400000f,    // 400 nm
        0.008756000000f,    // 410 nm
        0.021391000000f,    // 420 nm
        0.038676000000f,    // 430 nm
        0.062077000000f,    // 440 nm
        0.089456000000f,    // 450 nm
        0.128201000000f,    // 460 nm
        0.185190000000f,    // 470 nm
        0.253589000000f,    // 480 nm
        0.339133000000f,    // 490 nm
        0.460777000000f,    // 500 nm
        0.606741000000f,    // 510 nm
        0.761757000000f,    // 520 nm
        0.875211000000f,    // 530 nm
        0.961988000000f,    // 540 nm
        0.991761000000f,    // 550 nm
        0.997340000000f,    // 560 nm
        0.955552000000f,    // 570 nm
        0.868934000000f,    // 580 nm
        0.777405000000f,    // 590 nm
        0.658341000000f,    // 600 nm
        0.527963000000f,    // 610 nm
        0.398057000000f,    // 620 nm
        0.283493000000f,    // 630 nm
        0.179828000000f,    // 640 nm
        0.107633000000f,    // 650 nm
        0.060281000000f,    // 660 nm
        0.031800400000f,    // 670 nm
        0.015905100000f,    // 680 nm
        0.007748800000f,    // 690 nm
        0.003717740000f     // 700 nm
    };
    const float XYZCMFCIE196410DegTabZ[31] =
    {
        0.086010900000f,    // 400 nm
        0.389366000000f,    // 410 nm
        0.972542000000f,    // 420 nm
        1.553480000000f,    // 430 nm
        1.967280000000f,    // 440 nm
        1.994800000000f,    // 450 nm
        1.745370000000f,    // 460 nm
        1.317560000000f,    // 470 nm
        0.772125000000f,    // 480 nm
        0.415254000000f,    // 490 nm
        0.218502000000f,    // 500 nm
        0.112044000000f,    // 510 nm
        0.060709000000f,    // 520 nm
        0.030451000000f,    // 530 nm
        0.013676000000f,    // 540 nm
        0.003988000000f,    // 550 nm
        0.000000000000f,    // 560 nm
        0.000000000000f,    // 570 nm
        0.000000000000f,    // 580 nm
        0.000000000000f,    // 590 nm
        0.000000000000f,    // 600 nm
        0.000000000000f,    // 610 nm
        0.000000000000f,    // 620 nm
        0.000000000000f,    // 630 nm
        0.000000000000f,    // 640 nm
        0.000000000000f,    // 650 nm
        0.000000000000f,    // 660 nm
        0.000000000000f,    // 670 nm
        0.000000000000f,    // 680 nm
        0.000000000000f,    // 690 nm
        0.000000000000f     // 700 nm
    };

    // RGB color matching functions -- Stiles and Burch (1955) 2-deg.
    const float RGBCMFStilesBurch19552DegTabX[31] =
    {
         9.62640e-003f,     // 400 nm
         3.08030e-002f,     // 410 nm
         5.16620e-002f,     // 420 nm
         4.42870e-002f,     // 430 nm
         1.47630e-002f,     // 440 nm
        -2.91300e-002f,     // 450 nm
        -9.62240e-002f,     // 460 nm
        -1.74860e-001f,     // 470 nm
        -2.37800e-001f,     // 480 nm
        -2.77270e-001f,     // 490 nm
        -2.95000e-001f,     // 500 nm
        -2.67590e-001f,     // 510 nm
        -1.47680e-001f,     // 520 nm
         1.06140e-001f,     // 530 nm
         4.19760e-001f,     // 540 nm
         7.90040e-001f,     // 550 nm
         1.22830e+000f,     // 560 nm
         1.74760e+000f,     // 570 nm
         2.27240e+000f,     // 580 nm
         2.67250e+000f,     // 590 nm
         2.87170e+000f,     // 600 nm
         2.76010e+000f,     // 610 nm
         2.37430e+000f,     // 620 nm
         1.81450e+000f,     // 630 nm
         1.25430e+000f,     // 640 nm
         7.86420e-001f,     // 650 nm
         4.43200e-001f,     // 660 nm
         2.34550e-001f,     // 670 nm
         1.20860e-001f,     // 680 nm
         6.02600e-002f,     // 690 nm
         2.81140e-002f      // 700 nm
    };
    const float RGBCMFStilesBurch19552DegTabY[31] =
    {
        -2.16890e-003f,     // 400 nm
        -7.20480e-003f,     // 410 nm
        -1.66510e-002f,     // 420 nm
        -1.99360e-002f,     // 430 nm
        -7.34570e-003f,     // 440 nm
         1.96100e-002f,     // 450 nm
         7.09540e-002f,     // 460 nm
         1.50880e-001f,     // 470 nm
         2.40420e-001f,     // 480 nm
         3.33530e-001f,     // 490 nm
         4.90600e-001f,     // 500 nm
         7.01840e-001f,     // 510 nm
         9.10760e-001f,     // 520 nm
         1.03390e+000f,     // 530 nm
         1.05120e+000f,     // 540 nm
         1.03680e+000f,     // 550 nm
         9.37830e-001f,     // 560 nm
         8.28350e-001f,     // 570 nm
         6.49300e-001f,     // 580 nm
         4.76750e-001f,     // 590 nm
         3.00690e-001f,     // 600 nm
         1.65750e-001f,     // 610 nm
         7.46820e-002f,     // 620 nm
         2.63330e-002f,     // 630 nm
         4.50330e-003f,     // 640 nm
        -1.96450e-003f,     // 650 nm
        -2.62620e-003f,     // 660 nm
        -1.87000e-003f,     // 670 nm
        -1.07550e-003f,     // 680 nm
        -5.67650e-004f,     // 690 nm
        -2.62310e-004f      // 700 nm
    };
    const float RGBCMFStilesBurch19552DegTabZ[31] =
    {
         6.23710e-002f,     // 400 nm
         2.27500e-001f,     // 410 nm
         5.23960e-001f,     // 420 nm
         7.96040e-001f,     // 430 nm
         9.63950e-001f,     // 440 nm
         9.18750e-001f,     // 450 nm
         7.85540e-001f,     // 460 nm
         6.10980e-001f,     // 470 nm
         3.61950e-001f,     // 480 nm
         1.95930e-001f,     // 490 nm
         1.07490e-001f,     // 500 nm
         5.02480e-002f,     // 510 nm
         1.33090e-002f,     // 520 nm
        -4.15740e-003f,     // 530 nm
        -1.21910e-002f,     // 540 nm
        -1.46810e-002f,     // 550 nm
        -1.46130e-002f,     // 560 nm
        -1.26500e-002f,     // 570 nm
        -9.93170e-003f,     // 580 nm
        -7.02100e-003f,     // 590 nm
        -4.27430e-003f,     // 600 nm
        -2.26930e-003f,     // 610 nm
        -1.50690e-003f,     // 620 nm
        -5.53160e-004f,     // 630 nm
        -1.43190e-004f,     // 640 nm
         1.10810e-004f,     // 650 nm
         2.26560e-004f,     // 660 nm
         1.63610e-004f,     // 670 nm
         5.10330e-005f,     // 680 nm
         3.12110e-005f,     // 690 nm
         1.65210e-005f      // 700 nm
    };

    // RGB color matching functions -- Stiles and Burch (1959) 10-deg.
    const float RGBCMFStilesBurch195910DegTabX[31] =
    {
         8.9000E-03f,       // 400 nm
         3.5000E-02f,       // 410 nm
         7.0200E-02f,       // 420 nm
         7.4500E-02f,       // 430 nm
         3.2300E-02f,       // 440 nm
        -4.7800E-02f,       // 450 nm
        -1.5860E-01f,       // 460 nm
        -2.8480E-01f,       // 470 nm
        -3.7760E-01f,       // 480 nm
        -4.3170E-01f,       // 490 nm
        -4.3500E-01f,       // 500 nm
        -3.6730E-01f,       // 510 nm
        -1.8550E-01f,       // 520 nm
         1.2700E-01f,       // 530 nm
         5.3620E-01f,       // 540 nm
         1.0059E+00f,       // 550 nm
         1.5574E+00f,       // 560 nm
         2.1511E+00f,       // 570 nm
         2.6574E+00f,       // 580 nm
         3.0779E+00f,       // 590 nm
         3.1673E+00f,       // 600 nm
         2.9462E+00f,       // 610 nm
         2.4526E+00f,       // 620 nm
         1.8358E+00f,       // 630 nm
         1.2428E+00f,       // 640 nm
         7.8270E-01f,       // 650 nm
         4.4420E-01f,       // 660 nm
         2.3940E-01f,       // 670 nm
         1.2210E-01f,       // 680 nm
         5.8600E-02f,       // 690 nm
         2.8400E-02f        // 700 nm
    };
    const float RGBCMFStilesBurch195910DegTabY[31] =
    {
        -2.5000E-03f,       // 400 nm
        -1.1900E-02f,       // 410 nm
        -2.8900E-02f,       // 420 nm
        -3.4900E-02f,       // 430 nm
        -1.6900E-02f,       // 440 nm
         2.8300E-02f,       // 450 nm
         1.0820E-01f,       // 460 nm
         2.2010E-01f,       // 470 nm
         3.4280E-01f,       // 480 nm
         4.7160E-01f,       // 490 nm
         6.2600E-01f,       // 500 nm
         7.9350E-01f,       // 510 nm
         9.4770E-01f,       // 520 nm
         1.0203E+00f,       // 530 nm
         1.0517E+00f,       // 540 nm
         1.0029E+00f,       // 550 nm
         9.1620E-01f,       // 560 nm
         7.8230E-01f,       // 570 nm
         5.9660E-01f,       // 580 nm
         4.2030E-01f,       // 590 nm
         2.5910E-01f,       // 600 nm
         1.3670E-01f,       // 610 nm
         6.1100E-02f,       // 620 nm
         2.1500E-02f,       // 630 nm
         4.4000E-03f,       // 640 nm
        -1.3680E-03f,       // 650 nm
        -2.1680E-03f,       // 660 nm
        -1.6420E-03f,       // 670 nm
        -9.4700E-04f,       // 680 nm
        -4.7800E-04f,       // 690 nm
        -2.3500E-04f        // 700 nm
    };
    const float RGBCMFStilesBurch195910DegTabZ[31] =
    {
         4.0000E-02f,       // 400 nm
         1.8020E-01f,       // 410 nm
         4.6700E-01f,       // 420 nm
         7.6380E-01f,       // 430 nm
         9.7550E-01f,       // 440 nm
         9.9960E-01f,       // 450 nm
         8.2970E-01f,       // 460 nm
         6.1340E-01f,       // 470 nm
         3.4950E-01f,       // 480 nm
         1.8190E-01f,       // 490 nm
         9.1000E-02f,       // 500 nm
         3.5700E-02f,       // 510 nm
         9.5000E-03f,       // 520 nm
        -4.3000E-03f,       // 530 nm
        -8.2000E-03f,       // 540 nm
        -9.7000E-03f,       // 550 nm
        -9.3000E-03f,       // 560 nm
        -8.0000E-03f,       // 570 nm
        -6.3000E-03f,       // 580 nm
        -4.4500E-03f,       // 590 nm
        -2.7700E-03f,       // 600 nm
        -1.5000E-03f,       // 610 nm
        -6.8000E-04f,       // 620 nm
        -2.7200E-04f,       // 630 nm
        -5.4900E-05f,       // 640 nm
         2.3700E-05f,       // 650 nm
         2.6100E-05f,       // 660 nm
         1.8200E-05f,       // 670 nm
         1.0300E-05f,       // 680 nm
         5.2200E-06f,       // 690 nm
         2.5600E-06f        // 700 nm
    };
}

// XYZ color matching functions.
const RegularSpectrum31f XYZCMFCIE19312Deg[3] =
{
    RegularSpectrum31f::from_array(XYZCMFCIE19312DegTabX),
    RegularSpectrum31f::from_array(XYZCMFCIE19312DegTabY),
    RegularSpectrum31f::from_array(XYZCMFCIE19312DegTabZ)
};
const RegularSpectrum31f XYZCMFCIE1931Judd2Deg[3] =
{
    RegularSpectrum31f::from_array(XYZCMFCIE1931Judd2DegTabX),
    RegularSpectrum31f::from_array(XYZCMFCIE1931Judd2DegTabY),
    RegularSpectrum31f::from_array(XYZCMFCIE1931Judd2DegTabZ)
};
const RegularSpectrum31f XYZCMFCIE1931JuddVos2Deg[3] =
{
    RegularSpectrum31f::from_array(XYZCMFCIE1931JuddVos2DegTabX),
    RegularSpectrum31f::from_array(XYZCMFCIE1931JuddVos2DegTabY),
    RegularSpectrum31f::from_array(XYZCMFCIE1931JuddVos2DegTabZ)
};
const RegularSpectrum31f XYZCMFCIE196410Deg[3] =
{
    RegularSpectrum31f::from_array(XYZCMFCIE196410DegTabX),
    RegularSpectrum31f::from_array(XYZCMFCIE196410DegTabY),
    RegularSpectrum31f::from_array(XYZCMFCIE196410DegTabZ)
};

// RGB color matching functions.
const RegularSpectrum31f RGBCMFStilesBurch19552Deg[3] =
{
    RegularSpectrum31f::from_array(RGBCMFStilesBurch19552DegTabX),
    RegularSpectrum31f::from_array(RGBCMFStilesBurch19552DegTabY),
    RegularSpectrum31f::from_array(RGBCMFStilesBurch19552DegTabZ)
};
const RegularSpectrum31f RGBCMFStilesBurch195910Deg[3] =
{
    RegularSpectrum31f::from_array(RGBCMFStilesBurch195910DegTabX),
    RegularSpectrum31f::from_array(RGBCMFStilesBurch195910DegTabY),
    RegularSpectrum31f::from_array(RGBCMFStilesBurch195910DegTabZ)
};


//
// Basis spectra for RGB-to-spectrum conversion.
//
// These are generated by the Foundation_Image_ColorSpace::ResamplePBRTBasisSpectra unit test
// in foundation/meta/tests/test_colorspace.cpp.
//

namespace
{
    const float RGBToSpectrumWhiteReflectanceTab[31] =
    {
         1.061387f,         // 400 nm
         1.062091f,         // 410 nm
         1.062237f,         // 420 nm
         1.062381f,         // 430 nm
         1.062469f,         // 440 nm
         1.062411f,         // 450 nm
         1.062487f,         // 460 nm
         1.062502f,         // 470 nm
         1.062409f,         // 480 nm
         1.062052f,         // 490 nm
         1.061349f,         // 500 nm
         1.061037f,         // 510 nm
         1.061312f,         // 520 nm
         1.061379f,         // 530 nm
         1.061760f,         // 540 nm
         1.062337f,         // 550 nm
         1.062536f,         // 560 nm
         1.062453f,         // 570 nm
         1.062512f,         // 580 nm
         1.062427f,         // 590 nm
         1.062479f,         // 600 nm
         1.062553f,         // 610 nm
         1.062541f,         // 620 nm
         1.062416f,         // 630 nm
         1.062356f,         // 640 nm
         1.062562f,         // 650 nm
         1.061957f,         // 660 nm
         1.060339f,         // 670 nm
         1.059459f,         // 680 nm
         1.060084f,         // 690 nm
         1.060248f          // 700 nm
    };

    const float RGBToSpectrumCyanReflectanceTab[31] =
    {
         1.013784f,         // 400 nm
         1.031559f,         // 410 nm
         1.014911f,         // 420 nm
         1.025949f,         // 430 nm
         1.044892f,         // 440 nm
         1.049293f,         // 450 nm
         1.044493f,         // 460 nm
         1.019764f,         // 470 nm
         1.046143f,         // 480 nm
         1.053069f,         // 490 nm
         1.053699f,         // 500 nm
         1.053409f,         // 510 nm
         1.053772f,         // 520 nm
         1.053003f,         // 530 nm
         1.052717f,         // 540 nm
         1.054052f,         // 550 nm
         1.055816f,         // 560 nm
         1.067498f,         // 570 nm
         0.974937f,         // 580 nm
         0.557098f,         // 590 nm
         0.162351f,         // 600 nm
        -0.004918f,         // 610 nm
        -0.001546f,         // 620 nm
        -0.006570f,         // 630 nm
        -0.003945f,         // 640 nm
        -0.000951f,         // 650 nm
         0.008167f,         // 660 nm
         0.004759f,         // 670 nm
         0.001682f,         // 680 nm
         0.015969f,         // 690 nm
         0.004066f          // 700 nm
    };

    const float RGBToSpectrumMagentaReflectanceTab[31] =
    {
         0.982980f,         // 400 nm
         0.991489f,         // 410 nm
         1.013132f,         // 420 nm
         1.018938f,         // 430 nm
         1.020450f,         // 440 nm
         1.012822f,         // 450 nm
         0.997617f,         // 460 nm
         1.018910f,         // 470 nm
         0.996257f,         // 480 nm
         0.624846f,         // 490 nm
         0.023716f,         // 500 nm
         0.000435f,         // 510 nm
         0.003808f,         // 520 nm
         0.001347f,         // 530 nm
        -0.006551f,         // 540 nm
        -0.002995f,         // 550 nm
        -0.009466f,         // 560 nm
         0.046101f,         // 570 nm
         0.307088f,         // 580 nm
         0.688640f,         // 590 nm
         0.983592f,         // 600 nm
         0.971656f,         // 610 nm
         1.014684f,         // 620 nm
         1.005811f,         // 630 nm
         0.966371f,         // 640 nm
         0.876699f,         // 650 nm
         0.898889f,         // 660 nm
         0.952359f,         // 670 nm
         0.968253f,         // 680 nm
         0.969608f,         // 690 nm
         0.859295f          // 700 nm
    };

    const float RGBToSpectrumYellowReflectanceTab[31] =
    {
        -0.005256f,         // 400 nm
        -0.006240f,         // 410 nm
        -0.006453f,         // 420 nm
        -0.005079f,         // 430 nm
         0.002202f,         // 440 nm
         0.041046f,         // 450 nm
         0.126522f,         // 460 nm
         0.240308f,         // 470 nm
         0.381160f,         // 480 nm
         0.545267f,         // 490 nm
         0.732706f,         // 500 nm
         0.899055f,         // 510 nm
         1.026172f,         // 520 nm
         1.054259f,         // 530 nm
         1.051548f,         // 540 nm
         1.051072f,         // 550 nm
         1.051318f,         // 560 nm
         1.051766f,         // 570 nm
         1.051519f,         // 580 nm
         1.051164f,         // 590 nm
         1.051176f,         // 600 nm
         1.051657f,         // 610 nm
         1.051418f,         // 620 nm
         1.051589f,         // 630 nm
         1.051238f,         // 640 nm
         1.051408f,         // 650 nm
         1.051198f,         // 660 nm
         1.051017f,         // 670 nm
         1.049825f,         // 680 nm
         1.048018f,         // 690 nm
         1.048736f          // 700 nm
    };

    const float RGBToSpectrumRedReflectanceTab[31] =
    {
         0.123370f,         // 400 nm
         0.118612f,         // 410 nm
         0.093120f,         // 420 nm
         0.053243f,         // 430 nm
         0.007507f,         // 440 nm
        -0.003131f,         // 450 nm
         0.016717f,         // 460 nm
         0.006158f,         // 470 nm
         0.012482f,         // 480 nm
        -0.006159f,         // 490 nm
        -0.001790f,         // 500 nm
        -0.010045f,         // 510 nm
        -0.004015f,         // 520 nm
        -0.008488f,         // 530 nm
        -0.009905f,         // 540 nm
        -0.005552f,         // 550 nm
         0.002525f,         // 560 nm
        -0.018421f,         // 570 nm
         0.098336f,         // 580 nm
         0.679751f,         // 590 nm
         0.998411f,         // 600 nm
         0.993848f,         // 610 nm
         1.003718f,         // 620 nm
         0.993893f,         // 630 nm
         0.996498f,         // 640 nm
         1.007329f,         // 650 nm
         0.994003f,         // 660 nm
         0.994352f,         // 670 nm
         0.999046f,         // 680 nm
         0.975771f,         // 690 nm
         0.978881f          // 700 nm
    };

    const float RGBToSpectrumGreenReflectanceTab[31] =
    {
        -0.011856f,         // 400 nm
        -0.010107f,         // 410 nm
        -0.011761f,         // 420 nm
        -0.010045f,         // 430 nm
        -0.007570f,         // 440 nm
        -0.011862f,         // 450 nm
        -0.000730f,         // 460 nm
         0.116436f,         // 470 nm
         0.437371f,         // 480 nm
         0.760744f,         // 490 nm
         0.956170f,         // 500 nm
         0.996844f,         // 510 nm
         1.000443f,         // 520 nm
         0.999553f,         // 530 nm
         0.999740f,         // 540 nm
         0.999719f,         // 550 nm
         1.001184f,         // 560 nm
         0.999628f,         // 570 nm
         0.903812f,         // 580 nm
         0.578153f,         // 590 nm
         0.239696f,         // 600 nm
         0.013386f,         // 610 nm
        -0.003558f,         // 620 nm
        -0.004351f,         // 630 nm
        -0.006185f,         // 640 nm
        -0.008415f,         // 650 nm
        -0.008937f,         // 660 nm
        -0.008493f,         // 670 nm
        -0.008510f,         // 680 nm
        -0.006252f,         // 690 nm
         0.001272f          // 700 nm
    };

    const float RGBToSpectrumBlueReflectanceTab[31] =
    {
         0.994496f,         // 400 nm
         0.995734f,         // 410 nm
         0.992298f,         // 420 nm
         0.996506f,         // 430 nm
         1.000538f,         // 440 nm
         1.000383f,         // 450 nm
         1.002111f,         // 460 nm
         0.957210f,         // 470 nm
         0.764892f,         // 480 nm
         0.554058f,         // 490 nm
         0.343920f,         // 500 nm
         0.161104f,         // 510 nm
         0.038247f,         // 520 nm
         0.001238f,         // 530 nm
        -0.001204f,         // 540 nm
        -0.000583f,         // 550 nm
         0.000324f,         // 560 nm
         0.002647f,         // 570 nm
         0.003114f,         // 580 nm
        -0.000802f,         // 590 nm
         0.000202f,         // 600 nm
         0.007214f,         // 610 nm
         0.023857f,         // 620 nm
         0.035788f,         // 630 nm
         0.047014f,         // 640 nm
         0.050053f,         // 650 nm
         0.050377f,         // 660 nm
         0.046082f,         // 670 nm
         0.036494f,         // 680 nm
         0.028174f,         // 690 nm
         0.018875f          // 700 nm
    };

    const float RGBToSpectrumWhiteIlluminanceTab[31] =
    {
         1.156698f,         // 400 nm
         1.155758f,         // 410 nm
         1.155931f,         // 420 nm
         1.156565f,         // 430 nm
         1.156822f,         // 440 nm
         1.156811f,         // 450 nm
         1.156651f,         // 460 nm
         1.156388f,         // 470 nm
         1.156707f,         // 480 nm
         1.156509f,         // 490 nm
         1.156426f,         // 500 nm
         1.157271f,         // 510 nm
         1.150024f,         // 520 nm
         1.137745f,         // 530 nm
         1.130518f,         // 540 nm
         1.133846f,         // 550 nm
         1.098581f,         // 560 nm
         1.045236f,         // 570 nm
         1.036640f,         // 580 nm
         0.987287f,         // 590 nm
         0.953973f,         // 600 nm
         0.925293f,         // 610 nm
         0.916277f,         // 620 nm
         0.901951f,         // 630 nm
         0.896439f,         // 640 nm
         0.891381f,         // 650 nm
         0.884983f,         // 660 nm
         0.881022f,         // 670 nm
         0.878510f,         // 680 nm
         0.876875f,         // 690 nm
         0.880276f          // 700 nm
    };

    const float RGBToSpectrumCyanIlluminanceTab[31] =
    {
         1.133496f,         // 400 nm
         1.135722f,         // 410 nm
         1.135677f,         // 420 nm
         1.135899f,         // 430 nm
         1.136179f,         // 440 nm
         1.136363f,         // 450 nm
         1.136230f,         // 460 nm
         1.135595f,         // 470 nm
         1.136418f,         // 480 nm
         1.136031f,         // 490 nm
         1.136028f,         // 500 nm
         1.135439f,         // 510 nm
         1.136184f,         // 520 nm
         1.135807f,         // 530 nm
         1.135432f,         // 540 nm
         1.136641f,         // 550 nm
         1.135842f,         // 560 nm
         1.061325f,         // 570 nm
         0.843320f,         // 580 nm
         0.565191f,         // 590 nm
         0.280469f,         // 600 nm
         0.100495f,         // 610 nm
        -0.004459f,         // 620 nm
        -0.013942f,         // 630 nm
        -0.011351f,         // 640 nm
        -0.012158f,         // 650 nm
        -0.008124f,         // 660 nm
        -0.005750f,         // 670 nm
        -0.008846f,         // 680 nm
        -0.008741f,         // 690 nm
        -0.005066f          // 700 nm
    };

    const float RGBToSpectrumMagentaIlluminanceTab[31] =
    {
         1.074704f,         // 400 nm
         1.076600f,         // 410 nm
         1.078852f,         // 420 nm
         1.076860f,         // 430 nm
         1.073222f,         // 440 nm
         1.072362f,         // 450 nm
         1.075529f,         // 460 nm
         1.085388f,         // 470 nm
         1.076622f,         // 480 nm
         0.948043f,         // 490 nm
         0.577859f,         // 500 nm
         0.133047f,         // 510 nm
         0.006134f,         // 520 nm
        -0.004691f,         // 530 nm
        -0.001787f,         // 540 nm
        -0.000471f,         // 550 nm
        -0.000761f,         // 560 nm
        -0.004745f,         // 570 nm
         0.055622f,         // 580 nm
         0.296176f,         // 590 nm
         0.548960f,         // 600 nm
         0.901497f,         // 610 nm
         1.058057f,         // 620 nm
         1.089931f,         // 630 nm
         1.073832f,         // 640 nm
         1.034064f,         // 650 nm
         1.017433f,         // 660 nm
         1.054028f,         // 670 nm
         1.048301f,         // 680 nm
         0.995864f,         // 690 nm
         1.075629f          // 700 nm
    };

    const float RGBToSpectrumYellowIlluminanceTab[31] =
    {
         0.000371f,         // 400 nm
         0.000189f,         // 410 nm
        -0.000055f,         // 420 nm
        -0.000159f,         // 430 nm
        -0.000136f,         // 440 nm
        -0.002447f,         // 450 nm
        -0.000851f,         // 460 nm
         0.112473f,         // 470 nm
         0.547520f,         // 480 nm
         1.037173f,         // 490 nm
         1.033988f,         // 500 nm
         1.036503f,         // 510 nm
         1.036619f,         // 520 nm
         1.036492f,         // 530 nm
         1.036760f,         // 540 nm
         1.036743f,         // 550 nm
         1.036461f,         // 560 nm
         1.036481f,         // 570 nm
         1.036714f,         // 580 nm
         1.036487f,         // 590 nm
         1.036149f,         // 600 nm
         1.035322f,         // 610 nm
         1.014072f,         // 620 nm
         0.874031f,         // 630 nm
         0.764727f,         // 640 nm
         0.685721f,         // 650 nm
         0.625980f,         // 660 nm
         0.597532f,         // 670 nm
         0.595525f,         // 680 nm
         0.587864f,         // 690 nm
         0.562571f          // 700 nm
    };

    const float RGBToSpectrumRedIlluminanceTab[31] =
    {
         0.060372f,         // 400 nm
         0.058076f,         // 410 nm
         0.049804f,         // 420 nm
         0.041882f,         // 430 nm
         0.032383f,         // 440 nm
         0.016357f,         // 450 nm
         0.001227f,         // 460 nm
        -0.000600f,         // 470 nm
         0.000994f,         // 480 nm
         0.000352f,         // 490 nm
        -0.000416f,         // 500 nm
        -0.000134f,         // 510 nm
        -0.000109f,         // 520 nm
        -0.000136f,         // 530 nm
        -0.000159f,         // 540 nm
        -0.002088f,         // 550 nm
         0.003661f,         // 560 nm
         0.060596f,         // 570 nm
         0.200799f,         // 580 nm
         0.382020f,         // 590 nm
         0.585726f,         // 600 nm
         0.759210f,         // 610 nm
         0.879348f,         // 620 nm
         0.953040f,         // 630 nm
         0.984016f,         // 640 nm
         0.997430f,         // 650 nm
         0.992023f,         // 660 nm
         0.990260f,         // 670 nm
         0.988620f,         // 680 nm
         0.976626f,         // 690 nm
         0.991906f          // 700 nm
    };

    const float RGBToSpectrumGreenIlluminanceTab[31] =
    {
         0.010635f,         // 400 nm
         0.006551f,         // 410 nm
         0.002472f,         // 420 nm
         0.001207f,         // 430 nm
        -0.012218f,         // 440 nm
        -0.009396f,         // 450 nm
         0.005333f,         // 460 nm
         0.041161f,         // 470 nm
         0.393510f,         // 480 nm
         1.026335f,         // 490 nm
         1.033528f,         // 500 nm
         1.032193f,         // 510 nm
         1.036581f,         // 520 nm
         1.019782f,         // 530 nm
         1.025242f,         // 540 nm
         1.036703f,         // 550 nm
         1.037640f,         // 560 nm
         1.034223f,         // 570 nm
         1.032591f,         // 580 nm
         0.866002f,         // 590 nm
        -0.030362f,         // 600 nm
         0.000527f,         // 610 nm
         0.006544f,         // 620 nm
         0.000415f,         // 630 nm
         0.017375f,         // 640 nm
         0.012188f,         // 650 nm
         0.002288f,         // 660 nm
         0.001011f,         // 670 nm
        -0.002722f,         // 680 nm
        -0.004106f,         // 690 nm
         0.015684f          // 700 nm
    };

    const float RGBToSpectrumBlueIlluminanceTab[31] =
    {
         1.054973f,         // 400 nm
         1.053157f,         // 410 nm
         1.056342f,         // 420 nm
         1.058145f,         // 430 nm
         1.058072f,         // 440 nm
         1.058307f,         // 450 nm
         1.057469f,         // 460 nm
         1.056468f,         // 470 nm
         1.060424f,         // 480 nm
         1.031067f,         // 490 nm
         0.359200f,         // 500 nm
         0.025142f,         // 510 nm
        -0.008347f,         // 520 nm
        -0.001384f,         // 530 nm
        -0.001313f,         // 540 nm
        -0.001800f,         // 550 nm
        -0.000581f,         // 560 nm
         0.000669f,         // 570 nm
        -0.001740f,         // 580 nm
        -0.001339f,         // 590 nm
        -0.001779f,         // 600 nm
        -0.001401f,         // 610 nm
         0.008460f,         // 620 nm
         0.024439f,         // 630 nm
         0.071304f,         // 640 nm
         0.122407f,         // 650 nm
         0.151016f,         // 660 nm
         0.152744f,         // 670 nm
         0.153725f,         // 680 nm
         0.168626f,         // 690 nm
         0.168186f          // 700 nm
    };
}

// Basis spectra for reflectance conversions.
const RegularSpectrum31f RGBToSpectrumWhiteReflectance(RegularSpectrum31f::from_array(RGBToSpectrumWhiteReflectanceTab));
const RegularSpectrum31f RGBToSpectrumCyanReflectance(RegularSpectrum31f::from_array(RGBToSpectrumCyanReflectanceTab));
const RegularSpectrum31f RGBToSpectrumMagentaReflectance(RegularSpectrum31f::from_array(RGBToSpectrumMagentaReflectanceTab));
const RegularSpectrum31f RGBToSpectrumYellowReflectance(RegularSpectrum31f::from_array(RGBToSpectrumYellowReflectanceTab));
const RegularSpectrum31f RGBToSpectrumRedReflectance(RegularSpectrum31f::from_array(RGBToSpectrumRedReflectanceTab));
const RegularSpectrum31f RGBToSpectrumGreenReflectance(RegularSpectrum31f::from_array(RGBToSpectrumGreenReflectanceTab));
const RegularSpectrum31f RGBToSpectrumBlueReflectance(RegularSpectrum31f::from_array(RGBToSpectrumBlueReflectanceTab));

// Basis spectra for illuminance conversions.
const RegularSpectrum31f RGBToSpectrumWhiteIlluminance(RegularSpectrum31f::from_array(RGBToSpectrumWhiteIlluminanceTab));
const RegularSpectrum31f RGBToSpectrumCyanIlluminance(RegularSpectrum31f::from_array(RGBToSpectrumCyanIlluminanceTab));
const RegularSpectrum31f RGBToSpectrumMagentaIlluminance(RegularSpectrum31f::from_array(RGBToSpectrumMagentaIlluminanceTab));
const RegularSpectrum31f RGBToSpectrumYellowIlluminance(RegularSpectrum31f::from_array(RGBToSpectrumYellowIlluminanceTab));
const RegularSpectrum31f RGBToSpectrumRedIlluminance(RegularSpectrum31f::from_array(RGBToSpectrumRedIlluminanceTab));
const RegularSpectrum31f RGBToSpectrumGreenIlluminance(RegularSpectrum31f::from_array(RGBToSpectrumGreenIlluminanceTab));
const RegularSpectrum31f RGBToSpectrumBlueIlluminance(RegularSpectrum31f::from_array(RGBToSpectrumBlueIlluminanceTab));


//
// Lighting conditions class implementation.
//

LightingConditions::LightingConditions()
{
}

LightingConditions::LightingConditions(
    const RegularSpectrum31f&   illuminant,
    const RegularSpectrum31f    cmf[3])
{
    // Precompute convolution of color matching functions and illuminant.
    for (size_t w = 0; w < 32; ++w)
    {
        m_cmf[w][0] = cmf[0][w] * illuminant[w];
        m_cmf[w][1] = cmf[1][w] * illuminant[w];
        m_cmf[w][2] = cmf[2][w] * illuminant[w];
        m_cmf[w][3] = 0.0f;
    }

    // Integrate the luminance.
    float n = 0.0f;
    for (size_t w = 0; w < 31; ++w)
        n += m_cmf[w][1];
    float rcp_n = 1.0f / n;

    // Rescale the color matching functions such that luminance integrates to 1.
    for (size_t w = 0; w < 31; ++w)
        m_cmf[w] *= rcp_n;
}


//
// Spectrum <-> CIE XYZ transformations implementation.
//

void spectrum_to_ciexyz_standard(
    const float                 spectrum[],
    float                       ciexyz[3])
{
    const LightingConditions lighting_conditions(IlluminantCIED65, XYZCMFCIE19312Deg);

    const Color3f c =
        spectrum_to_ciexyz<float, RegularSpectrum31f>(
            lighting_conditions,
            RegularSpectrum31f::from_array(spectrum));

    ciexyz[0] = c[0];
    ciexyz[1] = c[1];
    ciexyz[2] = c[2];
}

}   // namespace foundation
