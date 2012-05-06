
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
      case ColorSpaceCIEXYZ:    return "ciexyz";
      case ColorSpaceSpectral:  return "spectral";
      default:                  return "";
    }
}


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
const Spectrum31f IlluminantCIED65(IlluminantCIED65Tab);

// Incandescent lighting illuminants.
const Spectrum31f IlluminantCIEA(IlluminantCIEATab);


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
    const float RGBCMFStilesBurch19552DefTabX[31] =
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
    const float RGBCMFStilesBurch19552DefTabY[31] =
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
    const float RGBCMFStilesBurch19552DefTabZ[31] =
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
    const float RGBCMFStilesBurch195910DefTabX[31] =
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
    const float RGBCMFStilesBurch195910DefTabY[31] =
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
    const float RGBCMFStilesBurch195910DefTabZ[31] =
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
const Spectrum31f XYZCMFCIE19312Deg[3] =
{
    Spectrum31f(XYZCMFCIE19312DegTabX),
    Spectrum31f(XYZCMFCIE19312DegTabY),
    Spectrum31f(XYZCMFCIE19312DegTabZ)
};
const Spectrum31f XYZCMFCIE1931Judd2Deg[3] =
{
    Spectrum31f(XYZCMFCIE1931Judd2DegTabX),
    Spectrum31f(XYZCMFCIE1931Judd2DegTabY),
    Spectrum31f(XYZCMFCIE1931Judd2DegTabZ)
};
const Spectrum31f XYZCMFCIE1931JuddVos2Deg[3] =
{
    Spectrum31f(XYZCMFCIE1931JuddVos2DegTabX),
    Spectrum31f(XYZCMFCIE1931JuddVos2DegTabY),
    Spectrum31f(XYZCMFCIE1931JuddVos2DegTabZ)
};
const Spectrum31f XYZCMFCIE196410Deg[3] =
{
    Spectrum31f(XYZCMFCIE196410DegTabX),
    Spectrum31f(XYZCMFCIE196410DegTabY),
    Spectrum31f(XYZCMFCIE196410DegTabZ)
};

// RGB color matching functions.
const Spectrum31f RGBCMFStilesBurch19552Def[3] =
{
    Spectrum31f(RGBCMFStilesBurch19552DefTabX),
    Spectrum31f(RGBCMFStilesBurch19552DefTabY),
    Spectrum31f(RGBCMFStilesBurch19552DefTabZ)
};
const Spectrum31f RGBCMFStilesBurch195910Def[3] =
{
    Spectrum31f(RGBCMFStilesBurch195910DefTabX),
    Spectrum31f(RGBCMFStilesBurch195910DefTabY),
    Spectrum31f(RGBCMFStilesBurch195910DefTabZ)
};


//
// Lighting conditions class implementation.
//

LightingConditions::LightingConditions(
    const Spectrum31f&  illuminant,
    const Spectrum31f   cmf[3])
{
    // Precompute convolution of color matching functions and illuminant.
    for (size_t w = 0; w < 31; ++w)
    {
        m_cmf[w][0] = cmf[0][w] * illuminant[w];
        m_cmf[w][1] = cmf[1][w] * illuminant[w];
        m_cmf[w][2] = cmf[2][w] * illuminant[w];
    }

    // Compute 1/N.
    float n = 0.0f;
    for (size_t w = 0; w < 31; ++w)
        n += m_cmf[w][1];
    float rcp_n = 1.0f / n;

    // Multiply the color matching functions by 1/N.
    for (size_t w = 0; w < 31; ++w)
        m_cmf[w] *= rcp_n;
}

}   // namespace foundation
