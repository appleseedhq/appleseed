
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016 Luis Barrancos, The appleseedhq Organization
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

#ifndef AS_COLOR_DATA_H
#define AS_COLOR_DATA_H

// The xyz chromaticity coordinates for the RGB color spaces (z=1-x-y), and Y.
// OSL does not support 3x3 matrices, use vector[3] instead.

// ITU-R/BT.601 Ref: https://www.itu.int/rec/R-REC-BT.601-7-201103-I/en
#define BT601_CHROMATICITIES_Rxyz           0.640, 0.330, 0.030
#define BT601_CHROMATICITIES_Gxyz           0.290, 0.600, 0.110
#define BT601_CHROMATICITIES_Bxyz           0.150, 0.060, 0.790

#define BT601_LUMINANCE_COEFFS              0.299, 0.587, 0.114

// ITU-R/BT.709 Ref: https://www.itu.int/rec/R-REC-BT.709/en
#define BT709_CHROMATICITIES_Rxyz           0.640, 0.330, 0.030
#define BT709_CHROMATICITIES_Gxyz           0.300, 0.600, 0.100
#define BT709_CHROMATICITIES_Bxyz           0.150, 0.060, 0.790

#define BT709_LUMINANCE_COEFFS              0.212656, 0.715158, 0.072186  

// ITU-R/BT.2020 Ref: https://www.itu.int/rec/R-REC-BT.2020/en
#define BT2020_CHROMATICITIES_Rxyz          0.708, 0.292, 0.000
#define BT2020_CHROMATICITIES_Gxyz          0.170, 0.797, 0.033
#define BT2020_CHROMATICITIES_Bxyz          0.131, 0.046, 0.823

#define BT2020_LUMINANCE_COEFFS             0.2627, 0.6780, 0.0593  

// ITU-R/BT.1886 Ref: https://www.itu.int/rec/R-REC-BT.1886-0-201103-I/en
// BT.1886, BT.709 share chromaticities, Y is shared with BT.2020.
#define BT1886_CHROMATICITIES_Rxyz          BT709_CHROMATICITIES_Rxyz
#define BT1886_CHROMATICITIES_Gxyz          BT709_CHROMATICITIES_Gxyz
#define BT1886_CHROMATICITIES_Bxyz          BT709_CHROMATICITIES_Bxyz

#define BT1886_LUMINANCE_COEFFS             BT2020_LUMINANCE_COEFFS

// ITU-R/BT.2100 Ref: https://www.itu.int/rec/R-REC-BT.2100
// BT.2100 and BT.2020 share primaries chromaticity coordinates, Y. 
#define BT2100_CHROMATICITIES_Rxyz          BT2020_CHROMATICITIES_Rxyz
#define BT2100_CHROMATICITIES_Gxyz          BT2020_CHROMATICITIES_Gxyz
#define BT2100_CHROMATICITIES_Bxyz          BT2020_CHROMATICITIES_Bxyz

#define BT2100_LUMINANCE_COEFFS             BT2020_LUMINANCE_COEFFS

// sRGB Ref: https://www.w3.org/Graphics/Color/sRGB.html
// sRGB and BT.709 share primaries chromaticity coordinates, Y.
#define SRGB_CHROMATICITIES_Rxyz            BT709_CHROMATICITIES_Rxyz
#define SRGB_CHROMATICITIES_Gxyz            BT709_CHROMATICITIES_Gxyz
#define SRGB_CHROMATICITIES_Bxyz            BT709_CHROMATICITIES_Bxyz

#define SRGB_LUMINANCE_COEFFS               BT709_LUMINANCE_COEFFS

// AdobeRGB Ref: http://www.adobe.com/digitalimag/pdfs/AdobeRGB1998.pdf
#define ADOBERGB98_CHROMATICITIES_Rxyz      0.640, 0.330, 0.030
#define ADOBERGB98_CHROMATICITIES_Gxyz      0.210, 0.710, 0.080
#define ADOBERGB98_CHROMATICITIES_Bxyz      0.150, 0.060, 0.790

#define ADOBERGB98_LUMINANCE_COEFFS         0.297361, 0.627355, 0.075284

// White point xyz chromaticity coordinates, and XYZ coordinates,
// CIE1931 2° standard observer, Hernandez 1999 method (WXYZ = (1/Wy)Wxyz).
#define D55_WHITEPOINT_CIE1931_2DEG_xyz     0.33242, 0.34743, 0.32015
#define D60_WHITEPOINT_CIE1931_2DEG_xyz     0.32168, 0.33767, 0.34065
#define D65_WHITEPOINT_CIE1931_2DEG_xyz     0.31270, 0.32900, 0.35830
#define DCIP3_WHITEPOINT_CIE1931_2DEG_xyz   0.31400, 0.35100, 0.33500

#define D55_WHITEPOINT_CIE1931_2DEG_XYZ     0.956797, 1.000000, 0.921480
#define D60_WHITEPOINT_CIE1931_2DEG_XYZ     0.952646, 1.000000, 1.008825
#define D65_WHITEPOINT_CIE1931_2DEG_XYZ     0.950455, 1.000000, 1.089057
#define DCIP3_WHITEPOINT_CIE1931_2DEG_XYZ   0.894586, 1.000000, 0.954415

// Some useful quantities for the EOTF, OETF.
#define DCIP3_GAMMA         2.6
#define ADOBERGB98_GAMMA    2.19921875

// The XYZ<>RGB 3x3 matrices for given color space and white points, see
// also: http://www.brucelindbloom.com/index.html?ChromAdaptEval.html
// 3x3 matrices are not supported in OSL, use vector[3] instead.

#define RGB2XYZ_D65_ADOBERGB98_R0           0.5767309, 0.1855540, 0.1881852
#define RGB2XYZ_D65_ADOBERGB98_R1           0.2973769, 0.6273491, 0.0752741
#define RGB2XYZ_D65_ADOBERGB98_R2           0.0270343, 0.0706872, 0.9911085

#define RGB2XYZ_D65_SRGB_R0                 0.4124564, 0.3575671, 0.1804375
#define RGB2XYZ_D65_SRGB_R1                 0.2126729, 0.7151522, 0.0721750
#define RGB2XYZ_D65_SRGB_R2                 0.0193339, 0.1191920, 0.9503041

#define XYZ2RGB_D65_ADOBERGB98_R0           2.0413690,-0.5649464,-0.3446944
#define XYZ2RGB_D65_ADOBERGB98_R1          -0.9692660, 1.8760108, 0.0415560
#define XYZ2RGB_D65_ADOBERGB98_R2           0.0134474,-0.1183897, 1.0154096

#define XYZ2RGB_D65_SRGB_R0                 3.2404542,-1.5371385,-0.4985314
#define XYZ2RGB_D65_SRGB_R1                -0.9692660, 1.8760108, 0.0415560
#define XYZ2RGB_D65_SRGB_R2                 0.0556434,-0.2040259, 1.0572252 

#define RGB2XYZ_D65_ADOBERGB98 {            \
    vector(RGB2XYZ_D65_ADOBERGB98_R0),      \
    vector(RGB2XYZ_D65_ADOBERGB98_R1),      \
    vector(RGB2XYZ_D65_ADOBERGB98_R2)       \
    }      

#define RGB2XYZ_D65_SRGB {                  \
    vector(RGB2XYZ_D65_SRGB_R0),            \
    vector(RGB2XYZ_D65_SRGB_R1),            \
    vector(RGB2XYZ_D65_SRGB_R2)             \
    }            

#define XYZ2RGB_D65_ADOBERGB98 {            \
    vector(XYZ2RGB_D65_ADOBERGB98_R0),      \
    vector(XYZ2RGB_D65_ADOBERGB98_R1),      \
    vector(XYZ2RGB_D65_ADOBERGB98_R2)       \
    }

#define XYZ2RGB_D65_SRGB {                  \
    vector(XYZ2RGB_D65_SRGB_R0),            \
    vector(XYZ2RGB_D65_SRGB_R1),            \
    vector(XYZ2RGB_D65_SRGB_R2)             \
    }

#endif /// AS_COLOR_DATA_H
