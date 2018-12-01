
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2018 Luis Barrancos, The appleseedhq Organization
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

#include "appleseed/color/as_chromatic_adaptation.h"
#include "appleseed/color/as_colorimetry.h"
#include "appleseed/color/as_color_transforms.h"
#include "appleseed/color/as_transfer_functions.h"
#include "appleseed/maya/as_maya_cms_syncolor_idt.h"

#ifndef NCOMPS
#define NCOMPS  3
#endif


//
//  The luminance coefficients Y are provided by the RGB<>XYZ matrices, which
//  depend on the CIE xy chromaticity coordinates of the RGB primaries and
//  of the whitepoint W. Versions are provided, where chromatic adaptation
//  takes place if the working/rendering space has a different whitepoint than
//  the whitepoint of the used RGBW standard. This used the Bradford CAT,
//  since this seems to be the most used adaptation transform.
//

float as_luminance_D65(color in_C, string colorspace)
{
    color coeffs;

    if (colorspace == "Rec.601")
    {
        coeffs = color(REC601_D65_LUMINANCE_COEFFS);
    }
    else if (colorspace == "Rec.709" || colorspace == "sRGB" ||
             colorspace == "sRGB/Rec.709")
    {
        coeffs = color(REC709_D65_LUMINANCE_COEFFS);
    }
    else if (colorspace == "AdobeRGB")
    {
        coeffs = color(ADOBERGB_D65_LUMINANCE_COEFFS);
    }
    else if (colorspace == "Rec.2020")
    {
        coeffs = color(REC2020_D65_LUMINANCE_COEFFS);
    }
    else if (colorspace == "ACES")
    {
        coeffs = color(ACES_D65_LUMINANCE_COEFFS);
    }
    else if (colorspace == "ACEScg")
    {
        coeffs = color(ACESCG_D65_LUMINANCE_COEFFS);
    }
    else if (colorspace == "DCI-P3")
    {
        coeffs = color(DCIP3_D65_LUMINANCE_COEFFS);
    }
    else
    {
        coeffs = color(0);
    }
    return coeffs[0] * in_C[0] +
           coeffs[1] * in_C[1] +
           coeffs[2] * in_C[2];
}

float as_luminance_D60(color in_C, string colorspace)
{
    color coeffs;

    if (colorspace == "Rec.601")
    {
        coeffs = color(REC601_D60_LUMINANCE_COEFFS);
    }
    else if (colorspace == "Rec.709" || colorspace == "sRGB" ||
             colorspace == "sRGB/Rec.709")
    {
        coeffs = color(REC709_D60_LUMINANCE_COEFFS);
    }
    else if (colorspace == "AdobeRGB")
    {
        coeffs = color(ADOBERGB_D60_LUMINANCE_COEFFS);
    }
    else if (colorspace == "Rec.2020")
    {
        coeffs = color(REC2020_D60_LUMINANCE_COEFFS);
    }
    else if (colorspace == "ACES")
    {
        coeffs = color(ACES_D60_LUMINANCE_COEFFS);
    }
    else if (colorspace == "ACEScg")
    {
        coeffs = color(ACESCG_D60_LUMINANCE_COEFFS);
    }
    else if (colorspace == "DCI-P3")
    {
        coeffs = color(DCIP3_D60_LUMINANCE_COEFFS);
    }
    else
    {
        coeffs = color(0);
    }
    return coeffs[0] * in_C[0] +
           coeffs[1] * in_C[1] +
           coeffs[2] * in_C[2];
}

float as_luminance_DCI(color in_C, string colorspace)
{
    color coeffs;

    if (colorspace == "Rec.601")
    {
        coeffs = color(REC601_DCI_LUMINANCE_COEFFS);
    }
    else if (colorspace == "Rec.709" || colorspace == "sRGB" ||
             colorspace == "sRGB/Rec.709")
    {
        coeffs = color(REC709_DCI_LUMINANCE_COEFFS);
    }
    else if (colorspace == "AdobeRGB")
    {
        coeffs = color(ADOBERGB_DCI_LUMINANCE_COEFFS);
    }
    else if (colorspace == "Rec.2020")
    {
        coeffs = color(REC2020_DCI_LUMINANCE_COEFFS);
    }
    else if (colorspace == "ACES")
    {
        coeffs = color(ACES_DCI_LUMINANCE_COEFFS);
    }
    else if (colorspace == "ACEScg")
    {
        coeffs = color(ACESCG_DCI_LUMINANCE_COEFFS);
    }
    else if (colorspace == "DCI-P3")
    {
        coeffs = color(DCIP3_DCI_LUMINANCE_COEFFS);
    }
    else
    {
        coeffs = color(0);
    }
    return coeffs[0] * in_C[0] +
           coeffs[1] * in_C[1] +
           coeffs[2] * in_C[2];
}

float as_luminance(color in_C, string colorspace, string illuminant)
{
    float Y;

    if (illuminant == "D60")
    {
        Y = as_luminance_D60(in_C, colorspace);
    }
    else if (illuminant == "D65")
    {
        Y = as_luminance_D65(in_C, colorspace);
    }
    else if (illuminant == "DCI")
    {
        Y = as_luminance_DCI(in_C, colorspace);
    }
    else
    {
        Y = 0.0;
    }
    return Y;
}

float as_luminance(color in_C, string colorspace)
{
    color coeffs; // assuming input color is in "colorspace" working space

    if (colorspace == "Rec.601")
    {
        coeffs = color(REC601_D65_LUMINANCE_COEFFS);
    }
    else if (colorspace == "Rec.709" || colorspace == "sRGB" ||
             colorspace == "sRGB/Rec.709")
    {
        coeffs = color(REC709_D65_LUMINANCE_COEFFS);
    }
    else if (colorspace == "AdobeRGB")
    {
        coeffs = color(ADOBERGB_D65_LUMINANCE_COEFFS);
    }
    else if (colorspace == "Rec.2020")
    {
        coeffs = color(REC2020_D65_LUMINANCE_COEFFS);
    }
    else if (colorspace == "ACES")
    {
        coeffs = color(ACES_D60_LUMINANCE_COEFFS);
    }
    else if (colorspace == "ACEScg")
    {
        coeffs = color(ACESCG_D60_LUMINANCE_COEFFS);
    }
    else if (colorspace == "DCI-P3")
    {
        coeffs = color(DCIP3_DCI_LUMINANCE_COEFFS);
    }
    else
    {
        coeffs = color(0);
#ifdef DEBUG
        string shadername = "";

        getattribute("shader:shadername", shadername);
        warning("[DEBUG]: Invalid working space specified in %s, %s:%d\n",
                shadername, __FILE__, __LINE__);
#endif
    }
    return coeffs[0] * in_C[0] +
           coeffs[1] * in_C[1] +
           coeffs[2] * in_C[2];
}

void initialize_RGBW_primaries(
    string RGB_primaries,
    string illuminant,
    float R_CIExy[2],
    float G_CIExy[2],
    float B_CIExy[2],
    float W_CIExy[2],
    output vector RGBW_CIExyz[4])
{
    if (RGB_primaries == "Rec.601")
    {
        RGBW_CIExyz[0] = REC601_CHROMATICITIES_Rxyz;
        RGBW_CIExyz[1] = REC601_CHROMATICITIES_Gxyz;
        RGBW_CIExyz[2] = REC601_CHROMATICITIES_Bxyz;
    }
    else if (RGB_primaries == "Rec.709" || RGB_primaries == "sRGB" ||
             RGB_primaries == "sRGB/Rec.709")
    {
        RGBW_CIExyz[0] = REC709_CHROMATICITIES_Rxyz;
        RGBW_CIExyz[1] = REC709_CHROMATICITIES_Gxyz;
        RGBW_CIExyz[2] = REC709_CHROMATICITIES_Bxyz;
    }
    else if (RGB_primaries == "AdobeRGB")
    {
        RGBW_CIExyz[0] = ADOBERGB98_CHROMATICITIES_Rxyz;
        RGBW_CIExyz[1] = ADOBERGB98_CHROMATICITIES_Gxyz;
        RGBW_CIExyz[2] = ADOBERGB98_CHROMATICITIES_Bxyz;
    }
    else if (RGB_primaries == "Rec.2020")
    {
        RGBW_CIExyz[0] = REC2020_CHROMATICITIES_Rxyz;
        RGBW_CIExyz[1] = REC2020_CHROMATICITIES_Gxyz;
        RGBW_CIExyz[2] = REC2020_CHROMATICITIES_Bxyz;
    }
    else if (RGB_primaries == "DCI-P3")
    {
        RGBW_CIExyz[0] = DCIP3_CHROMATICITIES_Rxyz;
        RGBW_CIExyz[1] = DCIP3_CHROMATICITIES_Gxyz;
        RGBW_CIExyz[2] = DCIP3_CHROMATICITIES_Bxyz;
    }
    else if (RGB_primaries == "ACES")
    {
        RGBW_CIExyz[0] = ACES_AP0_CHROMATICITIES_Rxyz;
        RGBW_CIExyz[1] = ACES_AP0_CHROMATICITIES_Gxyz;
        RGBW_CIExyz[2] = ACES_AP0_CHROMATICITIES_Bxyz;
    }
    else if (RGB_primaries == "ACEScg")
    {
        RGBW_CIExyz[0] = ACESCG_AP1_CHROMATICITIES_Rxyz;
        RGBW_CIExyz[1] = ACESCG_AP1_CHROMATICITIES_Gxyz;
        RGBW_CIExyz[2] = ACESCG_AP1_CHROMATICITIES_Bxyz;
    }
    else if (RGB_primaries == "custom")
    {
        RGBW_CIExyz[0] = transform_CIExy_to_CIExyz(R_CIExy);
        RGBW_CIExyz[1] = transform_CIExy_to_CIExyz(G_CIExy);
        RGBW_CIExyz[2] = transform_CIExy_to_CIExyz(B_CIExy);
    }
    else
    {
        RGBW_CIExyz[0] = RGBW_CIExyz[1] =
        RGBW_CIExyz[2] = RGBW_CIExyz[3] = 0.0;
    }
    RGBW_CIExyz[3] = get_illuminant_CIExyz(illuminant, W_CIExy);
}

void initialize_RGB_primaries(
    string RGB_primaries,
    output vector RGB_CIExyz[3])
{
    if (RGB_primaries == "Rec.601")
    {
        RGB_CIExyz[0] = REC601_CHROMATICITIES_Rxyz;
        RGB_CIExyz[1] = REC601_CHROMATICITIES_Gxyz;
        RGB_CIExyz[2] = REC601_CHROMATICITIES_Bxyz;
    }
    else if (RGB_primaries == "Rec.709" || RGB_primaries == "sRGB" ||
             RGB_primaries == "sRGB/Rec.709")
    {
        RGB_CIExyz[0] = REC709_CHROMATICITIES_Rxyz;
        RGB_CIExyz[1] = REC709_CHROMATICITIES_Gxyz;
        RGB_CIExyz[2] = REC709_CHROMATICITIES_Bxyz;
    }
    else if (RGB_primaries == "AdobeRGB")
    {
        RGB_CIExyz[0] = ADOBERGB98_CHROMATICITIES_Rxyz;
        RGB_CIExyz[1] = ADOBERGB98_CHROMATICITIES_Gxyz;
        RGB_CIExyz[2] = ADOBERGB98_CHROMATICITIES_Bxyz;
    }
    else if (RGB_primaries == "Rec.2020")
    {
        RGB_CIExyz[0] = REC2020_CHROMATICITIES_Rxyz;
        RGB_CIExyz[1] = REC2020_CHROMATICITIES_Gxyz;
        RGB_CIExyz[2] = REC2020_CHROMATICITIES_Bxyz;
    }
    else if (RGB_primaries == "DCI-P3")
    {
        RGB_CIExyz[0] = DCIP3_CHROMATICITIES_Rxyz;
        RGB_CIExyz[1] = DCIP3_CHROMATICITIES_Gxyz;
        RGB_CIExyz[2] = DCIP3_CHROMATICITIES_Bxyz;
    }
    else if (RGB_primaries == "ACES")
    {
        RGB_CIExyz[0] = ACES_AP0_CHROMATICITIES_Rxyz;
        RGB_CIExyz[1] = ACES_AP0_CHROMATICITIES_Gxyz;
        RGB_CIExyz[2] = ACES_AP0_CHROMATICITIES_Bxyz;
    }
    else if (RGB_primaries == "ACEScg")
    {
        RGB_CIExyz[0] = ACESCG_AP1_CHROMATICITIES_Rxyz;
        RGB_CIExyz[1] = ACESCG_AP1_CHROMATICITIES_Gxyz;
        RGB_CIExyz[2] = ACESCG_AP1_CHROMATICITIES_Bxyz;
    }
    else
    {
        RGB_CIExyz[0] = RGB_CIExyz[1] = RGB_CIExyz[2] = 0.0;
    }
}

color apply_color_management(
    color input,
    string eotf,
    string rgb_primaries,
    string workingspace_rgb_primaries)
{
    color scene_linear_cms;

    if (eotf == "Raw")
    {
        scene_linear_cms = input;
    }
    else if (eotf == "sRGB")
    {
        scene_linear_cms = sRGB_EOTF(input);
    }
    else if (eotf == "Rec.709")
    {
        scene_linear_cms = Rec709_EOTF(input);
    }
    else if (eotf == "Gamma 2.2")
    {
        scene_linear_cms = gamma_CCTF(input, 2.2);
    }
    else if (eotf == "Gamma 2.4")
    {
        scene_linear_cms = gamma_CCTF(input, 2.4);
    }
    else if (eotf == "Gamma 2.6 (DCI)")
    {
        scene_linear_cms = gamma_CCTF(input, DCIP3_GAMMA);
    }
    else if (eotf == "Rec.1886")
    {
        scene_linear_cms = Rec1886_EOTF(input);
    }
    else if (eotf == "Rec.2020")
    {
        scene_linear_cms = Rec2020_EOTF(input);
    }
    else
    {
#ifdef DEBUG
        string shadername = "";
        getattribute("shader:shadername", shadername);
        warning("[WARNING]: Unknown OETF mode %s, in %s, %s:%d\n",
                eotf, shadername, __FILE__, __LINE__);
#endif
        return color(0);
    }
        
    // We're assuming the ingested material is in [0,1] range, if not,
    // we need to check the extension (*.hdr, *.exr), and bitdepth, and
    // transform to a log representation, before applying a xform, then
    // expanding it back.

    if (rgb_primaries != "Raw" &&
        rgb_primaries != workingspace_rgb_primaries)
    {
        scene_linear_cms = transform_colorspace_to_workingspace(
            scene_linear_cms,
            rgb_primaries,
            workingspace_rgb_primaries);
    }
    return scene_linear_cms;
}
