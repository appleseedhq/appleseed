
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Luis Barrancos, The appleseedhq Organization
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
#include "appleseed/math/as_math_helpers.h"

vector transform_CIExy_to_CIExyz(float CIEx, float CIEy)
{
    return vector(CIEx, CIEy, 1.0 - CIEx - CIEy);
}

vector transform_CIExy_to_CIExyz(float CIExy[2])
{
    return transform_CIExy_to_CIExyz(CIExy[0], CIExy[1]);
}

void transform_CIExy_to_CIExyz(float CIExy[2], output float CIExyz[3])
{
    CIExyz[0] = CIExy[0];
    CIExyz[1] = CIExy[1];
    CIExyz[2] = 1.0 - CIExy[0] - CIExy[1];
}

vector transform_CIExy_to_CIEXYZ(float CIEx, float CIEy)
{
    return (CIEy == 0.0)
        ? vector(0)
        : vector(CIEx / CIEy,
                 1.0,
                 (1.0 - CIEx - CIEy) / CIEy); // z / y
}

vector transform_CIExy_to_CIEXYZ(float CIExy[2])
{
    return transform_CIExy_to_CIEXYZ(CIExy[0], CIExy[1]);
}

vector transform_CIExyz_to_CIEXYZ(vector CIExyz)
{
    return (CIExyz[1] == 0.0)
        ? vector(0)
        : CIExyz / CIExyz[1];
}

vector transform_CIExyY_to_CIEXYZ(vector CIExyY)
{
    return (CIExyY[1] == 0.0)
        ? vector(0)
        : vector(CIExyY[2] * CIExyY[0] / CIExyY[1],
                 CIExyY[2],
                 CIExyY[2] * (1.0 - CIExyY[0] - CIExyY[1]) / CIExyY[1]);
}

vector get_illuminant_CIExyY(string illuminant)
{
    vector white_CIExyY;

    if (illuminant == "D50")
    {
        white_CIExyY = vector(D50_WHITEPOINT_CIE1931_2DEG_xy, 1.0);
    }
    else if (illuminant == "D55")
    {
        white_CIExyY = vector(D55_WHITEPOINT_CIE1931_2DEG_xy, 1.0);
    }
    else if (illuminant == "D60")
    {
        white_CIExyY = vector(D60_WHITEPOINT_CIE1931_2DEG_xy, 1.0);
    }
    else if (illuminant == "D65")
    {
        white_CIExyY = vector(D65_WHITEPOINT_CIE1931_2DEG_xy, 1.0);
    }
    else if (illuminant == "D75")
    {
        white_CIExyY = vector(D75_WHITEPOINT_CIE1931_2DEG_xy, 1.0);
    }
    else if (illuminant == "DCIP3")
    {
        white_CIExyY = vector(DCIP3_WHITEPOINT_CIE1931_2DEG_xy, 1.0);
    }
    else
    {
        white_CIExyY = vector(0);
    }
    return white_CIExyY;
}

void get_illuminant_CIExy(string illuminant, output float CIExy[2])
{
    vector white_CIExyY = get_illuminant_CIExyY(illuminant);
    CIExy[0] = white_CIExyY[0];
    CIExy[1] = white_CIExyY[1];
}

vector get_illuminant_CIExyz(string illuminant)
{
    vector white_CIExyY = get_illuminant_CIExyY(illuminant);
    return transform_CIExy_to_CIExyz(white_CIExyY[0], white_CIExyY[1]);
}

vector get_illuminant_CIExyz(string illuminant, float white_CIExy[2])
{
    return (illuminant != "custom")
        ? get_illuminant_CIExyz(illuminant)
        : transform_CIExy_to_CIExyz(white_CIExy);
}


//
// Reference:
//
//      Colour Space Conversions
//
//      http://www.poynton.com/PDFs/coloureq.pdf
//      http://www.brucelindbloom.com/index.html?Eqn_XYZ_to_xyY.html
//

vector transform_CIEXYZ_to_CIExyY(color XYZ, float white_CIExy[2])
{
    vector CIExyY;

    if (XYZ != 0)
    {
        float XYZ_sum = XYZ[0] + XYZ[1] + XYZ[2];
        CIExyY = vector(XYZ[0] / XYZ_sum, XYZ[1] / XYZ_sum, XYZ[1]);
    }
    else
    {
        CIExyY = vector(white_CIExy[0], white_CIExy[1], XYZ[1]);
    }
    return CIExyY;
}

vector transform_CIEXYZ_to_CIExyY(color XYZ, string illuminant)
{
    float white_CIExy[2];

    get_illuminant_CIExy(illuminant, white_CIExy);

    return transform_CIEXYZ_to_CIExyY(XYZ, white_CIExy);
}


//
// Reference:
//
//      xyY to XYZ conversion
//
//      http://www.brucelindbloom.com/index.html?Eqn_xyY_to_XYZ.html
//

color transform_CIExyY_to_CIEXYZ(vector CIExyY)
{
    return (CIExyY[1] == 0.0)
        ? color(0)
        : color(CIExyY[0] * CIExyY[2] / CIExyY[1],
                CIExyY[2],
                (1.0 - CIExyY[0] - CIExyY[1]) * CIExyY[2] / CIExyY[1]);
}

color transform_CIExy_to_CIEXYZ(float CIExy[2])
{
    return transform_CIExyY_to_CIEXYZ(vector(CIExy[0], CIExy[1], 1.0));
}

color get_illuminant_CIEXYZ(string illuminant)
{
    return transform_CIExyY_to_CIEXYZ(get_illuminant_CIExyY(illuminant));
}


//  Reference:
//
//      http://www.brucelindbloom.com/index.html?Eqn_RGB_XYZ_Matrix.html
//
//      Precomputed RGB<>CIEXYZ matrices data created with Colour,
//      http://colour-science.org/
//

void get_RGB_to_XYZ_matrix(
    string color_space,
    output vector RGB_to_XYZ[3],
    output string source_illuminant)
{
    if (color_space == "ACES" || color_space == "ACES2065-1")
    {
        source_illuminant = "D60";

        RGB_to_XYZ[0] = vector(RGB_TO_XYZ_ACES_R0);
        RGB_to_XYZ[1] = vector(RGB_TO_XYZ_ACES_R1);
        RGB_to_XYZ[2] = vector(RGB_TO_XYZ_ACES_R2);
    }
    else if (color_space == "ACEScg")
    {
        source_illuminant = "D60";

        RGB_to_XYZ[0] = vector(RGB_TO_XYZ_ACESCG_R0);
        RGB_to_XYZ[1] = vector(RGB_TO_XYZ_ACESCG_R1);
        RGB_to_XYZ[2] = vector(RGB_TO_XYZ_ACESCG_R2);
    }
    else if (color_space == "AdobeRGB")
    {
        source_illuminant = "D65";

        RGB_to_XYZ[0] = vector(RGB_TO_XYZ_ADOBERGB_R0);
        RGB_to_XYZ[1] = vector(RGB_TO_XYZ_ADOBERGB_R1);
        RGB_to_XYZ[2] = vector(RGB_TO_XYZ_ADOBERGB_R2);
    }
    else if (color_space == "DCIP3")
    {
        source_illuminant = "DCIP3";

        RGB_to_XYZ[0] = vector(RGB_TO_XYZ_DCIP3_R0);
        RGB_to_XYZ[1] = vector(RGB_TO_XYZ_DCIP3_R1);
        RGB_to_XYZ[2] = vector(RGB_TO_XYZ_DCIP3_R2);
    }
    else if (color_space == "Rec.2020" ||
             color_space == "scene-linear Rec 2020")
    {
        source_illuminant = "D65";

        RGB_to_XYZ[0] = vector(RGB_TO_XYZ_REC2020_R0);
        RGB_to_XYZ[1] = vector(RGB_TO_XYZ_REC2020_R1);
        RGB_to_XYZ[2] = vector(RGB_TO_XYZ_REC2020_R2);
    }
    else if (color_space == "Rec.709" ||
             color_space == "scene-linear Rec 709/sRGB")
    {
        source_illuminant = "D65";

        RGB_to_XYZ[0] = vector(RGB_TO_XYZ_REC709_R0);
        RGB_to_XYZ[1] = vector(RGB_TO_XYZ_REC709_R1);
        RGB_to_XYZ[2] = vector(RGB_TO_XYZ_REC709_R2);
    }
    else
    {
        return; // invalid color space, this will fail
    }
}    


//
//  Reference:
//
//      http://www.brucelindbloom.com/index.html?Eqn_RGB_XYZ_Matrix.html
//
//      Precomputed RGB<>CIEXYZ matrices data created with Colour,
//      http://colour-science.org/
//

void get_XYZ_to_RGB_matrix(
    string color_space,
    output vector XYZ_to_RGB[3],
    output string source_illuminant)
{
    if (color_space == "ACES" || color_space == "ACES2065-1")
    {
        source_illuminant = "D60";

        XYZ_to_RGB[0] = vector(XYZ_TO_RGB_ACES_R0);
        XYZ_to_RGB[1] = vector(XYZ_TO_RGB_ACES_R1);
        XYZ_to_RGB[2] = vector(XYZ_TO_RGB_ACES_R2);
    }
    else if (color_space == "ACEScg")
    {
        source_illuminant = "D60";

        XYZ_to_RGB[0] = vector(XYZ_TO_RGB_ACESCG_R0);
        XYZ_to_RGB[1] = vector(XYZ_TO_RGB_ACESCG_R1);
        XYZ_to_RGB[2] = vector(XYZ_TO_RGB_ACESCG_R2);
    }
    else if (color_space == "AdobeRGB")
    {
        source_illuminant = "D65";

        XYZ_to_RGB[0] = vector(XYZ_TO_RGB_ADOBERGB_R0);
        XYZ_to_RGB[1] = vector(XYZ_TO_RGB_ADOBERGB_R1);
        XYZ_to_RGB[2] = vector(XYZ_TO_RGB_ADOBERGB_R2);
    }
    else if (color_space == "DCIP3")
    {
        source_illuminant = "DCIP3";

        XYZ_to_RGB[0] = vector(XYZ_TO_RGB_DCIP3_R0);
        XYZ_to_RGB[1] = vector(XYZ_TO_RGB_DCIP3_R1);
        XYZ_to_RGB[2] = vector(XYZ_TO_RGB_DCIP3_R2);
    }
    else if (color_space == "Rec.2020" ||
             color_space == "scene-linear Rec 2020")
    {
        source_illuminant = "D65";

        XYZ_to_RGB[0] = vector(XYZ_TO_RGB_REC2020_R0);
        XYZ_to_RGB[1] = vector(XYZ_TO_RGB_REC2020_R1);
        XYZ_to_RGB[2] = vector(XYZ_TO_RGB_REC2020_R2);
    }
    else if (color_space == "Rec.709" ||
             color_space == "scene-linear Rec 709/sRGB")
    {
        source_illuminant = "D65";

        XYZ_to_RGB[0] = vector(XYZ_TO_RGB_REC709_R0);
        XYZ_to_RGB[1] = vector(XYZ_TO_RGB_REC709_R1);
        XYZ_to_RGB[2] = vector(XYZ_TO_RGB_REC709_R2);
    }
    else
    {
        return; // invalid color space, this will fail
    }
}


//
// Reference:
//
//      RGB to XYZ conversion, and chromatic adaptation
//
//      http://www.brucelindbloom.com/index.html?Eqn_RGB_to_XYZ.html
//      http://www.brucelindbloom.com/index.html?Eqn_ChromAdapt.html
//      http://colour-science.org/
//

color transform_linear_RGB_to_CIEXYZ(
    color linear_RGB_color,
    string color_space,
    string target_illuminant,
    string CAT)
{
    string source_illuminant = "";
    vector source_RGB_to_CIEXYZ[3];

    get_RGB_to_XYZ_matrix(
        color_space,
        source_RGB_to_CIEXYZ,
        source_illuminant);

    color CIEXYZ = color(
        dot(source_RGB_to_CIEXYZ[0], (vector) linear_RGB_color),
        dot(source_RGB_to_CIEXYZ[1], (vector) linear_RGB_color),
        dot(source_RGB_to_CIEXYZ[2], (vector) linear_RGB_color));

    if (source_illuminant != "" && source_illuminant != target_illuminant)
    {
        color source_white_XYZ = get_illuminant_CIEXYZ(source_illuminant);
        color target_white_XYZ = get_illuminant_CIEXYZ(target_illuminant);

        vector CAT_matrix[3];

        chromatic_adaptation_vonKries(
            source_white_XYZ,
            target_white_XYZ,
            CAT,
            CAT_matrix);

        color adapted_CIEXYZ = color(
            dot(CAT_matrix[0], (vector) CIEXYZ),
            dot(CAT_matrix[1], (vector) CIEXYZ),
            dot(CAT_matrix[2], (vector) CIEXYZ));

        CIEXYZ = adapted_CIEXYZ;
    }
    return max(0.0, CIEXYZ);
}

color transform_linear_RGB_to_CIEXYZ(
    color linear_RGB_color,
    string color_space,
    string target_illuminant)
{
    return transform_linear_RGB_to_CIEXYZ(
        linear_RGB_color,
        color_space,
        target_illuminant,
        "Bradford");
}


//
// Reference:
//
//      XYZ to RGB conversion, with choice of chromatic adaptation transform.
//
//      http://www.brucelindbloom.com/index.html?Eqn_XYZ_to_RGB.html
//      http://www.brucelindbloom.com/Eqn_ChromAdapt.html
//      http://colour-science.org/
//

color transform_CIEXYZ_to_linear_RGB(
    color linear_XYZ_color,
    string color_space,
    string target_illuminant,
    string CAT)
{
    string source_illuminant = "";
    vector source_XYZ_to_RGB[3];

    get_XYZ_to_RGB_matrix(
        color_space,
        source_XYZ_to_RGB,
        source_illuminant);

    color CIEXYZ = linear_XYZ_color;

    if (source_illuminant != "" && source_illuminant != target_illuminant)
    {
        color source_white_XYZ = get_illuminant_CIEXYZ(source_illuminant);
        color target_white_XYZ = get_illuminant_CIEXYZ(target_illuminant);

        vector CAT_matrix[3];

        chromatic_adaptation_vonKries(
            source_white_XYZ,
            target_white_XYZ,
            CAT,
            CAT_matrix);

        color adapted_CIEXYZ = color(
            dot(CAT_matrix[0], (vector) CIEXYZ),
            dot(CAT_matrix[1], (vector) CIEXYZ),
            dot(CAT_matrix[2], (vector) CIEXYZ));

        CIEXYZ = adapted_CIEXYZ;
    }

    color linear_RGB = color(
        dot(source_XYZ_to_RGB[0], CIEXYZ),
        dot(source_XYZ_to_RGB[1], CIEXYZ),
        dot(source_XYZ_to_RGB[2], CIEXYZ));

    return max(0.0, linear_RGB);
}

color transform_CIEXYZ_to_linear_RGB(
    color linear_XYZ_color,
    string color_space,
    string target_illuminant)
{
    return transform_CIEXYZ_to_linear_RGB(
        linear_XYZ_color,
        color_space,
        target_illuminant,
        "Bradford");
}


//
// Overloaded RGB<>XYZ transformations, assuming identical white points.
//

color transform_linear_RGB_to_CIEXYZ(
    color linear_RGB_color,
    string color_space)
{
    string source_illuminant = "";
    vector source_RGB_to_XYZ[3];

    get_RGB_to_XYZ_matrix(
        color_space,
        source_RGB_to_XYZ,
        source_illuminant);

    color CIEXYZ = color(
        dot(source_RGB_to_XYZ[0], (vector) linear_RGB_color),
        dot(source_RGB_to_XYZ[1], (vector) linear_RGB_color),
        dot(source_RGB_to_XYZ[2], (vector) linear_RGB_color));

    return max(0.0, CIEXYZ);
}

color transform_CIEXYZ_to_linear_RGB(
    color linear_XYZ_color,
    string color_space)
{
    string source_illuminant = "";
    vector source_XYZ_to_RGB[3];

    get_XYZ_to_RGB_matrix(
        color_space,
        source_XYZ_to_RGB,
        source_illuminant);

    color linear_RGB = color(
        dot(source_XYZ_to_RGB[0], (vector) linear_XYZ_color),
        dot(source_XYZ_to_RGB[1], (vector) linear_XYZ_color),
        dot(source_XYZ_to_RGB[2], (vector) linear_XYZ_color));

    return max(0.0, linear_RGB);
}


//
// Create a RGB->XYZ transformation matrix, given the set of RGB primaries
// and white point CIE xy chromaticity coordinates.
//

matrix create_RGB_to_XYZ_matrix(
    float R_CIExy[2],
    float G_CIExy[2],
    float B_CIExy[2],
    float W_CIExy[2],
    float target_W_CIExy[2],
    string CAT)
{
    vector R_CIEXYZ = transform_CIExy_to_CIEXYZ(R_CIExy);
    vector G_CIEXYZ = transform_CIExy_to_CIEXYZ(G_CIExy);
    vector B_CIEXYZ = transform_CIExy_to_CIEXYZ(B_CIExy);

    // If the source & target wp differ, prepare for chromatic adaptation.
    
    vector source_W_CIEXYZ = transform_CIExy_to_CIEXYZ(W_CIExy);
    vector target_W_CIEXYZ = transform_CIExy_to_CIEXYZ(target_W_CIExy);
    
    matrix CAT_matrix = 
        chromatic_adaptation_vonKries(
            source_W_CIEXYZ,
            target_W_CIEXYZ,
            CAT);

    vector W_CIEXYZ = source_W_CIEXYZ;

    matrix invXYZ = inverse(matrix(
        R_CIEXYZ[0], G_CIEXYZ[0], B_CIEXYZ[0], 0.0,
        R_CIEXYZ[1], G_CIEXYZ[1], B_CIEXYZ[1], 0.0,
        R_CIEXYZ[2], G_CIEXYZ[2], B_CIEXYZ[2], 0.0,
        0.0, 0.0, 0.0, 1.0));
    
    vector XrXgXb = vector(invXYZ[0][0], invXYZ[0][1], invXYZ[0][2]);
    vector YrYgYb = vector(invXYZ[1][0], invXYZ[1][1], invXYZ[1][2]);
    vector ZrZgZb = vector(invXYZ[2][0], invXYZ[2][1], invXYZ[2][2]);

    vector S = vector(
        dot(XrXgXb, source_W_CIEXYZ),
        dot(YrYgYb, source_W_CIEXYZ),
        dot(ZrZgZb, source_W_CIEXYZ));

    // Non chromatically adapted RGB->XYZ matrix is M.
    matrix M = matrix(
        S[0] * R_CIEXYZ[0], S[1] * G_CIEXYZ[0], S[2] * B_CIEXYZ[0], 0.0,
        S[0] * R_CIEXYZ[1], S[1] * G_CIEXYZ[1], S[2] * B_CIEXYZ[1], 0.0,
        S[0] * R_CIEXYZ[2], S[1] * G_CIEXYZ[2], S[2] * B_CIEXYZ[2], 0.0,
        0.0, 0.0, 0.0, 1.0);

#ifdef DEBUG
    warning("[DEBUG]:\nRGB2XYZ = %.5f\n\tAdapted RGB2XYZ = %.5f\n",
            M, CAT_matrix * M);
#endif
    
    return CAT_matrix * M;
}    


//
// Reference:
//
//      "Design of Advanced Color Temperature Control Systems for HTDV
//      Applications"
//      Bongsoon Kang, Ohak Moon, Changhee Hong, Honam Lee, Bonghwan Cho,
//      Youngsun Kim
//
//      http://www.jkps.or.kr/journal/download_pdf.php?spage=865&volume=41&number=6
//

void get_CIExy_from_CCT_Kang(int CCT, output float CIExy[2])
{
    float x, y;

    if (CCT >= 1667 && CCT <= 4000)
    {
        // Not enough precision otherwise, so explicit powers used.

        x = -0.2661239 * pow(10.0, 9.0) / pow(CCT, 3.0) -
             0.2343589 * pow(10.0, 6.0) / pow(CCT, 2.0) +
             0.8776596 * pow(10.0, 3.0) / CCT +
             0.179910;

        if (CCT <= 2222)
        {
            y = -1.10638140 * pow(x, 3.0) -
                 1.34811020 * pow(x, 2.0) +
                 2.18555832 * x -
                 0.20219683;
        }
        else
        {
            y = -0.95494760 * pow(x, 3.0) -
                 1.37418593 * pow(x, 2.0) +
                 2.09137015 * x -
                 0.16748867;
        }
    }
    else if (CCT > 4000 && CCT < 25000)
    {
        x = -3.0258469 * pow(10.0, 9.0) / pow(CCT, 3.0) +
             2.1070379 * pow(10.0, 6.0) / pow(CCT, 2.0) +
             0.2226347 * pow(10.0, 3.0) / CCT +
             0.24039;

        y = 3.08175800 * pow(x, 3.0) -
            5.87338670 * pow(x, 2.0) +
            3.75112997 * x -
            0.37001483;
    }
    else
    {
        x = y = 0.0; // CCT out of range
    }
    CIExy[0] = x;
    CIExy[1] = y;
}


//
// Reference:
//
//      XYZ to CIE 1976 L*a*b* | CIELAB equations
//      Colorimetry - Part 4: CIE 1976 L*a*b* colour space
//
//      http://www.brucelindbloom.com/index.html?Eqn_XYZ_to_Lab.html
//      http://www.brucelindbloom.com/LContinuity.html
//      http://cie.co.at/index.php?i_ca_id=485
//
//      Lightness in [0,100], a*,b* in [-100,100] ranges
//

color transform_CIEXYZ_to_CIELAB(
    color linear_XYZ_color,
    vector reference_white_CIExyY)
{
    color white_XYZ = transform_CIExyY_to_CIEXYZ(reference_white_CIExyY);
    color XYZ_f = linear_XYZ_color / white_XYZ, CIEXYZ;

    for (int i = 0; i < 3; ++i)
    {
        CIEXYZ[i] = (XYZ_f[i] > CIE_E)
            ? pow(XYZ_f[i], 1.0 / 3.0)
            : (CIE_K * XYZ_f[i] + 16.0) / 116.0;
    }

    float L = 116.0 * CIEXYZ[1] - 16.0;
    float a = 500.0 * (CIEXYZ[0] - CIEXYZ[1]);
    float b = 200.0 * (CIEXYZ[1] - CIEXYZ[2]);

    return color(L, a, b);
}

color transform_CIEXYZ_to_CIELAB(
    color linear_XYZ_color,
    float reference_white_CIExy[2])
{
    return transform_CIEXYZ_to_CIELAB(
        linear_XYZ_color,
        vector(reference_white_CIExy[0], reference_white_CIExy[1], 1.0));
}

color transform_CIEXYZ_to_CIELAB(
    color linear_XYZ_color,
    string illuminant)
{
    return transform_CIEXYZ_to_CIELAB(
        linear_XYZ_color,
        get_illuminant_CIExyY(illuminant));
}


//
// Reference:
//
//      CIE L*a*b* 1976 | CIELAB to XYZ
//      Colorimetry - Part 4: CIE 1976 L*a*b* colour space
//
//      http://www.brucelindbloom.com/index.html?Eqn_Lab_to_XYZ.html
//      http://cie.co.at/index.php?i_ca_id=485
//
//      Lightness in [0,100], a,b in [-100,100] domains
//

color transform_CIELAB_to_CIEXYZ(
    color CIELAB,
    color reference_white_CIExyY)
{
    color white_CIEXYZ = transform_CIExyY_to_CIEXYZ(reference_white_CIExyY);

    float f_Y = (CIELAB[0] + 16.0) / 116.0;
    float f_X = CIELAB[1] / 500.0 + f_Y;
    float f_Z = f_Y - CIELAB[2] / 200.0;

    float X_r = (pow(f_X, 3.0) > CIE_E)
        ? pow(f_X, 3.0)
        : (116.0 * f_X - 16.0) / CIE_K;

    float Y_r = (CIELAB[0] > CIE_K * CIE_E)
        ? pow((CIELAB[0] + 16.0) / 116.0, 3.0)
        : CIELAB[0] / CIE_K;

    float Z_r = (pow(f_Z, 3.0) > CIE_E)
        ? pow(f_Z, 3.0)
        : (116.0 * f_Z - 16.0) / CIE_K;

    return color(X_r, Y_r, Z_r) * white_CIEXYZ;
}

color transform_CIELAB_to_CIEXYZ(
    color CIELAB,
    float reference_white_CIExy[2])
{
    return transform_CIELAB_to_CIEXYZ(
        CIELAB,
        vector(reference_white_CIExy[0], reference_white_CIExy[1], 1.0));
}

color transform_CIELAB_to_CIEXYZ(
    color CIELAB,
    string illuminant)
{
    return transform_CIELAB_to_CIEXYZ(
        CIELAB,
        get_illuminant_CIExyY(illuminant));
}


//
// Reference:
//
//      XYZ to CIE 1976 L*u*v* | CIELUV
//      Colorimetry - Part 5: CIE 1976 L*u*v* colour space and u',v'
//      Uniform Chromaticity Scale Diagram
//
//      http://www.brucelindbloom.com/index.html?Eqn_XYZ_to_Luv.html
//      http://www.brucelindbloom.com/LContinuity.html
//      http://hsevi.ir/RI_Standard/File/987
//
//      Lightness in [0,100], u,v in [-100,100] ranges
//

color transform_CIEXYZ_to_CIELUV(
    color linear_XYZ_color,
    vector reference_white_CIExyY)
{
    color white_CIEXYZ = transform_CIExyY_to_CIEXYZ(reference_white_CIExyY);

    float y_r = linear_XYZ_color[1] / white_CIEXYZ[1];

    float denom = linear_XYZ_color[0] + 15 *
                  linear_XYZ_color[1] + 3 *
                  linear_XYZ_color[2];

    float denom_r = white_CIEXYZ[0] + 15 *
                    white_CIEXYZ[1] + 3 * white_CIEXYZ[2];

    float u_prime = 4 * linear_XYZ_color[0] / denom;
    float v_prime = 9 * linear_XYZ_color[1] / denom;

    float u_prime_r = 4 * white_CIEXYZ[0] / denom_r;
    float v_prime_r = 9 * white_CIEXYZ[1] / denom_r;

    float L = (y_r > CIE_E)
        ? 116 * pow(y_r, 1.0 / 3.0) - 16
        : CIE_K * y_r;

    float Luv_u = 13 * L * (u_prime - u_prime_r);
    float Luv_v = 13 * L * (v_prime - v_prime_r);

    return color(L, Luv_u, Luv_v);
}

color transform_CIEXYZ_to_CIELUV(
    color linear_XYZ_color,
    float reference_white_CIExy[2])
{
    return transform_CIEXYZ_to_CIELUV(
        linear_XYZ_color,
        vector(reference_white_CIExy[0], reference_white_CIExy[1], 1.0));
}

color transform_CIEXYZ_to_CIELUV(
    color linear_XYZ_color,
    string illuminant)
{
    return transform_CIEXYZ_to_CIELUV(
        linear_XYZ_color,
        get_illuminant_CIExyY(illuminant));
}


//
// Reference:
//
//      CIE 1976 L*u*v* | CIELUV to XYZ
//      Colorimetry - Part 5: CIE 1976 L*u*v* colour space and u',v'
//      Uniform Chromaticity Scale Diagram
//
//      http://www.brucelindbloom.com/index.html?Eqn_XYZ_to_Luv.html
//      http://www.brucelindbloom.com/LContinuity.html
//      http://hsevi.ir/RI_Standard/File/987
//
//      Lightness in [0,100], u,v in [-100,100] domains
//

color transform_CIELUV_to_CIEXYZ(
    color CIELUV,
    vector reference_white_CIExyY)
{
    color white_CIEXYZ = transform_CIExyY_to_CIEXYZ(reference_white_CIExyY);

    float denom_r = white_CIEXYZ[0] + 15 *
                    white_CIEXYZ[1] + 3 *
                    white_CIEXYZ[2];

    float u0 = 4 * white_CIEXYZ[0] / denom_r;
    float v0 = 9 * white_CIEXYZ[1] / denom_r;

    float Y = (CIELUV[0] > CIE_K * CIE_E)
        ? pow((CIELUV[0] + 16) / 116, 3.0)
        : CIELUV[0] / CIE_K;

    float a = (52 * CIELUV[0] / (CIELUV[1] + 13 * CIELUV[0] * u0) - 1) / 3.0;
    float b = -5 * Y;
    float c = -1.0 / 3.0;
    float d = Y * (39 * CIELUV[0] / (CIELUV[2] + 13 * CIELUV[0] * v0) - 5);

    float X = (d - b) / (a - c);
    float Z = X * a + b;

    return color(X, Y, Z);
}

color transform_CIELUV_to_CIEXYZ(
    color CIELUV,
    float reference_white_CIExy[2])
{
    return transform_CIELUV_to_CIEXYZ(
        CIELUV,
        vector(reference_white_CIExy[0], reference_white_CIExy[1], 1.0));
}

color transform_CIELUV_to_CIEXYZ(
    color CIELUV,
    string illuminant)
{
    return transform_CIELUV_to_CIEXYZ(
        CIELUV,
        get_illuminant_CIExyY(illuminant));
}


//
// Reference:
//
//      CIE 1976 L*a*b* to LCh_ab equations
//      Colorimetry - Part 4: CIE 1976 L*a*b* colour space
//
//      http://www.brucelindbloom.com/index.html?Eqn_Lab_to_LCH.html
//      http://www.brucelindbloom.com/index.html?Eqn_XYZ_to_Luv.html
//      http://cie.co.at/index.php?i_ca_id=485
//
//      Lightness in [0,100], Chroma in [0,100], hue angle in [0,360] degrees
//      ranges.
//

color transform_CIELAB_to_CIELCh_ab(color CIELAB)
{
    float L = CIELAB[0];
    float C = hypot(CIELAB[1], CIELAB[2]);
    float h = mod(degrees(atan2(CIELAB[2], CIELAB[1])), 360);

    return color(L, C, h);
}

color transform_CIELCh_ab_to_CIELAB(color CIELCh_ab)
{
    float L = CIELCh_ab[0], a, b;

    sincos(radians(CIELCh_ab[2]), b, a);

    return color(L, CIELCh_ab[1] * a, CIELCh_ab[1] * b);
}

color transform_linear_RGB_to_CIELAB(
    color linear_RGB,
    string color_space,
    string target_illuminant)
{
    color CIEXYZ = transform_linear_RGB_to_CIEXYZ(
        linear_RGB,
        color_space,
        target_illuminant);

    return transform_CIEXYZ_to_CIELAB(CIEXYZ, target_illuminant);
}

color transform_CIELAB_to_linear_RGB(
    color CIELAB,
    string color_space,
    string target_illuminant)
{
    color CIEXYZ = transform_CIELAB_to_CIEXYZ(
        CIELAB,
        target_illuminant);

    return transform_CIEXYZ_to_linear_RGB(
        CIEXYZ,
        color_space,
        target_illuminant);
}

color transform_linear_RGB_to_CIELCh_ab(
    color linear_RGB,
    string color_space,
    string target_illuminant)
{
    color CIELAB = transform_linear_RGB_to_CIELAB(
        linear_RGB,
        color_space,
        target_illuminant);

    return transform_CIELAB_to_CIELCh_ab(CIELAB);
}

color transform_CIELCh_ab_to_linear_RGB(
    color CIELCh_ab,
    string color_space,
    string target_illuminant)
{
    color CIELAB = transform_CIELCh_ab_to_CIELAB(CIELCh_ab);

    return transform_CIELAB_to_linear_RGB(
        CIELAB,
        color_space,
        target_illuminant);
}


//
// Reference:
//
//      CIE 1976 L*u*v* to LCh_uv equations
//      Colorimetry - Part 5: CIE 1976 L*u*v* colour space and u',v'
//      Uniform Chromaticity Scale Diagram
//
//      http://www.brucelindbloom.com/index.html?Eqn_Luv_to_LCH.html
//      http://hsevi.ir/RI_Standard/File/987
//
//      Lightness in [0,100], Chroma in [0,100], hue angle in [0,360] degrees
//      ranges
// 

color transform_CIELUV_to_CIELCh_uv(color CIELUV)
{
    float L = CIELUV[0];
    float C = hypot(CIELUV[1], CIELUV[2]);
    float h = mod(degrees(atan2(CIELUV[2], CIELUV[1])), 360);

    return color(L, C, h);
}

color transform_CIELCh_uv_to_CIELUV(color CIELCh_uv)
{
    float L = CIELCh_uv[0], uu, vv;

    sincos(radians(CIELCh_uv[2]), vv, uu);

    return color(CIELCh_uv[0], CIELCh_uv[1] * uu, CIELCh_uv[1] * vv);
}

color transform_linear_RGB_to_CIELUV(
    color linear_RGB,
    string color_space,
    string target_illuminant)
{
    color CIEXYZ = transform_linear_RGB_to_CIEXYZ(
        linear_RGB,
        color_space,
        target_illuminant);

    return transform_CIEXYZ_to_CIELUV(
        CIEXYZ,
        target_illuminant);
}

color transform_CIELUV_to_linear_RGB(
    color CIELUV,
    string color_space,
    string target_illuminant)
{
    color CIEXYZ = transform_CIELUV_to_CIEXYZ(CIELUV, target_illuminant);

    return transform_CIEXYZ_to_linear_RGB(
        CIEXYZ,
        color_space,
        target_illuminant);
}

color transform_linear_RGB_to_CIELCh_uv(
    color linear_RGB,
    string color_space,
    string target_illuminant)
{
    color CIELUV = transform_linear_RGB_to_CIELUV(
        linear_RGB,
        color_space,
        target_illuminant);

    return transform_CIELUV_to_CIELCh_uv(CIELUV);
}

color transform_CIELCh_uv_to_linear_RGB(
    color CIELCh_uv,
    string color_space,
    string target_illuminant)
{
    color CIELUV = transform_CIELCh_uv_to_CIELUV(CIELCh_uv);

    return transform_CIELUV_to_linear_RGB(
        CIELUV,
        color_space,
        target_illuminant);
}


//
// CIELAB, CIELUV, have values in range L [0,100], a,b [-128,128]
// and u,v ([-134,220],[-140,122])
//

color remap_CIELAB(color CIELAB)
{
    float L = CIELAB[0] / 100.0;
    float a = (CIELAB[1] + 128.0) / 256.0;
    float b = (CIELAB[2] + 128.0) / 256.0;

    return color(L, a, b);
}

color remap_CIELUV(color CIELUV)
{
    float L = CIELUV[0] / 100.0;
    float u = (CIELUV[1] + 134.0) / 352.0;
    float v = (CIELUV[2] + 140.0) / 262.0;

    return color(L, u, v);
}

color inverse_remap_CIELAB(color CIELAB)
{
    float L = CIELAB[0] * 100.0;
    float a = CIELAB[1] * 256.0 - 128.0;
    float b = CIELAB[2] * 256.0 - 128.0;

    return color(L, a, b);
}

color inverse_remap_CIELUV(color CIELUV)
{
    float L = CIELUV[0] * 100.0;
    float u = CIELUV[1] * 352.0 - 134.0;
    float v = CIELUV[2] * 262.0 - 140.0;

    return color(L, u, v);
}


//
// LCh (ab, uv), has values L in [0,100] range, chroma in [0,100] range
// and hue in [0,360] degrees range.
//

color remap_CIELCh(color CIELCh)
{
    float L = CIELCh[0] * 0.01;
    float C = CIELCh[1] * 0.01;
    float h = CIELCh[2] / 360;

    return color(L, C, h);
}

color inverse_remap_CIELCh(color CIELCh)
{
    float L = CIELCh[0] * 100.0;
    float C = CIELCh[1] * 100.0;
    float h = CIELCh[2] * 360;

    return color(L, C, h);
}


//
// Reference:
//
//      Handbook of Digital Image Synthesis: Scientific Foundations of
//      Rendering, Vincent Pegoraro, CRC Press, 2016
//      ISBN 1315395215, 9781315395210
//
// Note: hue values are in [0,1], instead of [0,360] degrees
//

float get_hue_angle(color C, float value, float rgbmin, float chroma)
{
    float hue;

    if (C[0] == value)
    {
        hue = mod((C[1] - C[2]) / chroma, 6.0);
    }
    else if (C[1] == value)
    {
        hue = (C[2] - C[0]) / chroma + 2.0;
    }
    else
    {
        hue = (C[0] - C[1]) / chroma + 4.0;
    }
    return mod(hue / 6.0, 1.0);
}

float hue_to_rgb(float v1, float v2, float vH)
{
    float vh = mod(vH, 1.0), out = 0.0;

    if (vh * 6.0 < 1.0)
    {
        out = v1 + (v2 - v1) * 6.0 * vh;
    }
    else if (vh * 2.0 < 1.0)
    {
        out = v2;
    }
    else if (vh * 3.0 < 2.0)
    {
        out = v1 + (v2 - v1) * ((2.0 / 3.0) - vh) * 6.0;
    }
    else
    {
        out = v1;
    }
    return out;
}

color transform_RGB_to_HSV(color RGB)
{
    float value = max(RGB[0], max(RGB[1], RGB[2]));

    if (value <= 0.0)
    {
        return color(0); // black
    }

    float rgbmin = min(RGB[0], min(RGB[1], RGB[2]));
    float chroma = value - rgbmin;

    if (chroma <= 0.005) // chroma/value explodes otherwise with low V
    {
        return color(0.0, 0.0, value); // greyscale
    }

    float hue = get_hue_angle(RGB, value, rgbmin, chroma);
    float saturation = chroma / value;

    return color(hue, saturation, value);
}

color transform_RGB_to_HSL(color RGB)
{
    float value = max(RGB[0], max(RGB[1], RGB[2]));

    if (value <= 0.0)
    {
        return color(0); // black
    }

    float rgbmin = min(RGB[0], min(RGB[1], RGB[2]));
    float chroma = value - rgbmin;
    float lightness = (value + rgbmin) / 2.0;

    if (chroma <= 0.005 || lightness == 1.0) // chroma explodes with low V
    {
       return color(0.0, 0.0, lightness);
    }

    float saturation = chroma / (1.0 - abs(2.0 * lightness - 1.0));
    float hue = get_hue_angle(RGB, value, rgbmin, chroma);

    return color(hue, saturation, lightness);
}

color transform_HSV_to_RGB(color HSV)
{
    float hue = HSV[0], saturation = HSV[1], value = HSV[2];

    if (value <= 0.0)
    {
        return color(0);
    }

    if (saturation <= 0.0)
    {
        return color(value); // grey
    }

    float sector = hue * 6.0, r, g, b;

    int sextant = (int) floor(sector);
    float fract = sector - (float) sextant;

    float p = value * (1.0 - saturation);
    float q = value * (1.0 - saturation * fract);
    float t = value * (1.0 - saturation * (1.0 - fract));

    if (sextant == 0)
    {
        r = value;
        g = t;
        b = p;
    }
    else if (sextant == 1)
    {
        r = q;
        g = value;
        b = p;
    }
    else if (sextant == 2)
    {
        r = p;
        g = value;
        b = t;
    }
    else if (sextant == 3)
    {
        r = p;
        g = q;
        b = value;
    }
    else if (sextant == 4)
    {
        r = t;
        g = p;
        b = value;
    }
    else
    {
        r = value;
        g = p;
        b = q;
    }

    return color(r, g, b);
}

color transform_HSL_to_RGB(color HSL)
{
    float hue = HSL[0], saturation = HSL[1], lightness = HSL[2];

    if (lightness <= 0.0)
    {
        return color(0);
    }

    if (saturation <= 0.0)
    {
        return color(lightness);
    }

    float v2 = (lightness < 0.5)
        ? lightness * (1.0 + saturation)
        : (lightness + saturation) - (saturation * lightness);

    float v1 = 2.0 * lightness - v2;

    float r = hue_to_rgb(v1, v2, hue + (1.0 / 3.0));
    float g = hue_to_rgb(v1, v2, hue);
    float b = hue_to_rgb(v1, v2, hue - (1.0 / 3.0));

    return color(r, g, b);
}


//
// Reference:
//
//      Delta E (CIEDE2000)
//
//      https://en.wikipedia.org/wiki/Color_difference
//      http://www.brucelindbloom.com/index.html?Eqn_DeltaE_CIE2000.html
//      https://zschuessler.github.io/DeltaE/learn/
//
//      The CIEDE2000 Color-Difference Formula: Implementation Notes,
//      Supplementary Test Data, and Mathematical Observations
//
//      http://www.ece.rochester.edu/~gsharma/ciede2000/ciede2000noteCRNA.pdf
//
// Note. It's unlikely of use to the end-user, but it's handy for us.
//

float deltaE_CIEDE2000(
    color   reference_CIELAB,
    color   sampleval_CIELAB)
{
    float reference_L = reference_CIELAB[0];
    float reference_a = reference_CIELAB[1];
    float reference_b = reference_CIELAB[2];

    float sampleval_L = sampleval_CIELAB[0];
    float sampleval_a = sampleval_CIELAB[1];
    float sampleval_b = sampleval_CIELAB[2];


    float reference_C = hypot(reference_a, reference_b);
    float sampleval_C = hypot(sampleval_a, sampleval_b);

    float C_bar = (reference_C + sampleval_C) / 2;
    float C_7 = pow(C_bar, 7);

    // 25^7 = 6103515625, using value directly causes an integer overflow
    float C_7_sqrt = sqrt(pow(C_bar, 7) / (pow(C_bar, 7) + pow(25, 7)));
    float G = (1.0 - C_7_sqrt) / 2;

    float reference_a_prime = reference_a * (1 + G);
    float sampleval_a_prime = sampleval_a * (1 + G);

    float reference_C_prime = hypot(reference_a_prime, reference_b);
    float sampleval_C_prime = hypot(sampleval_a_prime, sampleval_b);
    float delta_C_prime = sampleval_C_prime - reference_C_prime;

    float C_bar_prime = (reference_C_prime + sampleval_C_prime) / 2;

    float reference_h_prime, sampleval_h_prime;

    if (reference_a_prime == reference_b)
    {
        reference_h_prime = 0;
    }
    else
    {
        reference_h_prime = atan2(reference_b, reference_a_prime);
        reference_h_prime = mod(degrees(reference_h_prime), 360);
    }

    if (sampleval_a_prime == sampleval_b)
    {
        sampleval_h_prime = 0;
    }
    else
    {
        sampleval_h_prime = atan2(sampleval_b, sampleval_a_prime);
        sampleval_h_prime = mod(degrees(sampleval_h_prime), 360);
    }

    float abs_h_prime_diff = abs(reference_h_prime - sampleval_h_prime);
    float delta_h_prime;

    if (reference_C_prime == 0 || sampleval_C_prime == 0)
    {
        delta_h_prime = 0;
    }
    else if (abs_h_prime_diff <= 180)
    {
        delta_h_prime = sampleval_h_prime - reference_h_prime;
    }
    else
    {
        delta_h_prime = (sampleval_h_prime <= reference_h_prime)
            ? sampleval_h_prime - reference_h_prime + 360
            : sampleval_h_prime - reference_h_prime - 360;
    }

    float delta_H_prime = 2 * sin(radians(delta_h_prime / 2)) *
        sqrt(reference_C_prime * sampleval_C_prime);

    float H_bar_prime;

    if (reference_C_prime == 0 || sampleval_C_prime == 0)
    {
        H_bar_prime = reference_h_prime + sampleval_h_prime;
    }
    else if (abs_h_prime_diff <= 180)
    {
        H_bar_prime = (reference_h_prime + sampleval_h_prime) / 2;
    }
    else
    {
        H_bar_prime = (reference_h_prime + sampleval_h_prime < 360)
            ? (reference_h_prime + sampleval_h_prime + 360) / 2
            : (reference_h_prime + sampleval_h_prime - 360) / 2;
    }

    float T = 1.0 -
        0.17 * cos(radians(H_bar_prime - 30)) +
        0.24 * cos(radians(2 * H_bar_prime)) +
        0.32 * cos(radians(3 * H_bar_prime + 6)) -
        0.20 * cos(radians(4 * H_bar_prime - 63));

    float delta_L_prime = sampleval_L - reference_L;

    float L_bar = (reference_L + sampleval_L) / 2;

    float S_L = 1.0 + (0.015 * sqr(L_bar - 50)) / sqrt(20 + sqr(L_bar - 50));
    float S_C = 1.0 + 0.045 * C_bar_prime;
    float S_H = 1.0 + 0.015 * C_bar_prime * T;

    float delta_theta = 30.0 * exp(-sqr((H_bar_prime - 275) / 25));

    float C_bar_7 = pow(C_bar_prime, 7);

    float R_T = -2.0 * sqrt(C_bar_7 / (C_bar_7 + pow(25, 7))) *
        sin(radians(2 * delta_theta));

    // K_L = K_C = K_H = 1

    float deltaE_00 = sqrt(
        sqr(delta_L_prime / S_L) +
        sqr(delta_C_prime / S_C) +
        sqr(delta_H_prime / S_H) +
        R_T * (delta_C_prime / S_C) * (delta_H_prime / S_H));

    return deltaE_00;
}


//
// Overloaded deltaE CIEDE2000, taking as reference and samples,
// (scene-linear) RGB values.
//

float deltaE_CIEDE2000(
    color reference_linear_RGB,
    string reference_color_space,
    string reference_illuminant,
    color sampleval_linear_RGB,
    string sampleval_color_space,
    string sampleval_illuminant)
{
    color reference_CIELAB = transform_linear_RGB_to_CIELAB(
        reference_linear_RGB,
        reference_color_space,
        reference_illuminant);

    color sampleval_CIELAB = transform_linear_RGB_to_CIELAB(
        sampleval_linear_RGB,
        sampleval_color_space,
        sampleval_illuminant);

    return deltaE_CIEDE2000(reference_CIELAB, sampleval_CIELAB);
}

float deltaE_CIEDE2000(
    color reference_linear_RGB,
    string reference_color_space,
    color sampleval_linear_RGB,
    string sampleval_color_space)
{
    return deltaE_CIEDE2000(
        reference_linear_RGB,
        reference_color_space,
        "D65",
        sampleval_linear_RGB,
        sampleval_color_space,
        "D65");
}
