
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Luis Barrancos, The appleseedhq Organization
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

#ifndef AS_COLOR_TRANSFORMS_H
#define AS_COLOR_TRANSFORMS_H

#include "appleseed/color/as_chromatic_adaptation.h"
#include "appleseed/color/as_colorimetry.h"
#include "appleseed/math/as_math_helpers.h"

//
// Reference:
//
//      Colour Space Conversions
//
//      http://www.poynton.com/PDFs/coloureq.pdf
//      http://www.brucelindbloom.com/index.html?Eqn_XYZ_to_xyY.html
//      

color transform_XYZ_to_xyY(color XYZ, float white_xy[2])
{
    color xyY;

    if (XYZ[0] == XYZ[1] == XYZ[2] == 0.0)
    {
        xyY = color(white_xy[0], white_xy[1], XYZ[1]);
    }
    else
    {
        float XYZ_sum = XYZ[0] + XYZ[1] + XYZ[2];

        xyY = color(XYZ[0] / XYZ_sum, XYZ[1] / XYZ_sum, XYZ[1]);
    }
    return xyY;
}

//
// Reference:
//
//      xyY to XYZ conversion
//
//      http://www.brucelindbloom.com/index.html?Eqn_xyY_to_XYZ.html
//

color transform_xyY_to_XYZ(color xyY)
{
    color XYZ;

    if (xyY[1] != 0)
    {
        XYZ[0] = xyY[0] * xyY[2] / xyY[1];
        XYZ[1] = xyY[2];
        XYZ[2] = (1.0 - xyY[0] - xyY[1]) * xyY[2] / xyY[1]; // z = 1-x-y
    }
    else
    {
        XYZ = color(0);
    }
    return XYZ;
}

color get_illuminant_xyY(string illuminant)
{
    color white_xyY;

    if (illuminant == "D50")
    {
        white_xyY = color(D50_WHITEPOINT_CIE1931_2DEG_xy, 1.0);
    }
    else if (illuminant == "D55")
    {
        white_xyY = color(D55_WHITEPOINT_CIE1931_2DEG_xy, 1.0);
    }
    else if (illuminant == "D60")
    {
        white_xyY = color(D60_WHITEPOINT_CIE1931_2DEG_xy, 1.0);
    }
    else if (illuminant == "D65")
    {
        white_xyY = color(D65_WHITEPOINT_CIE1931_2DEG_xy, 1.0);
    }
    else if (illuminant == "D75")
    {
        white_xyY = color(D65_WHITEPOINT_CIE1931_2DEG_xy, 1.0);
    }
    else if (illuminant == "DCIP3")
    {
        white_xyY = color(DCIP3_WHITEPOINT_CIE1931_2DEG_xy, 1.0);
    }
    else
    {
#ifdef DEBUG
        string shadername = "";
        getattribute("shader:shadername", shadername);

        warning("[WARNING]: Unsupported illuminant %s in %s, %s:%i\n",
                illuminant, shadername, __FILE__, __LINE__);
#endif
        return color(0);
    }
    return white_xyY;
}

color get_illuminant_XYZ(string illuminant)
{
    color white_xyY = get_illuminant_xyY(illuminant);

    return transform_xyY_to_XYZ(white_xyY);
}

void get_RGB_to_XYZ_matrix(
    string color_space,
    output vector RGB_to_XYZ[3],
    output string source_illuminant)
{
    if (color_space == "ACES")
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
    else if (color_space == "Rec.2020")
    {
        source_illuminant = "D65";

        RGB_to_XYZ[0] = vector(RGB_TO_XYZ_REC2020_R0);
        RGB_to_XYZ[1] = vector(RGB_TO_XYZ_REC2020_R1);
        RGB_to_XYZ[2] = vector(RGB_TO_XYZ_REC2020_R2);
    }
    else if (color_space == "Rec.709")
    {
        source_illuminant = "D65";

        RGB_to_XYZ[0] = vector(RGB_TO_XYZ_REC709_R0);
        RGB_to_XYZ[1] = vector(RGB_TO_XYZ_REC709_R1);
        RGB_to_XYZ[2] = vector(RGB_TO_XYZ_REC709_R2);
    }
    else
    {
#ifdef DEBUG
        string shadername = "";
        getattribute("shader:shadername", shadername);

        warning("[WARNING]:Unsupported/unknown color space %s in %s, %s:%i\n",
                color_space, shadername, __FILE__, __NAME__);
#endif
        exit(); // no color space nor illuminant, no point in continuing
    }
}

void get_XYZ_to_RGB_matrix(
    string color_space,
    output vector XYZ_to_RGB[3],
    output string source_illuminant)
{
    if (color_space == "ACES")
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
    else if (color_space == "Rec.2020")
    {
        source_illuminant = "D65";

        XYZ_to_RGB[0] = vector(XYZ_TO_RGB_REC2020_R0);
        XYZ_to_RGB[1] = vector(XYZ_TO_RGB_REC2020_R1);
        XYZ_to_RGB[2] = vector(XYZ_TO_RGB_REC2020_R2);
    }
    else if (color_space == "Rec.709")
    {
        source_illuminant = "D65";

        XYZ_to_RGB[0] = vector(XYZ_TO_RGB_REC709_R0);
        XYZ_to_RGB[1] = vector(XYZ_TO_RGB_REC709_R1);
        XYZ_to_RGB[2] = vector(XYZ_TO_RGB_REC709_R2);
    }
    else
    {
        string shadername = "";
        getattribute("shader:shadername", shadername);

        warning("[WARNING]:Unsupported/unknown color space %s in %s, %s:%i\n",
                color_space, shadername, __FILE__, __LINE__);

        // We can exit the execution completely, or try to handle it a bit
        // more nicely. Set the matrix as identity and enabling the warnings
        // might be better.

        XYZ_to_RGB[0] = vector(1, 0, 0);
        XYZ_to_RGB[1] = vector(0, 1, 0);
        XYZ_to_RGB[2] = vector(0, 0, 1);
    }
}

//
// Reference:
//
//      RGB to XYZ conversion, and chromatic adaptation
//
//      http://www.brucelindbloom.com/index.html?Eqn_RGB_to_XYZ.html
//      http://www.brucelindbloom.com/index.html?Eqn_ChromAdapt.html
//

color transform_linear_RGB_to_XYZ(
    color linear_RGB_color,
    string color_space,
    string target_illuminant)
{
    string source_illuminant = "";
    vector source_RGB_to_XYZ[3];

    get_RGB_to_XYZ_matrix(color_space, source_RGB_to_XYZ, source_illuminant);

    vector XYZ = vector(
        dot(source_RGB_to_XYZ[0], (vector) linear_RGB_color),
        dot(source_RGB_to_XYZ[1], (vector) linear_RGB_color),
        dot(source_RGB_to_XYZ[2], (vector) linear_RGB_color));

    if (source_illuminant != "" && source_illuminant != target_illuminant)
    {
        color source_white_XYZ = get_illuminant_XYZ(source_illuminant);
        color target_white_XYZ = get_illuminant_XYZ(target_illuminant);

        vector CAT[3];

        chromatic_adaptation_vonKries(
            source_white_XYZ,
            target_white_XYZ,
            "Bradford",
            CAT);

        vector adapted_XYZ = vector(
            dot(CAT[0], XYZ),
            dot(CAT[1], XYZ),
            dot(CAT[2], XYZ));

        XYZ = adapted_XYZ;
    }
    return (color) XYZ;
}

//
// Reference:
//
//      XYZ to RGB conversion, and chromatic adaptation
//
//      http://www.brucelindbloom.com/index.html?Eqn_XYZ_to_RGB.html
//      http://www.brucelindbloom.com/Eqn_ChromAdapt.html
//

color transform_XYZ_to_linear_RGB(
    color XYZ_color,
    string color_space,
    string target_illuminant)
{
    string source_illuminant = "";
    vector source_XYZ_to_RGB[3], XYZ;

    get_XYZ_to_RGB_matrix(color_space, source_XYZ_to_RGB, source_illuminant);

    if (source_illuminant != "" && source_illuminant != target_illuminant)
    {
        color source_white_XYZ = get_illuminant_XYZ(source_illuminant);
        color target_white_XYZ = get_illuminant_XYZ(target_illuminant);

        vector CAT[3];

        chromatic_adaptation_vonKries(
            source_white_XYZ,
            target_white_XYZ,
            "Bradford",
            CAT);

        XYZ = vector(
            dot(CAT[0], (vector) XYZ_color),
            dot(CAT[1], (vector) XYZ_color),
            dot(CAT[2], (vector) XYZ_color));
    }
    else
    {
        XYZ = (vector) XYZ_color;
    }

    vector linear_RGB = vector(
        dot(source_XYZ_to_RGB[0], XYZ),
        dot(source_XYZ_to_RGB[1], XYZ),
        dot(source_XYZ_to_RGB[2], XYZ));

    return (color) linear_RGB;
}

//
// Overloaded RGB<>XYZ transformation function, assuming the target white
// point is the same as the system white point, whatever that might be.
//

color transform_linear_RGB_to_XYZ(
    color linear_RGB_color,
    string color_space)
{
    string source_illuminant = "";
    vector source_RGB_to_XYZ[3];

    get_RGB_to_XYZ_matrix(color_space, source_RGB_to_XYZ, source_illuminant);

    vector XYZ = vector(
        dot(source_RGB_to_XYZ[0], (vector) linear_RGB_color),
        dot(source_RGB_to_XYZ[1], (vector) linear_RGB_color),
        dot(source_RGB_to_XYZ[2], (vector) linear_RGB_color));

    return (color) XYZ;
} 

color transform_XYZ_to_linear_RGB(
    color XYZ,
    string color_space,
    string target_illuminant)
{
    string source_illuminant = "";
    vector source_XYZ_to_RGB[3];

    get_XYZ_to_RGB_matrix(color_space, source_XYZ_to_RGB, source_illuminant);

    vector linear_RGB = vector(
        dot(source_XYZ_to_RGB[0], XYZ),
        dot(source_XYZ_to_RGB[1], XYZ),
        dot(source_XYZ_to_RGB[2], XYZ));

    return (color) linear_RGB;
}

//
// Reference:
//
//      Color Transformation Equations, 5.2
//
//      http://www.poynton.com/PDFs/coloureq.pdf
//

color transform_xyY_to_CIE_Yuv(color xyY)
{
    float denom = 6 * xyY[1] - xyY[0] + 1.5;
    float CIE_u = 2 * xyY[0] / denom;
    float CIE_v = 3 * xyY[1] / denom;

    return color(xyY[2], CIE_u, CIE_v);
}

//
// Reference:
//
//      RGB to XYZ to Lab equations
//
//      http://www.brucelindbloom.com/index.html?Eqn_XYZ_to_Lab.html
//      http://www.brucelindbloom.com/LContinuity.html
//

color transform_XYZ_to_Lab(color XYZ_color, color reference_white_xyY)
{
    color XYZ_white = transform_xyY_to_XYZ(reference_white_xyY);
    color XYZ_f = XYZ_color / XYZ_white, XYZ;

    for (int i = 0; i < 3; ++i)
    {
        XYZ[i] = (XYZ_f[i] > CIE_E)
            ? pow(XYZ_f[i], 1 / 3)
            : (CIE_K * XYZ_f[i] + 16) / 116;
    }

    float L  = 116 * XYZ[1] - 16;
    float a = 500 * (XYZ[0] - XYZ[1]);
    float b = 200 * (XYZ[1] - XYZ[2]);

    return color(L, a, b); // L in [0,100]
}

color transform_XYZ_to_Lab(color XYZ_color, float reference_white_xy[2])
{
    return transform_XYZ_to_Lab(
        XYZ_color,
        color( reference_white_xy[0], reference_white_xy[1], 1.0));
}

color transform_XYZ_to_Lab(color XYZ_color, string illuminant)
{
    return transform_XYZ_to_Lab(XYZ_color, get_illuminant_xyY(illuminant));
}

//
// Reference:
//
//      Lab to XYZ
//
//      http://www.brucelindbloom.com/index.html?Eqn_Lab_to_XYZ.html
//

color transform_Lab_to_XYZ(color Lab, color reference_white_xyY)
{
    color XYZ_white = transform_xyY_to_XYZ(reference_white_xyY);

    float f_Y = (Lab[0] + 16) / 116;
    float f_X = Lab[1] / 500 + f_Y;
    float f_Z = f_Y - Lab[2] / 200;

    float X_r = (pow(f_X, 3) > CIE_E)
        ? pow(f_X, 3)
        : (116 * f_X - 16) / CIE_K;

    float Y_r = (Lab[0] > CIE_K * CIE_E)
        ? pow((Lab[0] + 16) / 116, 3)
        : Lab[0] / CIE_K;

    float Z_r = (pow(f_Z, 3) > CIE_E)
        ? pow(f_Z, 3)
        : (116 * f_Z - 16) / CIE_K;

    return color(X_r, Y_r, Z_r) * XYZ_white;
}

color transform_Lab_to_XYZ(color Lab, float reference_white_xy[2])
{
    return transform_Lab_to_XYZ(
        Lab,
        color(reference_white_xy[0], reference_white_xy[1], 1.0));
}

color transform_Lab_to_XYZ(color Lab_color, string illuminant)
{
    return transform_Lab_to_XYZ(Lab_color, get_illuminant_xyY(illuminant));
}

//
// Reference:
//
//      XYZ to Luv
//
//      http://www.brucelindbloom.com/index.html?Eqn_XYZ_to_Luv.html
//      http://www.brucelindbloom.com/LContinuity.html
//

color transform_XYZ_to_Luv(color XYZ_color, color reference_white_xyY)
{
    color XYZ_white = transform_xyY_to_XYZ(reference_white_xyY);

    float y_r = XYZ_color[1] / XYZ_white[1];

    float denom = XYZ_color[0] + 15 * XYZ_color[1] + 3 * XYZ_color[2];
    float denom_r = XYZ_white[0] + 15 * XYZ_white[1] + 3 * XYZ_white[2];

    float u_prime = 4 * XYZ_color[0] / denom;
    float v_prime = 9 * XYZ_color[1] / denom;

    float u_prime_r = 4 * XYZ_white[0] / denom_r;
    float v_prime_r = 9 * XYZ_white[1] / denom_r;

    float L = (y_r > CIE_E)
        ? 116 * pow(y_r, 1/3) - 16
        : CIE_K * y_r;

    float Luv_u = 13 * L * (u_prime - u_prime_r);
    float Luv_v = 13 * L * (v_prime - v_prime_r);

    return color(L, Luv_u, Luv_v);
}

color transform_XYZ_to_Luv(color XYZ_color, float reference_white_xy[2])
{
    return transform_XYZ_to_Luv(
        XYZ_color,
        color( reference_white_xy[0], reference_white_xy[1], 1.0));
}

color transform_XYZ_to_Luv(color XYZ_color, string illuminant)
{
    return transform_XYZ_to_Luv(XYZ_color, get_illuminant_xyY(illuminant));
}

//
// Reference:
//
//      Luv to XYZ
//
//      http://www.brucelindbloom.com/index.html?Eqn_XYZ_to_Luv.html
//      http://www.brucelindbloom.com/LContinuity.html
//

color transform_Luv_to_XYZ(color Luv, color reference_white_xyY)
{
    color white_XYZ = transform_xyY_to_XYZ(reference_white_xyY);

    float denom_r = white_XYZ[0] + 15 * white_XYZ[1] + 3 * white_XYZ[2];

    float u0 = 4 * white_XYZ[0] / denom_r;
    float v0 = 9 * white_XYZ[1] / denom_r;

    float Y = (Luv[0] > CIE_K * CIE_E)
        ? pow((Luv[0] + 16) / 116, 1 / 3)
        : Luv[0] / CIE_K;

    float a = (52 * Luv[0] / (Luv[1] + 13 * Luv[0] * u0) - 1) / 3;
    float b = -5 * Y;
    float c = -1 / 3;
    float d = Y * (39 * Luv[0] / (Luv[2] + 13 * Luv[0] * v0) - 5);

    float X = (d - b) / (a - c);
    float Z = X * a + b;

    return color(X, Y, Z);
}

color transform_Luv_to_XYZ(color Luv, float reference_white_xy[2])
{
    return transform_Luv_to_XYZ(
        Luv,
        color(reference_white_xy[0], reference_white_xy[1], 1.0));
}

color transform_Luv_to_XYZ(color Luv, string illuminant)
{
    return transform_Luv_to_XYZ(Luv, get_illuminant_xyY(illuminant));
}

//
// Reference:
//
//      Lab to LCH(ab) equations
//
//      http://www.brucelindbloom.com/index.html?Eqn_Lab_to_LCH.html
//      http://www.brucelindbloom.com/index.html?Eqn_XYZ_to_Luv.html
//
// Note: When converting from Lab, LCH contains H in degrees, and when
//       converting to LCH, H is also expected in degrees.
//

color transform_Lab_to_LCh_ab(color Lab)
{
    float h = mod(degrees(atan2(Lab[2], Lab[1])), 360);
    float C = hypot(Lab[1], Lab[2]);
    float L = Lab[0];

    return color(L, C, h);
}

color transform_LCh_ab_to_Lab(color LCh_ab)
{
    float L = LCh_ab[0], a, b;

    sincos(radians(LCh_ab[2]), a, b);

    return color(L, a, b);
}

color transform_linear_RGB_to_Lab(
    color linear_RGB,
    string color_space,
    string target_illuminant)
{
    color XYZ = transform_linear_RGB_to_XYZ(
        linear_RGB,
        color_space,
        target_illuminant);

    return transform_XYZ_to_Lab(XYZ, target_illuminant);
}

color transform_Lab_to_linear_RGB(
    color Lab,
    string color_space,
    string target_illuminant)
{
    color XYZ = transform_Lab_to_XYZ(Lab, target_illuminant);

    return transform_XYZ_to_linear_RGB(
        XYZ,
        color_space,
        target_illuminant);
}

color transform_linear_RGB_to_LCh_ab(
    color linear_RGB,
    string color_space,
    string target_illuminant)
{
    color Lab = transform_linear_RGB_to_Lab(
        linear_RGB,
        color_space,
        target_illuminant);

    return transform_Lab_to_LCh_ab(Lab);
}

color transform_LCh_ab_to_linear_RGB(
    color LCh_ab,
    string color_space,
    string target_illuminant)
{
    color Lab = transform_LCh_ab_to_Lab(LCh_ab);

    return transform_Lab_to_linear_RGB(
        Lab,
        color_space,
        target_illuminant);
}

//
// Reference:
//
//      Luv to LCH(uv) equations
//
//      http://www.brucelindbloom.com/index.html?Eqn_Luv_to_LCH.html
//

color transform_Luv_to_LCh_uv(color Luv)
{
    float L = Luv[0];
    float C = hypot(Luv[1], Luv[2]);
    float h = mod(degrees(atan2(Luv[2], Luv[1])), 360);

    return color(L, C, h);
}

color transform_LCh_uv_to_Luv(color LCh_uv)
{
    float L = LCh_uv[0], uu, vv;

    sincos(radians(LCh_uv[2]), uu, vv);

    return color(LCh_uv[0], uu, vv);
}

color transform_linear_RGB_to_Luv(
    color linear_RGB,
    string color_space,
    string target_illuminant)
{
    color XYZ = transform_linear_RGB_to_XYZ(
        linear_RGB,
        color_space,
        target_illuminant);

    return transform_XYZ_to_Luv(XYZ, target_illuminant);
}

color transform_Luv_to_linear_RGB(
    color Luv,
    string color_space,
    string target_illuminant)
{
    color XYZ = transform_Luv_to_XYZ(Luv, target_illuminant);

    return transform_XYZ_to_linear_RGB(
        XYZ,
        color_space,
        target_illuminant);
}

color transform_linear_RGB_to_LCh_uv(
    color linear_RGB,
    string color_space,
    string target_illuminant)
{
    color Luv = transform_linear_RGB_to_Luv(
        linear_RGB,
        color_space,
        target_illuminant);

    return transform_Luv_to_LCh_uv(Luv);
}

color transform_LCh_uv_to_linear_RGB(
    color LCh_uv,
    string color_space,
    string target_illuminant)
{
    color Luv = transform_LCh_uv_to_Luv(LCh_uv);

    return transform_Luv_to_linear_RGB(
        Luv,
        color_space,
        target_illuminant);
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
// Note: It's unlikely this will be of any use to the end user, but it's of
//       use to us in measuring deviation from the color transformation 
//       results against reference values. Used for now with OSL testshade
//       exclusively.
//

float deltaE_CIEDE2000(
    color   reference_Lab,
    color   sampleval_Lab)
{   
    float reference_L = reference_Lab[0];
    float reference_a = reference_Lab[1];
    float reference_b = reference_Lab[2];

    float sampleval_L = sampleval_Lab[0];
    float sampleval_a = sampleval_Lab[1];
    float sampleval_b = sampleval_Lab[2];


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
// Overloaded deltaE_CIEDE2000, taking as reference and samples
// (scene-linear) RGB colors instead of Lab colors.
//

float deltaE_CIEDE2000(
    color   reference_linear_RGB,
    string  reference_color_space,
    color   sample_linear_RGB,
    string  sample_color_space)
{
    // If the colors are in a color space with a different white point,
    // then adapt them to D65.

    color reference_Lab = transform_linear_RGB_to_Lab(
        reference_linear_RGB,
        reference_color_space,
        "D65");

    color sampleval_Lab = transform_linear_RGB_to_Lab(
        sample_linear_RGB,
        sample_color_space,
        "D65");

    return deltaE_CIEDE2000(reference_Lab, sampleval_Lab);
}

#endif // !AS_COLOR_TRANSFORMS_H
