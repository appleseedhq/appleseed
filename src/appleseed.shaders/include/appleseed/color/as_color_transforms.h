
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

color get_illuminant_XYZ(string illuminant)
{
    color white_xyY;

    if (illuminant = "D50")
    {
        white_xyY = color(D50_WHITEPOINT_CIE1931_2DEG_xy, 1.0);
    }
    else if (illuminant = "D55")
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
#ifdef DEBUG
        string shadername = "";
        getattribute("shader:shadername", shadername);

        warning("[WARNING]:Unsupported/unknown color space %s in %s, %s:%i\n",
                color_space, shadername, __FILE__, __NAME__);
#endif
        exit(); // no color space nor illuminant, no point in continuing
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

#endif // !AS_COLOR_TRANSFORMS_H
