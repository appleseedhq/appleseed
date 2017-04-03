
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2017 Luis Barrancos, The appleseedhq Organization
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
 
#ifndef AS_MAYA_CMS_SYNCOLOR_IDT_H
#define AS_MAYA_CMS_SYNCOLOR_IDT_H

#include "appleseed/color/as_colorimetry.h"
#include "appleseed/color/as_transfer_functions.h"

//
// Reference:
//
//      Autodesk Color Management: Supplemental Information
//
//      $MAYA_LOCATION/docs/files/GUID-BB4F38CF-6AA8-4D35-96DD-7F75D62FD3A7.htm
//

// Different working spaces might have different illuminants, and require a
// chromatic adaptation transform. Though we can use 
// chromatic_adaptation_vonKries() in the respective header, that is costly
// and envolves the use of CIE XYZ as an intermediary space.
// With that in mind, some RGB<>RGB matrices were precomputed, and these
// already take into account the potential chromatic adaptation.
// They are limited to D60, D65, DCIP3 illuminants, and Rec.709, Rec.2020,
// AdobeRGB, ACES 2065-1 AP0, ACEScg AP1, DCI-P3 color spaces.

color transform_color_space_to_Rec709(
    color input_color,
    string color_space)
{
    color transformed_color;
    vector v_color = vector(input_color);

    if (color_space == "sRGB")
    {
        transformed_color = sRGB_EOTF(input_color);
    }
    else if (color_space == "scene-linear Rec 709/sRGB")
    {
        transformed_color = input_color;
    }
    else if (color_space == "scene-linear Rec 2020")
    {
        transformed_color = color(
            dot(vector(REC2020_TO_REC709_X), v_color),
            dot(vector(REC2020_TO_REC709_Y), v_color),
            dot(vector(REC2020_TO_REC709_Z), v_color));
    }
    else if (color_space == "scene-linear DCI-P3")
    {
        transformed_color = color(
            dot(vector(DCIP3_TO_REC709_X), v_color),
            dot(vector(DCIP3_TO_REC709_Y), v_color),
            dot(vector(DCIP3_TO_REC709_Z), v_color));
    }
    else if (color_space == "camera Rec 709")
    {
        transformed_color = Rec1886_EOTF(input_color);
    }
    else if (color_space == "gamma 1.8 Rec 709")
    {
        transformed_color = gamma_CCTF(input_color, 1.8);
    }
    else if (color_space == "gamma 2.2 Rec 709")
    {
        transformed_color = gamma_CCTF(input_color, 2.2);
    }
    else if (color_space == "gamma 2.4 Rec 709 (video)")
    {
        transformed_color = gamma_CCTF(input_color, REC709_GAMMA);
    }
    else if (color_space == "ACES2065-1")
    {
        transformed_color = color(
            dot(vector(ACES_TO_REC709_X), v_color),
            dot(vector(ACES_TO_REC709_Y), v_color),
            dot(vector(ACES_TO_REC709_Z), v_color));
    }
    else if (color_space == "ACEScg")
    {
        transformed_color = color(
            dot(vector(ACESCG_TO_REC709_X), v_color),
            dot(vector(ACESCG_TO_REC709_Y), v_color),
            dot(vector(ACESCG_TO_REC709_Z), v_color));
    }
    else
    {
#ifdef DEBUG
        string shadername = "";
        getattribute("shader:shadername", shadername);

        warning("[WARNING]:Unsupported color space %s selected in %s, %s:%i\n",
                color_space, shadername, __FILE__, __LINE__);
#endif
        transformed_color = color(0);
    }
    return transformed_color;
}

//
// Rec.2020 render space, D65 whitepoint
//

color transform_color_space_to_Rec2020(
    color input_color,
    string color_space)
{
    color transformed_color;
    vector v_color = vector(input_color);

    if (color_space == "sRGB")
    {
        v_color = (vector) sRGB_EOTF(input_color);

        transformed_color = color(
            dot(vector(REC709_TO_REC2020_X), v_color),
            dot(vector(REC709_TO_REC2020_Y), v_color),
            dot(vector(REC709_TO_REC2020_Z), v_color));
    }
    else if (color_space == "scene-linear Rec 709/sRGB")
    {
        transformed_color = color(
            dot(vector(REC709_TO_REC2020_X), v_color),
            dot(vector(REC709_TO_REC2020_Y), v_color),
            dot(vector(REC709_TO_REC2020_Z), v_color));
    }
    else if (color_space == "scene-linear Rec 2020")
    {
        transformed_color = input_color;
    }
    else if (color_space == "scene-linear DCI-P3")
    {
        transformed_color = color(
            dot(vector(DCIP3_TO_REC2020_X), v_color),
            dot(vector(DCIP3_TO_REC2020_Y), v_color),
            dot(vector(DCIP3_TO_REC2020_Z), v_color));
    }
    else if (color_space == "camera Rec 709")
    {
        v_color = (vector) Rec1886_EOTF(input_color);

        transformed_color = color(
            dot(vector(REC709_TO_REC2020_X), v_color),
            dot(vector(REC709_TO_REC2020_Y), v_color),
            dot(vector(REC709_TO_REC2020_Z), v_color));
    }
    else if (color_space == "gamma 1.8 Rec 709")
    {
        v_color = (vector) gamma_CCTF(input_color, 1.8);

        transformed_color = color(
            dot(vector(REC709_TO_REC2020_X), v_color),
            dot(vector(REC709_TO_REC2020_Y), v_color),
            dot(vector(REC709_TO_REC2020_Z), v_color));
    }
    else if (color_space == "gamma 2.2 Rec 709")
    {
        v_color = (vector) gamma_CCTF(input_color, 2.2);

        transformed_color = color(
            dot(vector(REC709_TO_REC2020_X), v_color),
            dot(vector(REC709_TO_REC2020_Y), v_color),
            dot(vector(REC709_TO_REC2020_Z), v_color));
    }
    else if (color_space == "gamma 2.4 Rec 709 (video)")
    {
        v_color = (vector) gamma_CCTF(input_color, REC709_GAMMA);

        transformed_color = color(
            dot(vector(REC709_TO_REC2020_X), v_color),
            dot(vector(REC709_TO_REC2020_Y), v_color),
            dot(vector(REC709_TO_REC2020_Z), v_color));
    }
    else if (color_space == "ACES2065-1")
    {
        transformed_color = color(
            dot(vector(ACES_TO_REC2020_X), v_color),
            dot(vector(ACES_TO_REC2020_Y), v_color),
            dot(vector(ACES_TO_REC2020_Z), v_color));
    }
    else if (color_space == "ACEScg")
    {
        transformed_color = color(
            dot(vector(ACESCG_TO_REC2020_X), v_color),
            dot(vector(ACESCG_TO_REC2020_Y), v_color),
            dot(vector(ACESCG_TO_REC2020_Z), v_color));
    }
    else
    {
#ifdef DEBUG
        string shadername = "";
        getattribute("shader:shadername", shadername);

        warning("[WARNING]:Invalid color space %s selected in %s, %s:%i\n",
                color_space, shadername, __FILE__, __LINE__);
#endif
        transformed_color = color(0);
    }
    return transformed_color;
}

//
// DCI-P3 render space, DCI whitepoint
//

color transform_color_space_to_DCIP3(
    color input_color,
    string color_space)
{
    color transformed_color;
    vector v_color = vector(input_color);

    if (color_space == "sRGB")
    {
        v_color = (vector) sRGB_EOTF(input_color);

        transformed_color = color(
            dot(vector(REC709_TO_DCIP3_X), v_color),
            dot(vector(REC709_TO_DCIP3_Y), v_color),
            dot(vector(REC709_TO_DCIP3_Z), v_color));
    }
    else if (color_space == "scene-linear Rec 709/sRGB")
    {
        transformed_color = color(
            dot(vector(REC709_TO_DCIP3_X), v_color),
            dot(vector(REC709_TO_DCIP3_Y), v_color),
            dot(vector(REC709_TO_DCIP3_Z), v_color));
    }
    else if (color_space == "scene-linear Rec 2020")
    {
        transformed_color = color(
            dot(vector(REC2020_TO_DCIP3_X), v_color),
            dot(vector(REC2020_TO_DCIP3_Y), v_color),
            dot(vector(REC2020_TO_DCIP3_Z), v_color));
    }
    else if (color_space == "scene-linear DCI-P3")
    {
        transformed_color = input_color;
    }
    else if (color_space == "camera Rec 709")
    {
        v_color = (vector) Rec1886_EOTF(input_color);

        transformed_color = color(
            dot(vector(REC709_TO_DCIP3_X), v_color),
            dot(vector(REC709_TO_DCIP3_Y), v_color),
            dot(vector(REC709_TO_DCIP3_Z), v_color));
    }
    else if (color_space == "gamma 1.8 Rec 709")
    {
        v_color = (vector) gamma_CCTF(input_color, 1.8);

        transformed_color = color(
            dot(vector(REC709_TO_DCIP3_X), v_color),
            dot(vector(REC709_TO_DCIP3_Y), v_color),
            dot(vector(REC709_TO_DCIP3_Z), v_color));
    }
    else if (color_space == "gamma 2.2 Rec 709")
    {
        v_color = (vector) gamma_CCTF(input_color, 2.2);

        transformed_color = color(
            dot(vector(REC709_TO_DCIP3_X), v_color),
            dot(vector(REC709_TO_DCIP3_Y), v_color),
            dot(vector(REC709_TO_DCIP3_Z), v_color));
    }
    else if (color_space == "gamma 2.4 Rec 709 (video)")
    {
        v_color = (vector) gamma_CCTF(input_color, REC709_GAMMA);

        transformed_color = color(
            dot(vector(REC709_TO_DCIP3_X), v_color),
            dot(vector(REC709_TO_DCIP3_Y), v_color),
            dot(vector(REC709_TO_DCIP3_Z), v_color)); 
    }
    else if (color_space == "ACES2065-1")
    {
        transformed_color = color(
            dot(vector(ACES_TO_DCIP3_X), v_color),
            dot(vector(ACES_TO_DCIP3_Y), v_color),
            dot(vector(ACES_TO_DCIP3_Z), v_color));
    }
    else if (color_space == "ACEScg")
    {
        transformed_color = color(
            dot(vector(ACESCG_TO_DCIP3_X), v_color),
            dot(vector(ACESCG_TO_DCIP3_Y), v_color),
            dot(vector(ACESCG_TO_DCIP3_Z), v_color));
    }
    else
    {
#ifdef DEBUG
        string shadername = "";
        getattribute("shader:shadername", shadername);

        warning("[WARNING]:Invalid color space %s selected in %s, %s:%i\n",
                color_space, shadername, __FILE__, __LINE__);
#endif
        transformed_color = color(0);
    }
    return transformed_color;
}

//
// ACES 2065-1 AP0 render space, D60 whitepoint
//

color transform_color_space_to_ACES(
    color input_color,
    string color_space)
{
    color transformed_color;
    vector v_color = vector(input_color);

    if (color_space == "sRGB")
    {
        v_color = (vector) sRGB_EOTF(input_color);

        transformed_color = color(
            dot(vector(REC709_TO_ACES_X), v_color),
            dot(vector(REC709_TO_ACES_Y), v_color),
            dot(vector(REC709_TO_ACES_Z), v_color));
    }
    else if (color_space == "scene-linear Rec 709/sRGB")
    {
        transformed_color = color(
            dot(vector(REC709_TO_ACES_X), v_color),
            dot(vector(REC709_TO_ACES_Y), v_color),
            dot(vector(REC709_TO_ACES_Z), v_color));
    }
    else if (color_space == "scene-linear Rec 2020")
    {
        transformed_color = color(
            dot(vector(REC2020_TO_ACES_X), v_color),
            dot(vector(REC2020_TO_ACES_Y), v_color),
            dot(vector(REC2020_TO_ACES_Z), v_color));
    }
    else if (color_space == "scene-linear DCI-P3")
    {
        transformed_color = color(
            dot(vector(DCIP3_TO_ACES_X), v_color),
            dot(vector(DCIP3_TO_ACES_Y), v_color),
            dot(vector(DCIP3_TO_ACES_Z), v_color));
    }
    else if (color_space == "camera Rec 709")
    {
        v_color = (vector) Rec1886_EOTF(input_color);

        transformed_color = color(
            dot(vector(REC709_TO_ACES_X), v_color),
            dot(vector(REC709_TO_ACES_Y), v_color),
            dot(vector(REC709_TO_ACES_Z), v_color));
    }
    else if (color_space == "gamma 1.8 Rec 709")
    {
        v_color = (vector) gamma_CCTF(input_color, 1.8);

        transformed_color = color(
            dot(vector(REC709_TO_ACES_X), v_color),
            dot(vector(REC709_TO_ACES_Y), v_color),
            dot(vector(REC709_TO_ACES_Z), v_color));
    }
    else if (color_space == "gamma 2.2 Rec 709")
    {
        v_color = (vector) gamma_CCTF(input_color, 2.2);

        transformed_color = color(
            dot(vector(REC709_TO_ACES_X), v_color),
            dot(vector(REC709_TO_ACES_Y), v_color),
            dot(vector(REC709_TO_ACES_Z), v_color));
    }
    else if (color_space == "gamma 2.4 Rec 709 (video)")
    {
        v_color = (vector) gamma_CCTF(input_color, REC709_GAMMA);

        transformed_color = color(
            dot(vector(REC709_TO_ACES_X), v_color),
            dot(vector(REC709_TO_ACES_Y), v_color),
            dot(vector(REC709_TO_ACES_Z), v_color));
    }
    else if (color_space == "ACES2065-1")
    {
        transformed_color = input_color;
    }
    else if (color_space == "ACEScg")
    {
        transformed_color = color(
            dot(vector(ACESCG_TO_ACES_X), v_color),
            dot(vector(ACESCG_TO_ACES_Y), v_color),
            dot(vector(ACESCG_TO_ACES_Z), v_color));
    }
    else
    {
#ifdef DEBUG
        string shadername = "";
        getattribute("shader:shadername", shadername);

        warning("[WARNING]:Invalid color space %s selected in %s, %s:%i\n",
                color_space, shadername, __FILE__, __LINE__);
#endif
        transformed_color = color(0);
    }
    return transformed_color;
}

//
// ACEScg AP1 render space, D60 whitepoint
//

color transform_color_space_to_ACEScg(
    color input_color,
    string color_space)
{
    color transformed_color;
    vector v_color = vector(input_color);

    if (color_space == "sRGB")
    {
        v_color = (vector) sRGB_EOTF(input_color);

        transformed_color = color(
            dot(vector(REC709_TO_ACESCG_X), v_color),
            dot(vector(REC709_TO_ACESCG_Y), v_color),
            dot(vector(REC709_TO_ACESCG_Z), v_color));
    }
    else if (color_space == "scene-linear Rec 709/sRGB")
    {
        transformed_color = color(
            dot(vector(REC709_TO_ACESCG_X), v_color),
            dot(vector(REC709_TO_ACESCG_Y), v_color),
            dot(vector(REC709_TO_ACESCG_Z), v_color));
    }
    else if (color_space == "scene-linear Rec 2020")
    {
        transformed_color = color(
            dot(vector(REC2020_TO_ACESCG_X), v_color),
            dot(vector(REC2020_TO_ACESCG_Y), v_color),
            dot(vector(REC2020_TO_ACESCG_Z), v_color));
    }
    else if (color_space == "scene-linear DCI-P3")
    {
        transformed_color = color(
            dot(vector(DCIP3_TO_ACESCG_X), v_color),
            dot(vector(DCIP3_TO_ACESCG_Y), v_color),
            dot(vector(DCIP3_TO_ACESCG_Z), v_color));
    }
    else if (color_space == "camera Rec 709")
    {
        v_color = (vector) Rec1886_EOTF(input_color);

        transformed_color = color(
            dot(vector(REC709_TO_ACESCG_X), v_color),
            dot(vector(REC709_TO_ACESCG_Y), v_color),
            dot(vector(REC709_TO_ACESCG_Z), v_color));
    }
    else if (color_space == "gamma 1.8 Rec 709")
    {
        v_color = (vector) gamma_CCTF(input_color, 1.8);

        transformed_color = color(
            dot(vector(REC709_TO_ACESCG_X), v_color),
            dot(vector(REC709_TO_ACESCG_Y), v_color),
            dot(vector(REC709_TO_ACESCG_Z), v_color));
    }
    else if (color_space == "gamma 2.2 Rec 709")
    {
        v_color = (vector) gamma_CCTF(input_color, 2.2);

        transformed_color = color(
            dot(vector(REC709_TO_ACESCG_X), v_color),
            dot(vector(REC709_TO_ACESCG_Y), v_color),
            dot(vector(REC709_TO_ACESCG_Z), v_color));
    }
    else if (color_space == "gamma 2.4 Rec 709 (video)")
    {
        v_color = (vector) gamma_CCTF(input_color, REC709_GAMMA);

        transformed_color = color(
            dot(vector(REC709_TO_ACESCG_X), v_color),
            dot(vector(REC709_TO_ACESCG_Y), v_color),
            dot(vector(REC709_TO_ACESCG_Z), v_color));
    }
    else if (color_space == "ACES2065-1")
    {
        transformed_color = color(
            dot(vector(ACES_TO_ACESCG_X), v_color),
            dot(vector(ACES_TO_ACESCG_Y), v_color),
            dot(vector(ACES_TO_ACESCG_Z), v_color));
    }
    else if (color_space == "ACEScg")
    {
        transformed_color = input_color;
    }
    else
    {
#ifdef DEBUG
        string shadername = "";
        getattribute("shader:shadername", shadername);

        warning("[WARNING]:Invalid color space %s selected in %s, %s:%i\n",
                color_space, shadername, __FILE__, __LINE__);
#endif
        transformed_color = color(0);
    }
    return transformed_color;
}

color transform_colorspace_to_workingspace(
    color input_color,
    string color_space,
    string working_space)
{
    color C;

    if (working_space == "scene-linear Rec 709/sRGB")
    {
        C = transform_color_space_to_Rec709(input_color, color_space);
    }
    else if (working_space == "scene-linear Rec 2020")
    {
        C = transform_color_space_to_Rec2020(input_color, color_space);
    }
    else if (working_space == "scene-linear DCI-P3")
    {
        C = transform_color_space_to_DCIP3(input_color, color_space);
    }
    else if (working_space == "ACES2065-1")
    {
        C = transform_color_space_to_ACES(input_color, color_space);
    }
    else if (working_space == "ACEScg")
    {
        C = transform_color_space_to_ACEScg(input_color, color_space);
    }
    else
    {
#ifdef DEBUG
        string shadername = "";
        getattribute("shader:shadername", shadername);

        warning("[WARNING]:Unsupported working space %s in %s, %s:%i\n",
                working_space, shadername, __FILE__, __LINE__);
#endif
        C = color(0);
    }
    return C;
}

#endif // !AS_MAYA_CMS_SYNCOLOR_IDT_H
