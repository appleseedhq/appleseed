
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

#ifndef AS_COLOR_IDT_H
#define AS_COLOR_IDT_H

#include "appleseed/color/as_color_data.h"
#include "appleseed/color/as_transfer_functions.h"

//
// Linearization and input space to working space conversions with precomputed
// RGB to RGB matrices, chromatically adapted where meaningful, using the
// Bradford CAT.
// Only the D60, D65, DCI-P3 illuminants, supported. for other illuminants
// and CATs, you might want to use the function
// chromatic_adaptation_transform_vonKries() function.

// Rec.709 render space, D65 white point
//
color transform_colorspace_to_Rec709(
    color input_color,
    string colorspace)
{
    color transformed_color;
    vector v_color = vector(input_color);

    if (colorspace == "sRGB")
    {
        // Same chromaticities, apply EOTF.
        transformed_color = sRGB_EOTF(input_color);
    }
    else if (colorspace == "scene-linear Rec 709/sRGB")
    {
        ; // Nothing to do here, assumed linear gamma, same chromaticities.
    }
    else if (colorspace == "scene-linear Rec 2020")
    {
        // Linear encoding, transform chromaticities, same whitepoint.
        transformed_color = color(
            dot(vector(REC2020_TO_REC709_X), v_color),
            dot(vector(REC2020_TO_REC709_Y), v_color),
            dot(vector(REC2020_TO_REC709_Z), v_color));
    }
    else if (colorspace == "scene-linear DCI-P3")
    {
        // Linear encoding, transform chromaticities, same whitepoint.
        transformed_color = color(
            dot(vector(DCIP3_TO_REC709_X), v_color),
            dot(vector(DCIP3_TO_REC709_Y), v_color),
            dot(vector(DCIP3_TO_REC709_Z), v_color));
    }
    else if (colorspace == "camera Rec 709")
    {
        // Same chromaticities, apply EOTF.
        transformed_color = Rec709_EOTF(input_color);
    }
    else if (colorspace == "gamma 1.8 Rec 709")
    {
        // Same chromaticities, apply EOTF.
        transformed_color = gamma_EOTF(input_color, 1.8);
    }
    else if (colorspace == "gamma 2.2 Rec 709")
    {
        // Same chromaticities, apply EOTF.
        transformed_color = gamma_EOTF(input_color, 2.2);
    }
    else if (colorspace == "gamma 2.4 Rec 709 (video)")
    {
        // Same chromaticities, apply EOTF.
        transformed_color = gamma_EOTF(input_color, 2.4);
    }
    else if (colorspace == "ACES2065-1")
    {
        // Linear encoding, chromaticities change with CAT for D60->D65.
        transformed_color = color(
            dot(vector(ACES_TO_REC709_X), v_color),
            dot(vector(ACES_TO_REC709_Y), v_color),
            dot(vector(ACES_TO_REC709_Z), v_color));
    }
    else if (colorspace == "ACEScg")
    {
        // Linear encoding, chromaticities change with CAT for D60->D65.
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
        warning("[WARNING]:Invalid color space selected in %s, %s:%i\n",
                shadername, __FILE__, __LINE__);
#endif
        transformed_color = color(0);
    }
    return transformed_color;
}

// Rec.2020 render space, D65 whitepoint
//
color transform_colorspace_to_Rec2020(
    color input_color,
    string colorspace)
{
    color transformed_color;
    vector v_color = vector(input_color);

    if (colorspace == "sRGB")
    {
        // apply EOTF first, then convert chromaticities, same whitepoint.
        v_color = (vector) sRGB_EOTF(input_color);

        transformed_color = color(
            dot(vector(REC709_TO_REC2020_X), v_color),
            dot(vector(REC709_TO_REC2020_Y), v_color),
            dot(vector(REC709_TO_REC2020_Z), v_color));
    }
    else if (colorspace == "scene-linear Rec 709/sRGB")
    {
        // Linear gamma already, convert chromaticities only, same whitepoint.
        transformed_color = color(
            dot(vector(REC709_TO_REC2020_X), v_color),
            dot(vector(REC709_TO_REC2020_Y), v_color),
            dot(vector(REC709_TO_REC2020_Z), v_color));
    }
    else if (colorspace == "scene-linear Rec 2020")
    {
        // Already in target gamma and chromaticites, nothing to do.
        ;
    }
    else if (colorspace == "scene-linear DCI-P3")
    {
        // Linear gamma, convert chromaticities, adjust whitepoint DCI->D65.
        transformed_color = color(
            dot(vector(DCIP3_TO_REC2020_X), v_color),
            dot(vector(DCIP3_TO_REC2020_Y), v_color),
            dot(vector(DCIP3_TO_REC2020_Z), v_color));
    }
    else if (colorspace == "camera Rec 709")
    {
        // Apply EOTF first, chromaticities change after, same whitepoint.
        v_color = (vector) Rec709_EOTF(input_color);

        transformed_color = color(
            dot(vector(REC709_TO_REC2020_X), v_color),
            dot(vector(REC709_TO_REC2020_Y), v_color),
            dot(vector(REC709_TO_REC2020_Z), v_color));
    }
    else if (colorspace == "gamma 1.8 Rec 709")
    {
        // Apply EOTF first, chromaticities change after, same whitepoint.
        v_color = (vector) gamma_EOTF(input_color, 1.8);

        transformed_color = color(
            dot(vector(REC709_TO_REC2020_X), v_color),
            dot(vector(REC709_TO_REC2020_Y), v_color),
            dot(vector(REC709_TO_REC2020_Z), v_color));
    }
    else if (colorspace == "gamma 2.2 Rec 709")
    {
        // Apply EOTF first, chromaticities change after, same whitepoint.
        v_color = (vector) gamma_EOTF(input_color, 2.2);

        transformed_color = color(
            dot(vector(REC709_TO_REC2020_X), v_color),
            dot(vector(REC709_TO_REC2020_Y), v_color),
            dot(vector(REC709_TO_REC2020_Z), v_color));
    }
    else if (colorspace == "gamma 2.4 Rec 709 (video)")
    {
        // Apply EOTF first, chromaticities change after, same whitepoint.
        v_color = (vector) gamma_EOTF(input_color, 2.4);

        transformed_color = color(
            dot(vector(REC709_TO_REC2020_X), v_color),
            dot(vector(REC709_TO_REC2020_Y), v_color),
            dot(vector(REC709_TO_REC2020_Z), v_color));
    }
    else if (colorspace == "ACES2065-1")
    {
        // Linear encoding, chromaticities change with CAT for D60->D65.
        transformed_color = color(
            dot(vector(ACES_TO_REC2020_X), v_color),
            dot(vector(ACES_TO_REC2020_Y), v_color),
            dot(vector(ACES_TO_REC2020_Z), v_color));
    }
    else if (colorspace == "ACEScg")
    {
        // Linear encoding, chromaticities change with CAT for D60->D65.
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
        warning("[WARNING]:Invalid color space selected in %s, %s:%i\n",
                shadername, __FILE__, __LINE__);
#endif
        transformed_color = color(0);
    }
    return transformed_color;
}

// DCI-P3 render space, DCI whitepoint
//
color transform_colorspace_to_DCIP3(
    color input_color,
    string colorspace)
{
    color transformed_color;
    vector v_color = vector(input_color);

    if (colorspace == "sRGB")
    {
        // apply EOTF first, then convert chromaticities, D65->DCI.
        v_color = (vector) sRGB_EOTF(input_color);

        transformed_color = color(
            dot(vector(REC709_TO_DCIP3_X), v_color),
            dot(vector(REC709_TO_DCIP3_Y), v_color),
            dot(vector(REC709_TO_DCIP3_Z), v_color));
    }
    else if (colorspace == "scene-linear Rec 709/sRGB")
    {
        // Linear gamma already, convert chromaticities, adjust whitepoint.
        transformed_color = color(
            dot(vector(REC709_TO_DCIP3_X), v_color),
            dot(vector(REC709_TO_DCIP3_Y), v_color),
            dot(vector(REC709_TO_DCIP3_Z), v_color));
    }
    else if (colorspace == "scene-linear Rec 2020")
    {
        // Linear gamma already, convert chromaticities, adjust whitepoint.
        transformed_color = color(
            dot(vector(REC2020_TO_DCIP3_X), v_color),
            dot(vector(REC2020_TO_DCIP3_Y), v_color),
            dot(vector(REC2020_TO_DCIP3_Z), v_color));
        ;
    }
    else if (colorspace == "scene-linear DCI-P3")
    {
        // Already in target space, nothing to do.
        ;
    }
    else if (colorspace == "camera Rec 709")
    {
        // Apply EOTF first, chromaticities change after, adjust whitepoint.
        v_color = (vector) Rec709_EOTF(input_color);

        transformed_color = color(
            dot(vector(REC709_TO_DCIP3_X), v_color),
            dot(vector(REC709_TO_DCIP3_Y), v_color),
            dot(vector(REC709_TO_DCIP3_Z), v_color));
    }
    else if (colorspace == "gamma 1.8 Rec 709")
    {
        // Apply EOTF first, chromaticities change after, adjusted whitepoint.
        v_color = (vector) gamma_EOTF(input_color, 1.8);

        transformed_color = color(
            dot(vector(REC709_TO_DCIP3_X), v_color),
            dot(vector(REC709_TO_DCIP3_Y), v_color),
            dot(vector(REC709_TO_DCIP3_Z), v_color));
    }
    else if (colorspace == "gamma 2.2 Rec 709")
    {
        // Apply EOTF first, chromaticities change after, adjusted whitepoint.
        v_color = (vector) gamma_EOTF(input_color, 2.2);

        transformed_color = color(
            dot(vector(REC709_TO_DCIP3_X), v_color),
            dot(vector(REC709_TO_DCIP3_Y), v_color),
            dot(vector(REC709_TO_DCIP3_Z), v_color));
    }
    else if (colorspace == "gamma 2.4 Rec 709 (video)")
    {
        // Apply EOTF first, chromaticities change after, adjusted whitepoint.
        v_color = (vector) gamma_EOTF(input_color, 2.4);

        transformed_color = color(
            dot(vector(REC709_TO_DCIP3_X), v_color),
            dot(vector(REC709_TO_DCIP3_Y), v_color),
            dot(vector(REC709_TO_DCIP3_Z), v_color)); 
    }
    else if (colorspace == "ACES2065-1")
    {
        // Linear encoding, chromaticities change with CAT for D60->DCI.
        transformed_color = color(
            dot(vector(ACES_TO_DCIP3_X), v_color),
            dot(vector(ACES_TO_DCIP3_Y), v_color),
            dot(vector(ACES_TO_DCIP3_Z), v_color));
    }
    else if (colorspace == "ACEScg")
    {
        // Linear encoding, chromaticities change with CAT for D60->DCI.
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
        warning("[WARNING]:Invalid color space selected in %s, %s:%i\n",
                shadername, __FILE__, __LINE__);
#endif
        transformed_color = color(0);
    }
    return transformed_color;
}

// ACES 2065-1 AP0 render space, D60 whitepoint
//
color transform_colorspace_to_ACES(
    color input_color,
    string colorspace)
{
    color transformed_color;
    vector v_color = vector(input_color);

    if (colorspace == "sRGB")
    {
        // apply EOTF first, then convert chromaticities, D65->D60.
        v_color = (vector) sRGB_EOTF(input_color);

        transformed_color = color(
            dot(vector(REC709_TO_ACES_X), v_color),
            dot(vector(REC709_TO_ACES_Y), v_color),
            dot(vector(REC709_TO_ACES_Z), v_color));
    }
    else if (colorspace == "scene-linear Rec 709/sRGB")
    {
        // Linear gamma already, convert chromaticities, adjust whitepoint.
        transformed_color = color(
            dot(vector(REC709_TO_ACES_X), v_color),
            dot(vector(REC709_TO_ACES_Y), v_color),
            dot(vector(REC709_TO_ACES_Z), v_color));
    }
    else if (colorspace == "scene-linear Rec 2020")
    {
        // Linear gamma already, convert chromaticities, adjust whitepoint.
        transformed_color = color(
            dot(vector(REC2020_TO_ACES_X), v_color),
            dot(vector(REC2020_TO_ACES_Y), v_color),
            dot(vector(REC2020_TO_ACES_Z), v_color));
        ;
    }
    else if (colorspace == "scene-linear DCI-P3")
    {
        // Linear gamma already, convert chromaticities, adjusted whitepoint.
        transformed_color = color(
            dot(vector(DCIP3_TO_ACES_X), v_color),
            dot(vector(DCIP3_TO_ACES_Y), v_color),
            dot(vector(DCIP3_TO_ACES_Z), v_color));
    }
    else if (colorspace == "camera Rec 709")
    {
        // Apply EOTF first, chromaticities change after, adjust whitepoint.
        v_color = (vector) Rec709_EOTF(input_color);

        transformed_color = color(
            dot(vector(REC709_TO_ACES_X), v_color),
            dot(vector(REC709_TO_ACES_Y), v_color),
            dot(vector(REC709_TO_ACES_Z), v_color));
    }
    else if (colorspace == "gamma 1.8 Rec 709")
    {
        // Apply EOTF first, chromaticities change after, adjusted whitepoint.
        v_color = (vector) gamma_EOTF(input_color, 1.8);

        transformed_color = color(
            dot(vector(REC709_TO_ACES_X), v_color),
            dot(vector(REC709_TO_ACES_Y), v_color),
            dot(vector(REC709_TO_ACES_Z), v_color));
    }
    else if (colorspace == "gamma 2.2 Rec 709")
    {
        // Apply EOTF first, chromaticities change after, adjusted whitepoint.
        v_color = (vector) gamma_EOTF(input_color, 2.2);

        transformed_color = color(
            dot(vector(REC709_TO_ACES_X), v_color),
            dot(vector(REC709_TO_ACES_Y), v_color),
            dot(vector(REC709_TO_ACES_Z), v_color));
    }
    else if (colorspace == "gamma 2.4 Rec 709 (video)")
    {
        // Apply EOTF first, chromaticities change after, adjusted whitepoint.
        v_color = (vector) gamma_EOTF(input_color, 2.4);

        transformed_color = color(
            dot(vector(REC709_TO_ACES_X), v_color),
            dot(vector(REC709_TO_ACES_Y), v_color),
            dot(vector(REC709_TO_ACES_Z), v_color));
    }
    else if (colorspace == "ACES2065-1")
    {
        // Same as target space, nothing to do here.
        ;
    }
    else if (colorspace == "ACEScg")
    {
        // Transform chromaticities only.
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
        warning("[WARNING]:Invalid color space selected in %s, %s:%i\n",
                shadername, __FILE__, __LINE__);
#endif
        transformed_color = color(0);
    }
    return transformed_color;
}

// ACEScg AP1 render space, D60 whitepoint
//
color transform_colorspace_to_ACEScg(
    color input_color,
    string colorspace)
{
    color transformed_color;
    vector v_color = vector(input_color);

    if (colorspace == "sRGB")
    {
        // apply EOTF first, then convert chromaticities, D65->D60.
        v_color = (vector) sRGB_EOTF(input_color);

        transformed_color = color(
            dot(vector(REC709_TO_ACESCG_X), v_color),
            dot(vector(REC709_TO_ACESCG_Y), v_color),
            dot(vector(REC709_TO_ACESCG_Z), v_color));
    }
    else if (colorspace == "scene-linear Rec 709/sRGB")
    {
        // Linear gamma already, convert chromaticities, adjust whitepoint.
        transformed_color = color(
            dot(vector(REC709_TO_ACESCG_X), v_color),
            dot(vector(REC709_TO_ACESCG_Y), v_color),
            dot(vector(REC709_TO_ACESCG_Z), v_color));
    }
    else if (colorspace == "scene-linear Rec 2020")
    {
        // Linear gamma already, convert chromaticities, adjust whitepoint.
        transformed_color = color(
            dot(vector(REC2020_TO_ACESCG_X), v_color),
            dot(vector(REC2020_TO_ACESCG_Y), v_color),
            dot(vector(REC2020_TO_ACESCG_Z), v_color));
        ;
    }
    else if (colorspace == "scene-linear DCI-P3")
    {
        // Linear gamma already, convert chromaticities, adjusted whitepoint.
        transformed_color = color(
            dot(vector(DCIP3_TO_ACESCG_X), v_color),
            dot(vector(DCIP3_TO_ACESCG_Y), v_color),
            dot(vector(DCIP3_TO_ACESCG_Z), v_color));
    }
    else if (colorspace == "camera Rec 709")
    {
        // Apply EOTF first, chromaticities change after, adjust whitepoint.
        v_color = (vector) Rec709_EOTF(input_color);

        transformed_color = color(
            dot(vector(REC709_TO_ACESCG_X), v_color),
            dot(vector(REC709_TO_ACESCG_Y), v_color),
            dot(vector(REC709_TO_ACESCG_Z), v_color));
    }
    else if (colorspace == "gamma 1.8 Rec 709")
    {
        // Apply EOTF first, chromaticities change after, adjusted whitepoint.
        v_color = (vector) gamma_EOTF(input_color, 1.8);

        transformed_color = color(
            dot(vector(REC709_TO_ACESCG_X), v_color),
            dot(vector(REC709_TO_ACESCG_Y), v_color),
            dot(vector(REC709_TO_ACESCG_Z), v_color));
    }
    else if (colorspace == "gamma 2.2 Rec 709")
    {
        // Apply EOTF first, chromaticities change after, adjusted whitepoint.
        v_color = (vector) gamma_EOTF(input_color, 2.2);

        transformed_color = color(
            dot(vector(REC709_TO_ACESCG_X), v_color),
            dot(vector(REC709_TO_ACESCG_Y), v_color),
            dot(vector(REC709_TO_ACESCG_Z), v_color));
    }
    else if (colorspace == "gamma 2.4 Rec 709 (video)")
    {
        // Apply EOTF first, chromaticities change after, adjusted whitepoint.
        v_color = (vector) gamma_EOTF(input_color, 2.4);

        transformed_color = color(
            dot(vector(REC709_TO_ACESCG_X), v_color),
            dot(vector(REC709_TO_ACESCG_Y), v_color),
            dot(vector(REC709_TO_ACESCG_Z), v_color));
    }
    else if (colorspace == "ACES2065-1")
    {
        // Transform chromaticities only, same white point.
        transformed_color = color(
            dot(vector(ACES_TO_ACESCG_X), v_color),
            dot(vector(ACES_TO_ACESCG_Y), v_color),
            dot(vector(ACES_TO_ACESCG_Z), v_color));
    }
    else if (colorspace == "ACEScg")
    {
        // Same as target space, nothing to do here.
        ;
    }
    else
    {
#ifdef DEBUG
        string shadername = "";
        getattribute("shader:shadername", shadername);
        warning("[WARNING]:Invalid color space selected in %s, %s:%i\n",
                shadername, __FILE__, __LINE__);
#endif
        transformed_color = color(0);
    }
    return transformed_color;
}

color transform_colorspace_to_workingspace(
    color input_color,
    string colorspace,
    string working_space)
{
    color C;

    if (working_space == "scene-linear Rec 709/sRGB")
    {
        C = transform_colorspace_to_Rec709(input_color, colorspace);
    }
    else if (working_space == "scene-linear Rec 2020")
    {
        C = transform_colorspace_to_Rec2020(input_color, colorspace);
    }
    else if (working_space == "scene-linear DCI-P3")
    {
        C = transform_colorspace_to_DCIP3(input_color, colorspace);
    }
    else if (working_space == "ACES2065-1")
    {
        C = transform_colorspace_to_ACES(input_color, colorspace);
    }
    else if (working_space == "ACEScg")
    {
        C = transform_colorspace_to_ACEScg(input_color, colorspace);
    }
    else
    {
#ifdef DEBUG
        string shadername = "";
        getattribute("shader:shadername", shadername);
        warning("[WARNING]:Unsupported working space \"%s\" in %s, %s:%i\n",
                working_space, shadername, __FILE__, __LINE__);
#endif
        C = color(0);
    }
    return C;
}

#endif // AS_COLOR_IDT_H
