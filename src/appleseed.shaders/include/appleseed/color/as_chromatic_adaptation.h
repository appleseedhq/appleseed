
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

#ifndef AS_CHROMATIC_ADAPTATION_H
#define AS_CHROMATIC_ADAPTATION_H

#include "appleseed/color/as_colorimetry.h"

//
// Reference:
//
//      Two New von Kries Based Chromatic Adaptation Transforms Found By
//      Numerical Optimization
//
//      www.ivl.disco.unimib.it/download/bianco2010two-new.pdf
//
//      Performance Of Five Chromatic Adaptation Transforms Using Large
//      Number Of Color Patches
//
//      http://hrcak.srce.hr/file/95370
//     

void choose_CAT(string CAT, output vector CAT_matrix[3])
{
    if (CAT == "Bradford")
    {
        vector CAT_Bradford[3] = {
            vector( 0.8951000,  0.2664000, -0.1614000),
            vector(-0.7502000,  1.7135000,  0.0367000),
            vector( 0.0389000, -0.0685000,  1.0296000)};

        CAT_matrix = CAT_Bradford;
    }
    else if (CAT == "vonKries")
    {
        vector CAT_VonKries[3] = {
            vector(  0.4002400,  0.7076000, -0.0808100),
            vector( -0.2263000,  1.1653200,  0.0457000),
            vector(  0.0000000,  0.0000000,  0.9182200)};
    }
    else if (CAT == "CMCCAT97")
    {
        vector CAT_CMCCAT97[3] = {
            vector(  0.8951000, -0.7502000,  0.0389000),
            vector(  0.2664000,  1.7135000,  0.0685000),
            vector( -0.1614000,  0.0367000,  1.0296000)};

        CAT_matrix = CAT_CMCCAT97;
    }
    else if (CAT == "CMCCAT2000")
    {
        vector CAT_CMCCAT2000[3] = {
            vector(  0.7982000,  0.3389000, -0.1371000),
            vector( -0.5918000,  1.5512000,  0.0406000),
            vector(  0.0008000,  0.0239000,  0.9753000)};

        CAT_matrix = CAT_CMCCAT2000;
    }
    else if (CAT == "Sharp")
    {
        vector CAT_Sharp[3] = {
            vector(  1.2694000,  0.0988000, -0.1706000),
            vector( -0.8364000,  1.8006000,  0.0357000),
            vector(  0.0297000, -0.0315000,  1.0018000)};

        CAT_matrix = CAT_Sharp;
    }
    else if (CAT == "CAT02")
    {
        vector CAT_CAT02[3] = {
            vector(  0.7328000,  0.4296000, -0.1624000),
            vector( -0.7036000,  1.6975000,  0.0061000),
            vector(  0.0030000,  0.0136000,  0.9834000)};

        CAT_matrix = CAT_CAT02;
    }
    else if (CAT == "Bianco")
    {
        vector CAT_Bianco[3] = {
            vector(  0.8752000,  0.2787000, -0.1539000),
            vector( -0.8904000,  1.8709000,  0.0195000),
            vector( -0.0061000,  0.0162000,  0.9899000)};

        CAT_matrix = CAT_Bianco;
    }
    else if (CAT == "BiancoPC")
    {
        vector CAT_BiancoPC[3] = {
            vector(  0.6489000,  0.3915000, -0.0404000),
            vector( -0.3775000,  1.3055000,  0.0720000),
            vector( -0.0271000,  0.0888000,  0.9383000)};

        CAT_matrix = CAT_BiancoPC;
    }
    else
    {
        // Fallback to XYZ scaling (aka "fake von Kries").
#ifdef DEBUG
        string shadername = "";
        getattribute("shader:shadername", shadername);

        warning("[WARNING]: No CAT or Invalid CAT selected in %s, %s:%i\n",
                shadername, __FILE__, __LINE__);
#endif
        CAT_matrix[0] = vector(1.0, 0.0, 0.0);
        CAT_matrix[1] = vector(0.0, 1.0, 0.0);
        CAT_matrix[2] = vector(0.0, 0.0, 1.0);
    }
}

// 
// Reference:
//
//      A Review Of Chromatic Adaptation Transforms
//
//      http://cie.mogi.bme.hu/cie_arch/kee/div1/tc152.pdf
//

matrix chromatic_adaptation_vonKries(
    vector source_whitepoint_XYZ,
    vector target_whitepoint_XYZ,
    string CAT)
{
    vector CAT_matrix[3];

    choose_CAT(CAT, CAT_matrix);

    vector source_whitepoint_LMS = vector(
        dot(CAT_matrix[0], source_whitepoint_XYZ),
        dot(CAT_matrix[1], source_whitepoint_XYZ),
        dot(CAT_matrix[2], source_whitepoint_XYZ));

    vector target_whitepoint_LMS = vector(
        dot(CAT_matrix[0], target_whitepoint_XYZ),
        dot(CAT_matrix[1], target_whitepoint_XYZ),
        dot(CAT_matrix[2], target_whitepoint_XYZ));

    vector cone_response_ratio =
        target_whitepoint_LMS / source_whitepoint_LMS;

    matrix diagonal_adaptation_matrix = matrix(
        cone_response_ratio[0], 0.0, 0.0, 0.0,
        0.0, cone_response_ratio[1], 0.0, 0.0,
        0.0, 0.0, cone_response_ratio[2], 0.0,
        0.0, 0.0, 0.0, 1.0);

    matrix CAT_matrix_4x4 = matrix(
        CAT_matrix[0][0], CAT_matrix[0][1], CAT_matrix[0][2], 0.0,
        CAT_matrix[1][0], CAT_matrix[1][1], CAT_matrix[1][2], 0.0,
        CAT_matrix[2][0], CAT_matrix[2][1], CAT_matrix[2][2], 0.0,
        0.0, 0.0, 0.0, 1.0);

    matrix chromatic_adaptation_matrix = inverse(CAT_matrix_4x4) *
        diagonal_adaptation_matrix * CAT_matrix_4x4;

    return chromatic_adaptation_matrix;
}

void chromatic_adaptation_vonKries(
    vector source_whitepoint_XYZ,
    vector target_whitepoint_XYZ,
    string CAT,
    output vector CAM[3])
{
    matrix chromatic_adaptation_matrix =
        chromatic_adaptation_vonKries(
            source_whitepoint_XYZ,
            target_whitepoint_XYZ,
            CAT);

    CAM[0] = vector(
        chromatic_adaptation_matrix[0][0],
        chromatic_adaptation_matrix[0][1],
        chromatic_adaptation_matrix[0][2]);

    CAM[1] = vector(
        chromatic_adaptation_matrix[1][0],
        chromatic_adaptation_matrix[1][1],
        chromatic_adaptation_matrix[1][2]);

    CAM[2] = vector(
        chromatic_adaptation_matrix[2][0],
        chromatic_adaptation_matrix[2][1],
        chromatic_adaptation_matrix[2][2]);
}

#endif // !AS_CHROMATIC_ADAPTATION_H
