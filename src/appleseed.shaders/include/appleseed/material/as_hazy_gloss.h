
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Rafael Brune, The appleseedhq Organization
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

closure color as_hazy_gloss
(
    vector Nn,
    vector tangent,
    float in_specular_roughness,
    float in_specular_roughness_2,
    float in_specular_roughness_blend,
    float in_anisotropy_amount,
    float in_ior
)
{
    closure color out_outColor = as_glossy(
        "ggx",
        Nn,
        tangent,
        in_specular_roughness,
        0.0,
        in_anisotropy_amount,
        in_ior);

    if (in_specular_roughness_blend != 0.0)
    {
        out_outColor *= (1.0 - in_specular_roughness_blend);
        out_outColor += in_specular_roughness_blend * as_glossy(
            "ggx",
            Nn,
            tangent,
            in_specular_roughness_2,
            0.0,
            in_anisotropy_amount,
            in_ior);
    }

    return out_outColor;
}



closure color as_hazy_metal
(
    vector Nn,
    vector tangent,
    color in_face_tint,
    color in_edge_tint,
    float in_specular_roughness,
    float in_specular_roughness_2,
    float in_specular_roughness_blend,
    float in_anisotropy_amount
)
{
    closure color out_outColor = as_metal(
        "ggx",
        Nn,
        tangent,
        in_face_tint,
        in_edge_tint,
        in_specular_roughness,
        0.0,
        in_anisotropy_amount);

    if (in_specular_roughness_blend != 0.0)
    {
        out_outColor *= (1.0 - in_specular_roughness_blend);
        out_outColor += in_specular_roughness_blend * as_metal(
            "ggx",
            Nn,
            tangent,
            in_face_tint,
            in_edge_tint,
            in_specular_roughness_2,
            0.0,
            in_anisotropy_amount);
    }

    return out_outColor;
}



closure color as_hazy_glass
(
    vector Nn,
    vector tangent,
    color coating_transmittance,
    color in_specular_color,
    color in_scaled_refraction_tint,
    float in_specular_roughness,
    float in_specular_roughness_2,
    float in_specular_roughness_blend,
    float in_anisotropy_amount,
    float in_ior,
    color in_absorption_color,
    float in_absorption_depth
)
{
    closure color out_outColor = as_glass(
        "ggx",
        Nn,
        tangent,
        coating_transmittance,
        in_specular_color,
        in_scaled_refraction_tint,
        in_specular_roughness,
        0.0,
        in_anisotropy_amount,
        in_ior,
        in_absorption_color,
        in_absorption_depth);

    if (in_specular_roughness_blend != 0.0)
    {
        out_outColor *= (1.0 - in_specular_roughness_blend);
        out_outColor += in_specular_roughness_blend * as_glass(
            "ggx",
            Nn,
            tangent,
            coating_transmittance,
            in_specular_color,
            in_scaled_refraction_tint,
            in_specular_roughness_2,
            0.0,
            in_anisotropy_amount,
            in_ior,
            in_absorption_color,
            in_absorption_depth);
    }

    return out_outColor;
}
