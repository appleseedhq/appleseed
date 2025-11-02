
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Rafael Brune, The appleseedhq Organization
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

// based on "A Composite BRDF Model for Hazy Gloss"
// by P. Barla, R. Pacanowski and P. Vangorp
// Eurographics Symposium on Rendering 2018

#pragma once

#include "appleseed/material/as_material_helpers.h"






closure color as_double_ggx_gloss
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



closure color as_double_ggx_metal
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



closure color as_double_ggx_glass
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





void haze_mapping_to_ggx
(
    float reflectivity,
    float roughness,
    float haziness,
    float haze_extent,
    output float eta,
    output float beta,
    output float alpha_n,
    output float alpha_w
)
{
	float r_c = reflectivity;
	alpha_n = roughness;
	float beta_h = haziness;
	float alpha_h = haze_extent;
    float w = 5; // interpolation weight for discontinuity

	alpha_w = alpha_n*(1+alpha_h); // wide roughness
	float p = 1.0/sqr(1+alpha_h); // peak ratio formula for iso Beckmann/GGX
	float k_h = 0.0;

	if (1){ // smooth out C1 discontinuity at rp = p
		float b = 2*(r_c*(1-w)+w*p);
		float u; // parametric coordinate for rational Bezier curve
		if (abs(2*(b-1))<EPS){ // use Taylor expansion around singularity
			u = (2*w*p-1- (4*sqr(w)*sqr(p)-4*sqr(w)*p+1)*(r_c-(0.5-w*p)/(1-w)) ) / (2*(w-1)) ;
		} else {
			float D = sqr(b) - 4*(b-1)*r_c;
			u = (b-sqrt(D)) / (2*(b-1));
		}
		k_h = (2*(1-u)*u*w*beta_h)/(sqr(1-u)+2*(1-u)*u*w+sqr(u)); // rational Bezier curve
	} else { // interpolation between 0 and positivity or energy constraints
		k_h = (r_c > p) ? beta_h*(1-r_c)/(1-p) : beta_h*r_c/p;
	}
	float r = r_c + (1-p)*k_h; // compound reflectivity

	beta = k_h/r; // mixture ratio
	//eta = (1+sqrt(r))/(1-sqrt(r)); // refractive index
    eta = r; // for reusability we do the conversion to ior outside
}



closure color as_hazy_gloss
(
    vector Nn,
    vector tangent,
    float in_specular_roughness,
    float in_specular_haziness,
    float in_specular_haze_extent,
    float in_anisotropy_amount,
    float in_ior
)
{
    float reflectivity = sqr((in_ior - 1.0) / (in_ior + 1.0));
    float alpha_n = sqr(in_specular_roughness); // map to microfacet alpha
    float haze_extent = 10.0 * in_specular_haze_extent; // map to a larger range

    float eta;
    float beta;
    float alpha_w;
    haze_mapping_to_ggx(reflectivity, alpha_n, in_specular_haziness, haze_extent, eta, beta, alpha_n, alpha_w);

    float roughness_2 = sqrt(alpha_w);
    float new_ior = (1+sqrt(eta))/(1-sqrt(eta));

    return as_double_ggx_gloss(
        Nn,
        tangent,
        in_specular_roughness,
        roughness_2,
        beta,
        in_anisotropy_amount,
        new_ior
    );
}



closure color as_hazy_glass
(
    vector Nn,
    vector tangent,
    color coating_transmittance,
    color in_specular_color,
    color in_scaled_refraction_tint,
    float in_specular_roughness,
    float in_specular_haziness,
    float in_specular_haze_extent,
    float in_anisotropy_amount,
    float in_ior,
    color in_absorption_color,
    float in_absorption_depth
)
{
    float reflectivity = sqr((in_ior - 1.0) / (in_ior + 1.0));
    float alpha_n = sqr(in_specular_roughness); // map to microfacet alpha
    float haze_extent = 10.0 * in_specular_haze_extent; // map to a larger range

    float eta;
    float beta;
    float alpha_w;
    haze_mapping_to_ggx(reflectivity, alpha_n, in_specular_haziness, haze_extent, eta, beta, alpha_n, alpha_w);

    float roughness_2 = sqrt(alpha_w);
    float new_ior = (1+sqrt(eta))/(1-sqrt(eta));

    return as_double_ggx_glass(
        Nn,
        tangent,
        coating_transmittance,
        in_specular_color,
        in_scaled_refraction_tint,
        in_specular_roughness,
        roughness_2,
        beta,
        in_anisotropy_amount,
        new_ior,
        in_absorption_color,
        in_absorption_depth
    );
}


closure color as_hazy_metal
(
    vector Nn,
    vector tangent,
    color in_specular_color,
    color in_face_tint,
    color in_edge_tint,
    float in_specular_roughness,
    float in_specular_haziness,
    float in_specular_haze_extent,
    float in_anisotropy_amount
)
{
    float reflectivity = max(in_specular_color);
    float alpha_n = sqr(in_specular_roughness); // map to microfacet alpha
    float haze_extent = 10.0 * in_specular_haze_extent; // map to a larger range

    float eta;
    float beta;
    float alpha_w;
    haze_mapping_to_ggx(reflectivity, alpha_n, in_specular_haziness, haze_extent, eta, beta, alpha_n, alpha_w);

    float roughness_2 = sqrt(alpha_w);

    return eta/reflectivity * as_double_ggx_metal(
        Nn,
        tangent,
        in_face_tint,
        in_edge_tint,
        in_specular_roughness,
        roughness_2,
        beta,
        in_anisotropy_amount
    );
}
