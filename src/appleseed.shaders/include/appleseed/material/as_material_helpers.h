
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

#include "appleseed/math/as_math_helpers.h"

float microfacet_roughness(float roughness, float depth_scale)
{
    float out = roughness;

    if (depth_scale > 1.0)
    {
        int ray_depth;
        getattribute("path:ray_depth", ray_depth);

        if (ray_depth)
        {
            out = roughness * depth_scale * ray_depth;
        }
    }
    return out;
}

float ior_from_normal_reflectance(float f0)
{
    float sqrt_f0 = sqrt(f0);

    return (sqrt_f0 + 1) / (1 - sqrt_f0);
}


//
// Reference:
//
//      Artist Friendly Fresnel, Ole Gulbradson
//      http://jcgt.org/published/0003/04/03/
//

float get_eta(float f0, float f90)
{
    float eta_min = (1.0 - f0) / (1.0 + f0);
    float sqrt_f0 = sqrt(f0);
    float eta_max = (1.0 + sqrt_f0) / (1.0 - sqrt_f0);

    return mix(eta_max, eta_min, f90);
}

float get_kappa(float f0, float eta)
{
    float nr = f0 * sqr(eta + 1.0) - sqr(eta - 1.0);
    return sqrt(nr / (1.0 - f0));
}

color get_eta(color f0, color f90)
{
    return color(get_eta(f0[0], f90[0]),
                 get_eta(f0[1], f90[1]),
                 get_eta(f0[2], f90[2]));
}

color get_kappa(color f0, color eta)
{
    return color(get_kappa(f0[0], eta[0]),
                 get_kappa(f0[1], eta[1]),
                 get_kappa(f0[2], eta[2]));
}


//
//  Reference:
//
//      Memo on Fresnel Equations, Sebastian Lagarde
//      https://seblagarde.wordpress.com/2013/04/29/memo-on-fresnel-equations/
//

float dielectricDielectricFresnel(
    float etai,
    float etat,
    float costhetai)
{
    float eta_i, eta_t, costheta_i;

    if (backfacing())
    {
        eta_i = etai;
        eta_t = etat;
        costheta_i = costhetai;
    }
    else
    {
        eta_i = etat;
        eta_t = etai;
        costheta_i = abs(costhetai);
    }

    float sintheta_i = sqrt(max(0.0, 1.0 - sqr(costheta_i)));
    float sintheta_t = eta_i / eta_t * sintheta_i;

    if (sintheta_t >= 1.0)
    {
        return 1.0; // TIR
    }

    float costheta_t = sqrt(max(0.0, 1.0 - sqr(sintheta_t)));

    float eta_t_costheta_i = eta_t * costheta_i;
    float eta_t_costheta_t = eta_t * costheta_t;
    float eta_i_costheta_i = eta_i * costheta_i;
    float eta_i_costheta_t = eta_i * costheta_t;

    float Rs = (eta_t_costheta_i - eta_i_costheta_t) /
               (eta_t_costheta_i + eta_i_costheta_t);

    float Rp = (eta_i_costheta_i - eta_t_costheta_t) /
               (eta_i_costheta_i + eta_t_costheta_t);

    return (sqr(Rs) + sqr(Rp)) / 2.0;
}

float dielectricConductorFresnel(
    float etai,
    float etat,
    float k,
    float costhetai)
{
    float eta = etat / etai;
    float etak = k / etai;

    float costheta_i2 = sqr(costhetai);
    float sintheta_i2 = 1.0 - costheta_i2;
    float eta2 = sqr(eta);
    float etak2 = sqr(etak);

    float t0 = eta2 - etak2 - sintheta_i2;

    float a2b2sum = sqrt(sqr(t0) + 4.0 * eta2 * etak2);

    float t1 = a2b2sum + costheta_i2;

    float sqrt_a = sqrt(0.5 * (a2b2sum + t0));

    float t2 = 2.0 * costhetai * sqrt_a;

    float Rs = (t1 - t2) / (t1 + t2);

    float t3 = a2b2sum * costheta_i2 + sqr(sintheta_i2);
    float t4 = t2 * sintheta_i2;

    float Rp = Rs * (t3 - t4) / (t3 + t4);

    return 0.5 * (Rp + Rs);
}
