
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015 Francois Beaune, The appleseedhq Organization
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

// Interface header.
#include "bssrdf.h"

// appleseed.renderer headers.
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/project/project.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"

using namespace foundation;

/*

Quick ref:
----------

    sigma_a     absorption coeff.
    sigma_s     scattering coeff.
    g           anisotropy

    sigma_t         extinction coeff.           -> sigma_a + sigma_s
    sigma_s_prime   reduced scattering coeff.   -> sigma_s * (1 - g)
    sigma_t_prime   reduced extinction coeff.   -> sigma_a + sigma_s_prime
    sigma_tr        effective extinction coeff. -> sqrt( 3 * sigma_a * sigma_t_prime)

    Texture mapping:
    ----------------

    Fdr             fresnel diffuse reflectance
    A                                           -> (1 + Fdr) / (1 - Fdr)
    alpha_prime                                 -> sigma_s_prime / sigma_t_prime
    ld              mean free path              -> 1 / sigma_tr

    sigma_t_prime = sigma_tr / sqrt( 3 * (1 - alpha_prime))
    sigma_s_prime = alpha_prime * sigma_t_prime
    sigma_a = sigma_t_prime - sigma_s_prime
*/

namespace renderer
{

namespace
{

// A Better Dipole, Eugene d’Eon

template<typename T>
static T C1(const T eta)
{
    if (eta >= T(1.0))
        return (T(-9.23372) + eta * (T(22.2272) + eta * (T(-20.9292) + eta * (T(10.2291) + eta * (T(-2.54396) + T(0.254913) * eta))))) * T(0.5);
    else
        return (T(0.919317) + eta * (T(-3.4793) + eta * (T(6.75335) + eta * (T(-7.80989) + eta *(T(4.98554) - T(1.36881) * eta))))) * T(0.5);
}

template<typename T>
static T C2(const T eta)
{
    T r = T(-1641.1) + eta * (T(1213.67) + eta * (T(-568.556) + eta * (T(164.798) + eta * (T(-27.0181) + T(1.91826) * eta))));
    r += (((T(135.926) / eta) - T(656.175)) / eta + T(1376.53)) / eta;
    return r * T(0.333333);
}

// Texture mapping for the Better Dipole model, Christophe Hery

template<typename T>
T compute_rd(T alpha_prime, T two_c1, T three_c2)
{
    T cphi = T(0.25) * (T(1.0) - two_c1);
    T ce = T(0.5) * (T(1.0) - three_c2);
    T mu_tr_D = std::sqrt((T(1.0) - alpha_prime) * (T(2.0) - alpha_prime) / T(3.0));
    T myexp = std::exp(-((T(1.0) + three_c2) / cphi) * mu_tr_D);
    return T(0.5) * square(alpha_prime) * std::exp(-std::sqrt(T(3.0) * (T(1.0) - alpha_prime) / (T(2.0) - alpha_prime))) * (ce * (T(1.0) + myexp) + cphi / mu_tr_D * (T(1.0) - myexp));
}

template<typename T>
T compute_alpha_prime(T rd, T C1, T C2)
{
    const T C12 = T(2.0) * C1;
    const T C23 = T(3.0) * C2;

    T x0 = T(0), x1 = T(1), xmid;

    // For now simple bisection.
    for (int i = 0, iters = 50; i < iters; ++i)
    {
        xmid = T(0.5) * (x0 + x1);
        const T f = compute_rd(xmid, C12, C23);
        f < rd ? x0 = xmid : x1 = xmid;
    }

    return xmid;
}

}

//
// BSSRDF class implementation.
//

namespace
{
    const UniqueID g_class_uid = new_guid();
}

UniqueID BSSRDF::get_class_uid()
{
    return g_class_uid;
}

BSSRDF::BSSRDF(
    const char*             name,
    const ParamArray&       params)
  : ConnectableEntity(g_class_uid, params)
  , m_lighting_conditions(0)
{
    set_name(name);
}

bool BSSRDF::on_frame_begin(
    const Project&          project,
    const Assembly&         assembly,
    IAbortSwitch*           abort_switch)
{
    m_lighting_conditions = &project.get_frame()->get_lighting_conditions();
    return true;
}

void BSSRDF::on_frame_end(
    const Project&          project,
    const Assembly&         assembly)
{
    m_lighting_conditions = 0;
}

size_t BSSRDF::compute_input_data_size(
    const Assembly&         assembly) const
{
    size_t s = get_inputs().compute_data_size();
    assert( s >= sizeof(BSSRDFInputValues));
    return s;
}

void BSSRDF::evaluate_inputs(
    const ShadingContext&   shading_context,
    InputEvaluator&         input_evaluator,
    const ShadingPoint&     shading_point,
    const size_t            offset) const
{
    input_evaluator.evaluate(get_inputs(), shading_point.get_uv(0), offset);
}

void BSSRDF::subsurface_from_diffuse(
    const Spectrum&    diffuse,
    const Spectrum&    mfp,
    Spectrum&          sigma_s_prime,
    Spectrum&          sigma_a)
{
    assert(diffuse.size() == mfp.size());

    sigma_s_prime.resize(diffuse.size());
    sigma_a.resize(diffuse.size());

    const double eta = 1.3;
    const double c1 = C1(eta);
    const double c2 = C2(eta);

    for (int i = 0, e = diffuse.size(); i < e; ++i)
    {
        const double alpha_prime = compute_alpha_prime(
            clamp(static_cast<double>(diffuse[i]), 0.0, 1.0),
            c1,
            c2);

        const double sigma_tr = 1.0 / mfp[i];
        const double sigma_t_prime =
            sigma_tr / std::sqrt( 3.0 * (1.0 - alpha_prime));

        sigma_s_prime[i] = alpha_prime * sigma_t_prime;
        sigma_a[i] = sigma_t_prime - sigma_s_prime[i];
    }
}

}   // namespace renderer
