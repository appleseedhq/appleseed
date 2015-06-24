
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015 Esteban Tovagliari, The appleseedhq Organization
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
#include "directionaldipolebssrdf.h"

// appleseed.renderer headers.
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/input/inputevaluator.h"

// appleseed.foundation headers.
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"

using namespace foundation;

namespace renderer
{

namespace
{

// Directional dipole profile
template<typename T>
T sp_d(
    const T                     sigma_a,
    const T                     sigma_s_prime,
    const T                     dot_xw,
    const T                     dot_wn,
    const T                     dot_xn,
    const T                     r)
{
    // constants
    const T four_pisq_rcp = 0.0253303;  // 1.0 / (4.0 * square(Pi));
    const T cp_norm = T(1.06509);       // 1.0 / (1.0 - 2.0 * C1(1.0 / eta));
    const T cp = T(0.138676);           // (1.0 - 2.0 * C1(eta)) / 4.0
    const T ce = T(0.351276);           // (1.0 - 3.0 * C2(eta)) / 2.0;

    const T r2 = square(r);
    const T r3_rcp = T(1.0) / (r2 * r);

    const T sigma_t_prime = sigma_a + sigma_s_prime;
    const T D = T(1.0) / (T(3.0) * sigma_t_prime);
    const T sigma_tr = std::sqrt(sigma_a / D);
    const T s_tr_r = sigma_tr * r;
    const T s_tr_r_one = T(1.0) + s_tr_r;
    const T t0 = cp_norm * four_pisq_rcp * std::exp(-s_tr_r) * r3_rcp;
    const T t1 = r2 / D + T(3.0) * s_tr_r_one * dot_xw;
    const T t2 = T(3.0) * D * s_tr_r_one * dot_wn;
    const T t3 = (s_tr_r_one + T(3.0) * D * (T(3.0) * s_tr_r_one + square(s_tr_r)) / r2 * dot_xw) * dot_xn;
    return t0 * (cp * t1 - ce * (t2 - t3));
}

template<typename T>
void bssrdf(
    const BSSRDFInputValues*    values,
    const Vector<T,3>&          xi,
    const Vector<T,3>&          ni,
    const Vector<T,3>&          wi,
    const Vector<T,3>&          xo,
    const Vector<T,3>&          no,
    const Vector<T,3>&          wo,
    Spectrum&                   result)
{
    // distance
    const Vector<T,3> xoxi = xo - xi;
    const T r = norm(xoxi);
    const T r2 = square(r);

    // modified normal
    const Vector<T,3> ni_s = cross(normalize(xoxi), normalize(cross(ni, xoxi)));

    // directions of ray sources
    const T eta = T(1.3);
    const T nnt = T(1.0) / eta;
    const T ddn = -dot(wi, ni);
    Vector<T,3> wr =
        normalize(wi * -nnt - ni * (ddn * nnt + std::sqrt(1.0 - nnt * nnt * (1.0 - ddn * ddn))));
    const Vector<T,3> wv = wr - ni_s * (2.0 * dot(ni_s, wr));

    const T cp = T(0.138676);       // (1.0 - 2.0 * C1(eta)) / 4.0
    const T ce = T(0.351276);       // (1.0 - 3.0 * C2(eta)) / 2.0;
    const T A = (1.0 - ce) / (2.0 * cp);

    const T dot_xiwr = dot(xoxi, wr);
    const T dot_wrn = dot(wr, no);
    const T dot_xin = dot(xoxi, no);
    const T dot_wvn = dot(wv, no);

    for (int i = 0, e = result.size(); i < e; ++i)
    {
        const T sigma_a = values->m_sigma_a[i];
        const T sigma_s_prime = values->m_sigma_s_prime[i];
        const T sigma_t_prime = sigma_a + sigma_s_prime;
        const T D = T(1.0) / (T(3.0) * sigma_t_prime);
        const T alpha_prime = sigma_s_prime / sigma_t_prime;
        const T de = T(2.131) * D / std::sqrt(alpha_prime);

        // assume g == 0 for now.
        const T sigma_t = sigma_t_prime;

        // distance to real sources
        const T cos_beta = -std::sqrt((r2 - square(dot(wr, xoxi))) / (r2 + square(de)));

        T dr;

        if (dot_wrn > T(0.0))
            dr = std::sqrt((D * dot_wrn) * ((D * dot_wrn) - de * cos_beta * T(2.0)) + r2);
        else
            dr = std::sqrt(T(1.0) / square(3.0 * sigma_t) + r2);

        // distance to virtual source
        const Vector<T,3> xoxv = xo - (xi + ni_s * (T(2.0) * A * de));
        const T dv = norm(xoxv);

        // BSSRDF
        const T sp_i = sp_d(
            sigma_a,
            sigma_s_prime,
            dot_xiwr,
            dot_wrn,
            dot_xin,
            dr);

        const T sp_v = sp_d(
            sigma_a,
            sigma_s_prime,
            dot(xoxv, wv),
            dot_wvn,
            dot(xoxv, no),
            dv);

        result[i] += std::max(sp_i - sp_v, T(0));
    }
}

}

namespace
{

    const char* Model = "directional_dipole_bssrdf";

    //
    // Directional dipole BSSRDF.
    //

    // reference: dirpole, directional dipole by T. Hachisuka and J. R. Frisvad

    class DirectionalDipoleBSSRDF
      : public BSSRDF
    {
      public:
        DirectionalDipoleBSSRDF(
            const char*                 name,
            const ParamArray&           params)
          : BSSRDF(name, params)
        {
        }

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const APPLESEED_OVERRIDE
        {
            return Model;
        }

        virtual void evaluate_inputs(
            const ShadingContext&       shading_context,
            InputEvaluator&             input_evaluator,
            const ShadingPoint&         shading_point,
            const size_t                offset = 0) const APPLESEED_OVERRIDE
        {
            BSSRDF::evaluate_inputs(shading_context, input_evaluator, shading_point, offset);

            DirectionalDipoleBSSRDFInputValues* values =
                reinterpret_cast<DirectionalDipoleBSSRDFInputValues*>(input_evaluator.data());

            if (values->m_diffuse.size() != values->m_mfp.size())
            {
                if (values->m_diffuse.is_spectral())
                    Spectrum::upgrade(values->m_mfp, values->m_mfp);
                else
                    values->m_mfp.convert_to_rgb(*m_lighting_conditions);
            }

            // convert in place.
            subsurface_from_diffuse(
                values->m_diffuse,
                values->m_mfp,
                values->m_diffuse,
                values->m_mfp);
        }

        void evaluate(
            const void*                 data,                       // input values
            const Vector3d&             outgoing_point,
            const Vector3d&             outgoing_normal,
            const Vector3d&             outgoing_dir,
            const Vector3d&             incoming_point,
            const Vector3d&             incoming_normal,
            const Vector3d&             incoming_dir,
            Spectrum&                   value) const
        {
            const BSSRDFInputValues* values =
                reinterpret_cast<const BSSRDFInputValues*>(data);

            value.resize(values->m_sigma_a.size());
            value.set(0.0f);

            bssrdf(
                values,
                incoming_point,
                incoming_normal,
                incoming_dir,
                outgoing_point,
                outgoing_normal,
                outgoing_dir,
                value);

            // reciprocal evaluation with the reciprocity hack.
            bssrdf(
                values,
                outgoing_point,
                outgoing_normal,
                outgoing_dir,
                incoming_point,
                incoming_normal,
                incoming_dir,
                value);

            value *= 0.5f;
        }

    };
}

//
// DirectionalDipoleBSSRDFFactory class implementation.
//

const char* DirectionalDipoleBSSRDFFactory::get_model() const
{
    return Model;
}

Dictionary DirectionalDipoleBSSRDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Directional Dipole BSSRDF");
}

DictionaryArray DirectionalDipoleBSSRDFFactory::get_input_metadata() const
{
    DictionaryArray metadata;
    return metadata;
}

auto_release_ptr<BSSRDF> DirectionalDipoleBSSRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSSRDF>(new DirectionalDipoleBSSRDF(name, params));
}

}   // namespace renderer
