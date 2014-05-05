
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_MATH_MICROFACET2_H
#define APPLESEED_FOUNDATION_MATH_MICROFACET2_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cmath>

namespace foundation
{

template <typename T>
struct MDF : NonCopyable
{
    virtual ~MDF() {}

    inline Vector<T,3> sample(
        const Vector<T,2>& s,
        const T alpha_x,
        const T alpha_y) const
    {
        assert(s[0] >= 0.0);
        assert(s[0] < 1.0);
        assert(s[1] >= 0.0);
        assert(s[1] < 1.0);
        Vector<T,3> result = do_sample(s, alpha_x, alpha_y);
        return result;
    }
    
    inline T D(
        const Vector<T,3>& h,
        const T alpha_x,
        const T alpha_y) const
    {
        T result = do_eval_D(h, alpha_x, alpha_y);
        assert(result >= 0.0);
        return result;
    }

    inline T G(
        const Vector<T,3>& incoming,
        const Vector<T,3>& outgoing,
        const Vector<T,3>& h,
        const T alpha_x,
        const T alpha_y) const
    {
        T result = do_eval_G(incoming, outgoing, h, alpha_x, alpha_y);
        assert(result >= 0.0);
        return result;
    }

    inline T pdf(
        const Vector<T,3>& h,
        const T alpha_x,
        const T alpha_y) const
    {
        T pdf = do_eval_pdf(h, alpha_x, alpha_y);
        assert(pdf >= 0.0);
        return pdf;
    }

  private:
    virtual Vector<T,3> do_sample(
        const Vector<T,2>& s,
        const T alpha_x,
        const T alpha_y) const = 0;

    // h is in shading basis space.
    virtual T do_eval_D(
        const Vector<T,3>& h,
        const T alpha_x,
        const T alpha_y) const = 0;

    // incoming and outgoing are in shading basis space.
    virtual T do_eval_G(
        const Vector<T,3>& incoming,
        const Vector<T,3>& outgoing,
        const Vector<T,3>& h,
        const T alpha_x,
        const T alpha_y) const
    {
        const T cos_hn = h.y;
        const T cos_on = outgoing.y;
        const T cos_in = incoming.y;
        const T cos_oh = dot(outgoing, h);
        const T rcp_cos_oh = T(1) / cos_oh;
        const T a = T(2) * cos_hn * cos_on * rcp_cos_oh;
        const T b = T(2) * cos_hn * cos_in * rcp_cos_oh;
        return min(a, b, T(1));
        return 0;
    }

    // h is in shading basis space.
    virtual T do_eval_pdf(
        const Vector<T,3>& h,
        const T alpha_x,
        const T alpha_y) const = 0;
};

// Anisotropic Beckmann distribution.
template <typename T>
struct BeckmannMDF2
  : public MDF<T>
{
  private:
    virtual Vector<T,3> do_sample(
        const Vector<T,2>& s,
        const T alpha_x,
        const T alpha_y) const OVERRIDE
    {
        // Same sampling procedure as for the Ward distribution.
        const T alpha_x2 = square(alpha_x);
        const T tan_alpha_2 = alpha_x2 * (-std::log(T(1.0) - s[0]));
        const T cos_alpha = T(1.0) / std::sqrt(T(1.0) + tan_alpha_2);
        const T sin_alpha = cos_alpha * std::sqrt(tan_alpha_2);
        const T phi = TwoPi * s[1];
    
        return Vector<T, 3>::unit_vector(cos_alpha, sin_alpha, std::cos(phi), std::sin(phi));
    }

    virtual T do_eval_D(
        const Vector<T,3>& h,
        const T alpha_x,
        const T alpha_y) const OVERRIDE
    {
        assert(h.y >= T(0));
    
        if (h.y == T(0.0))
            return T(0.0);
    
        const T cos_alpha_2 = h.y * h.y;
        const T cos_alpha_4 = cos_alpha_2 * cos_alpha_2;
        const T tan_alpha_2 = (T(1.0) - cos_alpha_2) / cos_alpha_2;
    
        // Note: in [2] there's a missing Pi factor in the denominator.
        const T alpha_x2 = square(alpha_x);
        return std::exp(-tan_alpha_2 / alpha_x2) / (alpha_x2 * T(Pi) * cos_alpha_4);
    }
    
    virtual T do_eval_pdf(
        const Vector<T,3>& h,
        const T alpha_x,
        const T alpha_y) const OVERRIDE
    {
        assert(h.y >= T(0.0));
    
        if (h.y == T(0.0))
            return T(0.0);
    
        const T cos_alpha_2 = h.y * h.y;
        const T cos_alpha_3 = h.y * cos_alpha_2;
        const T tan_alpha_2 = (T(1.0) - cos_alpha_2) / cos_alpha_2;
        const T alpha_x2 = square(alpha_x);        
        return std::exp(-tan_alpha_2 / alpha_x2) / (alpha_x2 * T(Pi) * cos_alpha_3);
    }
};

// Anisotropic GGX (GRT2) distribution.
template <typename T>
struct GGXMDF2
  : public MDF<T>
{
  private:
    virtual Vector<T,3> do_sample(
        const Vector<T,2>& s,
        const T alpha_x,
        const T alpha_y) const OVERRIDE
    {
        /*
        // anisotropic case
        if (alpha_x != alpha_y)
        {
            T cos_phi = std::cos(2 * Pi * s[0]) * alpha_x;
            T sin_phi = std::sin(2 * Pi * s[0]) * alpha_y;
            const T invnorm = 1 / std::sqrt(cos_phi * cos_phi + sin_phi * sin_phi);
            cos_phi *= invnorm;
            sin_phi *= invnorm;
            
            const T C = (cos_phi / alpha_x) * (cos_phi / alpha_x) + (sin_phi / alpha_y) * (sin_phi / alpha_y);
            const T tan_theta2 = s[1] / ((1 - s[1]) * C);
            const T cos_theta  = 1 / std::sqrt(1 + tan_theta2);
            const T sin_theta  = cos_theta * std::sqrt(tan_theta2);
            return Vector<T,3>(cos_phi * sin_theta, sin_phi * sin_theta, cos_theta);
        }
        
        // isotropic case
        const T tan_theta_m2 = square(alpha_x) * s[0] / (1 - s[0]);
        const T cos_theta_m  = 1 / std::sqrt(1 + tan_theta_m2);
        const T sin_theta_m  = cos_theta_m * std::sqrt(tan_theta_m2);
        const T phi_m = T(2) * Pi * s[1];
        return Vector<T,3>(std::cos(phi_m) * sin_theta_m, std::sin(phi_m) * sin_theta_m, cos_theta_m);
        */
        return Vector<T,3>(0, 0, 1);
    }
    
    virtual T do_eval_D(
        const Vector<T,3>& h,
        const T alpha_x,
        const T alpha_y) const OVERRIDE
    {
        /*
        const T alpha_x2 = square(alpha_x);
        const T cos_theta_m = h[2];
        const T cos_theta_m2 = square(cos_theta_m);
        const T cos_theta_m4 = square(cos_theta_m2);
        
        if (alpha_x != alpha_y)
        {
            const T alpha_y2 = alpha_y * alpha_y;
            const T sin_theta_m = std::sqrt(std::max(T(1) - cos_theta_m2, T(0)));
            const T inv_sin_theta_m = sin_theta_m > 0 ? 1 / sin_theta_m : 0;
            const T cos_phi2 = square(h.x * inv_sin_theta_m);
            const T sin_phi2 = square(h.y * inv_sin_theta_m);
            const T tan_theta_m2 = (sin_theta_m * sin_theta_m) / cos_theta_m2;
            const T tmp = 1 + tan_theta_m2 * (cos_phi2 / alpha_x2 + sin_phi2 / alpha_y2);
            return 1 / (Pi * alpha_x * alpha_y * cos_theta_m4 * square(tmp));
        }
 
        T tan_theta_m2 = (1 - cos_theta_m2) / cos_theta_m2;
        return alpha_x2 / (Pi * cos_theta_m4 * square(alpha_x2 + tan_theta_m2));
        */
        return 0;
    }
    
    virtual T do_eval_G(
        const Vector<T,3>& incoming,
        const Vector<T,3>& outgoing,
        const Vector<T,3>& h,
        const T alpha_x,
        const T alpha_y) const OVERRIDE
    {
        //return G(incoming, alpha_x, alpha_y) * G(outgoing, alpha_x, alpha_y);
        return 0;
    }

    virtual T do_eval_pdf(
        const Vector<T,3>& h,
        const T alpha_x,
        const T alpha_y) const OVERRIDE
    {
        return 0;    
    }    
    
    T G(
        const Vector<T,3>& w,
        const T alpha_x,
        const T alpha_y) const
    {
        float cos_theta = std::fabs(w.z);
        const T alpha_x2 = square(alpha_x);
        
        if (alpha_x != alpha_y)
        {
            const T alpha_y2 = square(alpha_y);            
            const T sin_theta = std::sqrt(std::max(T(1) - cos_theta * cos_theta, T(0)));
            T cos_phi2 = w.x / sin_theta;
            T sin_phi2 = w.y / sin_theta;
            cos_phi2 *= cos_phi2;
            sin_phi2 *= sin_phi2;

            const T alpha = std::sqrt(cos_phi2 * alpha_x2 + sin_phi2 * alpha_y2);
            const T a = cos_theta / (alpha * sin_theta);
            const T lambda = (T(-1) + std::sqrt(1 + 1 / (a * a))) * 0.5;
            return 1 / (1 + lambda);
        }

        // eq. 34: calculate G
        return T(2) / (1 + std::sqrt(1 + alpha_x2 * (1 - cos_theta * cos_theta) / (cos_theta * cos_theta)));
    }
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_MICROFACET_H
