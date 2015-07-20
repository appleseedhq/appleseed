
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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

// appleseed.foundation headers.
#include "foundation/image/regularspectrum.h"
#include "foundation/math/rng/distribution.h"
#include "foundation/math/rng/mersennetwister.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/scalar.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cmath>
#include <cstddef>

using namespace foundation;
using namespace std;

TEST_SUITE(Foundation_Math_Fresnel)
{
    TEST_CASE(FresnelReflectanceDielectricSchlick_GivenCosThetaIsZero_ReturnsOne)
    {
        const RegularSpectrum31f NormalReflectance(42.0f);

        RegularSpectrum31f result;
        fresnel_reflectance_dielectric_schlick(result, NormalReflectance, 0.0);

        EXPECT_EQ(RegularSpectrum31f(1.0f), result);
    }

    TEST_CASE(FresnelReflectanceDielectricSchlick_GivenCosThetaIsOne_ReturnsNormalReflectance)
    {
        const RegularSpectrum31f NormalReflectance(42.0f);

        RegularSpectrum31f result;
        fresnel_reflectance_dielectric_schlick(result, NormalReflectance, 1.0);

        EXPECT_EQ(NormalReflectance, result);
    }

    TEST_CASE(FresnelReflectanceDielectric_WhenSwappingEtaAndRcpEta_ReturnsIdenticalValues)
    {
        MersenneTwister rng;

        for (size_t i = 0; i < 1000; ++i)
        {
            const double eta = rand_double1(rng, 0.5, 2.0);

            double cos_theta_i, sin_theta_t2;
            do
            {
                cos_theta_i = rand_double1(rng);
                sin_theta_t2 = (1.0 - square(cos_theta_i)) / square(eta);
            }
            while (sin_theta_t2 > 1.0);
            const double cos_theta_t = sqrt(1.0 - sin_theta_t2);

            double fr_eta, fr_rcp_eta;
            fresnel_reflectance_dielectric(fr_eta, eta, cos_theta_i, cos_theta_t);
            fresnel_reflectance_dielectric(fr_rcp_eta, 1.0 / eta, cos_theta_i, cos_theta_t);

            EXPECT_FEQ_EPS(fr_eta, fr_rcp_eta, 1.0e-12);
        }
    }

    TEST_CASE(FresnelTransmittanceDielectric_WhenSwappingEtaAndRcpEtaAndSwappingNormal_ReturnsIdenticalValues)
    {
        MersenneTwister rng;

        for (size_t i = 0; i < 1000; ++i)
        {
            const double eta = rand_double1(rng, 0.5, 2.0);

            double cos_theta_i, sin_theta_t2;
            do
            {
                cos_theta_i = rand_double1(rng);
                sin_theta_t2 = (1.0 - square(cos_theta_i)) / square(eta);
            }
            while (sin_theta_t2 > 1.0);

            const double cos_theta_t = sqrt(1.0 - sin_theta_t2);

            double tr_eta, tr_rcp_eta;
            fresnel_transmittance_dielectric(tr_eta, eta, cos_theta_i);
            fresnel_transmittance_dielectric(tr_rcp_eta, 1.0 / eta, cos_theta_t);

            EXPECT_FEQ_EPS(tr_eta, tr_rcp_eta, 1.0e-8);
        }
    }
}
