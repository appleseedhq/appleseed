
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2016 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_MODELING_BSSRDF_SEPARABLEBSSRDF_H
#define APPLESEED_RENDERER_MODELING_BSSRDF_SEPARABLEBSSRDF_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bssrdf/bssrdf.h"

// appleseed.foundation headers.
#include "foundation/math/fresnel.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"

// Standard headers.
#include <cmath>

// Forward declarations.
namespace renderer  { class ParamArray; }

namespace renderer
{

//
// Base class for radially-symmetric BSSRDF models.
//

class SeparableBSSRDF
  : public BSSRDF
{
  public:
    // Constructor.
    SeparableBSSRDF(
        const char*                 name,
        const ParamArray&           params);

    virtual void evaluate(
        const void*                 data,
        const ShadingPoint&         outgoing_point,
        const foundation::Vector3d& outgoing_dir,
        const ShadingPoint&         incoming_point,
        const foundation::Vector3d& incoming_dir,
        Spectrum&                   value) const APPLESEED_OVERRIDE;

  protected:
    // Return the relative index of refraction.
    virtual double get_eta(
        const void*                 data) const = 0;

    // Evaluate the profile for a given (square) radius.
    virtual void evaluate_profile(
        const void*                 data,
        const double                square_radius,
        Spectrum&                   value) const = 0;
};


//
// SeparableBSSRDF class implementation.
//

inline SeparableBSSRDF::SeparableBSSRDF(
    const char*                     name,
    const ParamArray&               params)
  : BSSRDF(name, params)
{
}

inline void SeparableBSSRDF::evaluate(
    const void*                     data,
    const ShadingPoint&             outgoing_point,
    const foundation::Vector3d&     outgoing_dir,
    const ShadingPoint&             incoming_point,
    const foundation::Vector3d&     incoming_dir,
    Spectrum&                       value) const
{
    //
    // The diffusion term of a separable BSSRDF is given in [1] equation 5:
    //
    //   Sd(xi, wi, xo, wo) = 1/Pi * Ft(eta, wi) * Rd(||xi - xo||) * Ft(eta, wo)
    //
    // Rd(r) is the spatially resolved diffuse reflectance for a normally
    // incident beam of light on a planar semi-infinite medium ([2] equation 27):
    //
    //            /
    //            |
    //   Rd(x0) = |  Sd(xi, wi, xo, wo) (wo . no) dwo
    //            |
    //            / 2 Pi
    //
    // If Sd does not depend on wo, then Rd only depends on ||xi - xo|| and we get:
    //
    //   Rd = Pi Sd    <=>    Sd = 1/Pi Rd
    //
    // A note regarding the computation of the Fresnel factors:
    //
    //   Since the direction in the outside medium--be it the outgoing or the incoming
    //   direction--is always fixed, we always need to figure out a refracted direction
    //   in the inside medium, and thus we compute both Fresnel factors at the
    //   incoming and outgoing points using eta (defined as outside IOR / inside IOR).
    //
    // [1] A Practical Model for Subsurface Light Transport
    //     https://graphics.stanford.edu/papers/bssrdf/bssrdf.pdf
    //
    // [2] Directional Dipole Model for Subsurface Scattering
    //     Jeppe Revall Frisvad, Toshiya Hachisuka, Thomas Kim Kjeldsen
    //     http://www.ci.i.u-tokyo.ac.jp/~hachisuka/dirpole.pdf
    //

    const double eta = get_eta(data);

    double fo;
    const double cos_on = std::abs(foundation::dot(outgoing_dir, outgoing_point.get_shading_normal()));
    foundation::fresnel_transmittance_dielectric(fo, eta, cos_on);

    double fi;
    const double cos_in = std::abs(foundation::dot(incoming_dir, incoming_point.get_shading_normal()));
    foundation::fresnel_transmittance_dielectric(fi, eta, cos_in);

    const double square_radius =
        foundation::square_norm(outgoing_point.get_point() - incoming_point.get_point());
    evaluate_profile(data, square_radius, value);

    value *= static_cast<float>(foundation::RcpPi * fo * fi);
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_BSSRDF_SEPARABLEBSSRDF_H
