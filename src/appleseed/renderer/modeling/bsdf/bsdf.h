
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

#ifndef APPLESEED_RENDERER_MODELING_BSDF_BSDF_H
#define APPLESEED_RENDERER_MODELING_BSDF_BSDF_H

// appleseed.renderer headers.
#include "renderer/global/global.h"
#include "renderer/modeling/entity/connectableentity.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"

// Forward declarations.
namespace renderer      { class Project; }

namespace renderer
{

//
// Bidirectional Scattering Distribution Function (BSDF).
//
// All direction vectors are unit-length and pointing outward.
// All vectors are expressed in world space.
// All probability densities are measured with respect to solid angle.
//

class RENDERERDLL BSDF
  : public ConnectableEntity
{
  public:
    // Constructor.
    explicit BSDF(const ParamArray& params);

    // Return a string identifying the model of this entity.
    virtual const char* get_model() const = 0;

    // This method is called once before rendering each frame.
    virtual void on_frame_begin(
        const Project&              project,
        const void*                 data);                      // input values

    // This method is called once after rendering each frame.
    virtual void on_frame_end(
        const Project&              project);

    // Scattering modes.
    enum Mode
    {
        None        = 0,            // absorption
        Diffuse     = 1 << 0,       // diffuse reflection
        Glossy      = 1 << 1,       // glossy reflection
        Specular    = 1 << 2        // specular reflection
    };

    // Probability density of the Dirac Delta.
    static const double DiracDelta;

    // Given an outgoing direction, sample the BSDF and compute the incoming
    // direction, the probability density with which it was chosen, the value
    // of the BSDF divided by the probability density and the scattering mode.
    virtual void sample(
        const void*                 data,                       // input values
        const bool                  adjoint,                    // use the adjoint scattering kernel if true
        const foundation::Vector3d& geometric_normal,           // world space geometric normal, unit-length
        const foundation::Basis3d&  shading_basis,              // world space orthonormal basis around shading normal
        const foundation::Vector3d& s,                          // sample in [0,1)^3
        const foundation::Vector3d& outgoing,                   // world space outgoing direction, unit-length
        foundation::Vector3d&       incoming,                   // world space incoming direction, unit-length
        Spectrum&                   value,                      // BSDF value divided by PDF value
        double&                     probability,                // PDF value
        Mode&                       mode) const = 0;            // scattering mode

    // Evaluate the BSDF for a given pair of directions.
    virtual void evaluate(
        const void*                 data,                       // input values
        const bool                  adjoint,                    // use the adjoint scattering kernel if true
        const foundation::Vector3d& geometric_normal,           // world space geometric normal, unit-length
        const foundation::Basis3d&  shading_basis,              // world space orthonormal basis around shading normal
        const foundation::Vector3d& outgoing,                   // world space outgoing direction, unit-length
        const foundation::Vector3d& incoming,                   // world space incoming direction, unit-length
        Spectrum&                   value) const = 0;           // BSDF value for this pair of directions

    // Evaluate the PDF for a given pair of directions.
    virtual double evaluate_pdf(
        const void*                 data,                       // input values
        const foundation::Vector3d& geometric_normal,           // world space geometric normal, unit-length
        const foundation::Basis3d&  shading_basis,              // world space orthonormal basis around shading normal
        const foundation::Vector3d& outgoing,                   // world space outgoing direction, unit-length
        const foundation::Vector3d& incoming) const = 0;        // world space incoming direction, unit-length

  protected:
    // Force a given direction to lie above a surface described by its normal vector.
    static foundation::Vector3d force_above_surface(
        const foundation::Vector3d& direction,
        const foundation::Vector3d& normal);
};


//
// BSDF class implementation.
//

inline foundation::Vector3d BSDF::force_above_surface(
    const foundation::Vector3d& direction,
    const foundation::Vector3d& normal)
{
    const double Eps = 1.0e-4;

    const double cos_theta = foundation::dot(direction, normal);
    const double correction = Eps - cos_theta;

    return
        correction > 0.0
            ? normalize(direction + correction * normal)
            : direction;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_BSDF_BSDF_H
