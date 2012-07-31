
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/entity/connectableentity.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/vector.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace renderer      { class Assembly; }
namespace renderer      { class InputEvaluator; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Project; }

namespace renderer
{

//
// Bidirectional Scattering Distribution Function (BSDF).
//
// Conventions:
//
//   * All direction vectors are unit-length and pointing outward.
//   * All vectors are expressed in world space.
//   * All probability densities are measured with respect to solid angle.
//   * When the adjoint flag is false, the BSDF characterizes the light flow.
//     When the adjoint flag is true, the BSDF characterizes the importance flow.
//   * Regardless of the adjoint flag, light and importance always flow from the
//     incoming direction to the outgoing direction.
//   * The incoming direction is always the "sampled" direction.
//

class DLLSYMBOL BSDF
  : public ConnectableEntity
{
  public:
    // BSDF types.
    enum Type
    {
        Reflective          = 1 << 0,
        Transmissive        = 1 << 1
    };

    // Scattering modes.
    enum Mode
    {
        Absorption          = 0,
        Diffuse             = 1 << 0,
        Glossy              = 1 << 1,
        Specular            = 1 << 2,
        MaxScatteringMode
    };

    // Use a particular (negative) value as the probability density
    // of the Dirac Delta in order to detect incorrect usages.
    static const double DiracDelta;

    // Constructor.
    BSDF(
        const char*                 name,
        const Type                  type,
        const ParamArray&           params);

    // Return a string identifying the model of this entity.
    virtual const char* get_model() const = 0;

    Type get_type() const;

    // This method is called once before rendering each frame.
    virtual void on_frame_begin(
        const Project&              project,
        const Assembly&             assembly);

    // This method is called once after rendering each frame.
    virtual void on_frame_end(
        const Project&              project,
        const Assembly&             assembly);

    // Compute the cumulated size in bytes of the values of all inputs of
    // this BSDF and its child BSDFs, if any.
    virtual size_t compute_input_data_size(
        const Assembly&             assembly) const;

    // Evaluate the inputs of this BSDF and of its child BSDFs, if any.
    // Input values are stored in the input evaluator. This method is called
    // once per shading point and pair of incoming/outgoing directions.
    virtual void evaluate_inputs(
        InputEvaluator&             input_evaluator,
        const foundation::Vector2d& uv,
        const size_t                offset = 0) const;

    // Given an outgoing direction, sample the BSDF and compute the incoming
    // direction, the probability density with which it was chosen and the value
    // of the BSDF for this pair of directions. Return the scattering mode.
    // If the scattering mode is Absorption, the BSDF and PDF values are undefined.
    virtual Mode sample(
        SamplingContext&            sampling_context,
        const void*                 data,                       // input values
        const bool                  adjoint,                    // if true, use the adjoint scattering kernel
        const bool                  cosine_mult,                // if true, multiply by |cos(incoming, normal)|
        const foundation::Vector3d& geometric_normal,           // world space geometric normal, unit-length
        const foundation::Basis3d&  shading_basis,              // world space orthonormal basis around shading normal
        const foundation::Vector3d& outgoing,                   // world space outgoing direction, unit-length
        foundation::Vector3d&       incoming,                   // world space incoming direction, unit-length
        Spectrum&                   value,                      // BSDF value, or BSDF value * |cos(incoming, normal)|
        double&                     probability) const = 0;     // PDF value

    // Evaluate the BSDF for a given pair of directions. Return the PDF value
    // for this pair of directions. If the returned probability is zero, the
    // BSDF value is undefined.
    virtual double evaluate(
        const void*                 data,                       // input values
        const bool                  adjoint,                    // if true, use the adjoint scattering kernel
        const bool                  cosine_mult,                // if true, multiply by |cos(incoming, normal)|
        const foundation::Vector3d& geometric_normal,           // world space geometric normal, unit-length
        const foundation::Basis3d&  shading_basis,              // world space orthonormal basis around shading normal
        const foundation::Vector3d& outgoing,                   // world space outgoing direction, unit-length
        const foundation::Vector3d& incoming,                   // world space incoming direction, unit-length
        Spectrum&                   value) const = 0;           // BSDF value, or BSDF value * |cos(incoming, normal)|

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

  private:
    const Type m_type;
};


//
// BSDF class implementation.
//

inline BSDF::Type BSDF::get_type() const
{
    return m_type;
}

inline foundation::Vector3d BSDF::force_above_surface(
    const foundation::Vector3d& direction,
    const foundation::Vector3d& normal)
{
    const double Eps = 1.0e-4;

    const double cos_theta = foundation::dot(direction, normal);
    const double correction = Eps - cos_theta;

    return
        correction > 0.0
            ? foundation::normalize(direction + correction * normal)
            : direction;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_BSDF_BSDF_H
