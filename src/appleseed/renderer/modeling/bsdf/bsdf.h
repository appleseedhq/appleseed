
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

#ifndef APPLESEED_RENDERER_MODELING_BSDF_BSDF_H
#define APPLESEED_RENDERER_MODELING_BSDF_BSDF_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/entity/connectableentity.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/vector.h"
#include "foundation/utility/uid.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class AbortSwitch; }
namespace renderer      { class Assembly; }
namespace renderer      { class InputEvaluator; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Project; }
namespace renderer      { class ShadingPoint; }

namespace renderer
{

//
// Bidirectional Scattering Distribution Function (BSDF).
//
// Conventions (Veach, 3.7.5, figure 3.3 on page 93):
//
//   * All direction vectors are expressed in world space.
//
//   * All direction vectors are unit-length and pointing outward.
//
//   * All probability densities are measured with respect to solid angle.
//
//   * When the adjoint flag is false, such as in path tracing, the BSDF
//     characterizes the light flow.
//
//   * When the adjoint flag is true, such as in photon tracing, the BSDF
//     characterizes the importance flow.
//
//   * Regardless of the adjoint flag, light and importance always flow
//     from the incoming direction to the outgoing direction.
//
//   * The incoming direction is always the sampled direction.
//

class DLLSYMBOL BSDF
  : public ConnectableEntity
{
  public:
    // Return the unique ID of this class of entities.
    static foundation::UniqueID get_class_uid();

    // BSDF types.
    enum Type
    {
        Reflective          = 1 << 0,
        Transmissive        = 1 << 1,
        AllBSDFTypes        = Reflective | Transmissive
    };

    // Scattering modes.
    enum Mode
    {
        Absorption          = 0,
        Diffuse             = 1 << 0,
        Glossy              = 1 << 1,
        Specular            = 1 << 2,
        AllScatteringModes  = Diffuse | Glossy | Specular
    };

    // Test for the presence of specific scattering modes.
    static bool has_diffuse(const Mode mode);
    static bool has_glossy(const Mode mode);
    static bool has_specular(const Mode mode);
    static bool has_diffuse_or_glossy(const Mode mode);
    static bool has_glossy_or_specular(const Mode mode);

    // Use a particular (negative) value as the probability density
    // of the Dirac Delta in order to detect incorrect usages.
    static const double DiracDelta;

    // Constructor.
    BSDF(
        const char*                 name,
        const Type                  type,
        const int                   modes,
        const ParamArray&           params);

    // Return a string identifying the model of this entity.
    virtual const char* get_model() const = 0;

    // Return the type of the BSDF.
    Type get_type() const;

    // Return the possible scattering modes of the BSDF.
    int get_modes() const;

    // Convenient functions to check the possible scattering modes of the BSDF.
    bool is_purely_diffuse() const;
    bool is_purely_glossy() const;
    bool is_purely_specular() const;
    bool is_purely_diffuse_or_glossy() const;
    bool is_purely_glossy_or_specular() const;

    // This method is called once before rendering each frame.
    // Returns true on success, false otherwise.
    virtual bool on_frame_begin(
        const Project&              project,
        const Assembly&             assembly,
        foundation::AbortSwitch*    abort_switch = 0);

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
        const ShadingPoint&         shading_point,
        const size_t                offset = 0) const;

    // Given an outgoing direction, sample the BSDF and compute the incoming
    // direction, its probability density and the value of the BSDF for this
    // pair of directions. Return the scattering mode. If the scattering mode
    // is Absorption, the BSDF and PDF values are undefined.
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
        const int                   modes,                      // selected scattering modes
        Spectrum&                   value) const = 0;           // BSDF value, or BSDF value * |cos(incoming, normal)|

    // Evaluate the PDF for a given pair of directions.
    virtual double evaluate_pdf(
        const void*                 data,                       // input values
        const foundation::Vector3d& geometric_normal,           // world space geometric normal, unit-length
        const foundation::Basis3d&  shading_basis,              // world space orthonormal basis around shading normal
        const foundation::Vector3d& outgoing,                   // world space outgoing direction, unit-length
        const foundation::Vector3d& incoming,                   // world space incoming direction, unit-length
        const int                   modes) const = 0;           // selected scattering modes

  protected:
    // Force a given direction to lie above a surface described by its normal vector.
    static foundation::Vector3d force_above_surface(
        const foundation::Vector3d& direction,
        const foundation::Vector3d& normal);

  private:
    const Type  m_type;
    const int   m_modes;
};


//
// BSDF class implementation.
//

inline bool BSDF::has_diffuse(const Mode mode)
{
    return (mode & Diffuse) != 0;
}

inline bool BSDF::has_glossy(const Mode mode)
{
    return (mode & Glossy) != 0;
}

inline bool BSDF::has_specular(const Mode mode)
{
    return (mode & Specular) != 0;
}

inline bool BSDF::has_diffuse_or_glossy(const Mode mode)
{
    return (mode & (Diffuse | Glossy)) != 0;
}

inline bool BSDF::has_glossy_or_specular(const Mode mode)
{
    return (mode & (Glossy | Specular)) != 0;
}

inline BSDF::Type BSDF::get_type() const
{
    return m_type;
}

inline int BSDF::get_modes() const
{
    return m_modes;
}

inline bool BSDF::is_purely_diffuse() const
{
    return m_modes == Diffuse;
}

inline bool BSDF::is_purely_glossy() const
{
    return m_modes == Glossy;
}

inline bool BSDF::is_purely_specular() const
{
    return m_modes == Specular;
}

inline bool BSDF::is_purely_diffuse_or_glossy() const
{
    return m_modes == (Diffuse | Glossy);
}

inline bool BSDF::is_purely_glossy_or_specular() const
{
    return m_modes == (Glossy | Specular);
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
