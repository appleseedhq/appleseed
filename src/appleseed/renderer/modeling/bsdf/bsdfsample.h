
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014 Esteban Tovagliari, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_MODELING_BSDF_BSDFSAMPLE_H
#define APPLESEED_RENDERER_MODELING_BSDF_BSDFSAMPLE_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/vector.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// Forward declarations.

namespace renderer
{

class APPLESEED_DLLSYMBOL BSDFSample
{
  public:
    // Scattering modes.
    enum ScatteringMode
    {
        Absorption          = 0,
        Diffuse             = 1 << 0,
        Glossy              = 1 << 1,
        Specular            = 1 << 2,
        AllScatteringModes  = Diffuse | Glossy | Specular
    };

    // Constructor.
    BSDFSample(
        SamplingContext&            sampling_context,
        const foundation::Vector3d& geometric_normal,
        const foundation::Basis3d&  shading_basis,
        const foundation::Vector3d& outgoing);

    // Input fields.

    SamplingContext& get_sampling_context();

    const foundation::Vector3d& get_geometric_normal() const;

    const foundation::Vector3d& get_normal() const;

    const foundation::Vector3d& get_outgoing() const;

    // Input / Output fields.

    const foundation::Basis3d& get_shading_basis() const;
    void set_new_shading_basis(const foundation::Basis3d& new_basis);
    bool has_new_shading_basis() const;

    // Output fields.

    ScatteringMode get_mode() const;
    void set_mode(const ScatteringMode mode);

    const foundation::Vector3d& get_incoming() const;
    void set_incoming(const foundation::Vector3d& incoming);

    double get_probability() const;
    void set_probability(const double probability);

    const Spectrum& get_value() const;
    Spectrum& get_value();

    // Test for the presence of specific scattering modes.
    static bool has_diffuse(const ScatteringMode mode);
    static bool has_glossy(const ScatteringMode mode);
    static bool has_specular(const ScatteringMode mode);
    static bool has_diffuse_or_glossy(const ScatteringMode mode);
    static bool has_glossy_or_specular(const ScatteringMode mode);

  private:
    SamplingContext&        m_sampling_context; // sampling context used to sample BSDFs.
    foundation::Vector3d    m_geometric_normal; // world space geometric normal, unit-length.
    foundation::Vector3d    m_outgoing;         // world space outgoing direction, unit-length.
    foundation::Basis3d     m_shading_basis;    // world space orthonormal basis around shading normal.
    bool                    m_has_new_basis;    // true if the sample method sets a new shading basis (OSL).
    ScatteringMode          m_mode;             // scattering mode.
    foundation::Vector3d    m_incoming;         // world space incoming direction, unit-length.
    double                  m_probability;      // PDF value.
    Spectrum                m_value;            // BSDF value.
};


//
// BSDFSample class implementation.
//

inline BSDFSample::BSDFSample(
    SamplingContext&            sampling_context,
    const foundation::Vector3d& geometric_normal,
    const foundation::Basis3d&  shading_basis,
    const foundation::Vector3d& outgoing)
  : m_sampling_context(sampling_context)
  , m_geometric_normal(geometric_normal)
  , m_shading_basis(shading_basis)
  , m_has_new_basis(false)
  , m_outgoing(outgoing)
  , m_mode(Absorption)
  , m_value(0.0f)
  , m_probability(0.0)
{
}

inline SamplingContext& BSDFSample::get_sampling_context()
{
    return m_sampling_context;
}

inline const foundation::Vector3d& BSDFSample::get_geometric_normal() const
{
    return m_geometric_normal;
}

inline const foundation::Vector3d& BSDFSample::get_normal() const
{
    return m_shading_basis.get_normal();
}

inline const foundation::Vector3d& BSDFSample::get_outgoing() const
{
    return m_outgoing;
}

inline const foundation::Basis3d& BSDFSample::get_shading_basis() const
{
    return m_shading_basis;
}

inline void BSDFSample::set_new_shading_basis(const foundation::Basis3d& new_basis)
{
    m_shading_basis = new_basis;
    m_has_new_basis = true;
}

inline bool BSDFSample::has_new_shading_basis() const
{
    return m_has_new_basis;
}

inline BSDFSample::ScatteringMode BSDFSample::get_mode() const
{
    return m_mode;
}

inline void BSDFSample::set_mode(const ScatteringMode mode)
{
    m_mode = mode;
}

inline const foundation::Vector3d& BSDFSample::get_incoming() const
{
    return m_incoming;
}

inline void BSDFSample::set_incoming(const foundation::Vector3d& incoming)
{
    m_incoming = incoming;
}

inline double BSDFSample::get_probability() const
{
    return m_probability;
}

inline void BSDFSample::set_probability(const double probability)
{
    m_probability = probability;
}

inline const Spectrum& BSDFSample::get_value() const
{
    return m_value;
}

inline Spectrum& BSDFSample::get_value()
{
    return m_value;
}

inline bool BSDFSample::has_diffuse(const ScatteringMode mode)
{
    return (mode & Diffuse) != 0;
}

inline bool BSDFSample::has_glossy(const ScatteringMode mode)
{
    return (mode & Glossy) != 0;
}

inline bool BSDFSample::has_specular(const ScatteringMode mode)
{
    return (mode & Specular) != 0;
}

inline bool BSDFSample::has_diffuse_or_glossy(const ScatteringMode mode)
{
    return (mode & (Diffuse | Glossy)) != 0;
}

inline bool BSDFSample::has_glossy_or_specular(const ScatteringMode mode)
{
    return (mode & (Glossy | Specular)) != 0;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_BSDF_BSDFSAMPLE_H
