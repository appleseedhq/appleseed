
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2015 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/kernel/shading/shadingpoint.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/dual.h"
#include "foundation/math/vector.h"

namespace renderer
{

class BSDFSample
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
        const ShadingPoint&         shading_point,
        SamplingContext&            sampling_context,
        const foundation::Dual3d&   outgoing);

    // Input fields.

    SamplingContext& get_sampling_context();

    const foundation::Vector3d& get_geometric_normal() const;

    const foundation::Vector3d& get_shading_normal() const;

    const foundation::Vector3d& get_outgoing_vector() const;

    // Input / Output fields.

    const foundation::Basis3d& get_shading_basis() const;
    void set_shading_basis(const foundation::Basis3d& basis);

    // Output fields.

    ScatteringMode get_mode() const;
    void set_mode(const ScatteringMode mode);

    bool is_absorption() const;
    bool is_specular() const;

    const foundation::Dual3d& get_incoming() const;
    const foundation::Vector3d& get_incoming_vector() const;
    void set_incoming(const foundation::Vector3d& incoming);

    double get_probability() const;
    void set_probability(const double probability);

    const Spectrum& value() const;
    Spectrum& value();

    // Test for the presence of specific scattering modes.
    static bool has_diffuse(const ScatteringMode mode);
    static bool has_glossy(const ScatteringMode mode);
    static bool has_specular(const ScatteringMode mode);
    static bool has_diffuse_or_glossy(const ScatteringMode mode);
    static bool has_glossy_or_specular(const ScatteringMode mode);

  private:
    const ShadingPoint&     m_shading_point;        // shading point at which the sampling is done
    SamplingContext&        m_sampling_context;     // sampling context used to sample BSDFs
    foundation::Dual3d      m_outgoing;             // world space outgoing direction, unit-length
    ScatteringMode          m_mode;                 // scattering mode
    foundation::Dual3d      m_incoming;             // world space incoming direction, unit-length
    double                  m_probability;          // PDF value
    Spectrum                m_value;                // BSDF value
};


//
// BSDFSample class implementation.
//

inline BSDFSample::BSDFSample(
    const ShadingPoint&         shading_point,
    SamplingContext&            sampling_context,
    const foundation::Dual3d&   outgoing)
  : m_shading_point(shading_point)
  , m_sampling_context(sampling_context)
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
    return m_shading_point.get_geometric_normal();
}

inline const foundation::Vector3d& BSDFSample::get_shading_normal() const
{
    return m_shading_point.get_shading_normal();
}

inline const foundation::Vector3d& BSDFSample::get_outgoing_vector() const
{
    return m_outgoing.get_value();
}

inline const foundation::Basis3d& BSDFSample::get_shading_basis() const
{
    return m_shading_point.get_shading_basis();
}

inline void BSDFSample::set_shading_basis(const foundation::Basis3d& basis)
{
    m_shading_point.set_shading_basis(basis);
}

inline BSDFSample::ScatteringMode BSDFSample::get_mode() const
{
    return m_mode;
}

inline void BSDFSample::set_mode(const ScatteringMode mode)
{
    m_mode = mode;
}

inline bool BSDFSample::is_absorption() const
{
    return m_mode == Absorption;
}

inline bool BSDFSample::is_specular() const
{
    return m_mode == Specular;
}

inline const foundation::Dual3d& BSDFSample::get_incoming() const
{
    return m_incoming;
}

inline const foundation::Vector3d& BSDFSample::get_incoming_vector() const
{
    return m_incoming.get_value();
}

inline void BSDFSample::set_incoming(const foundation::Vector3d& incoming)
{
    m_incoming = foundation::Dual3d(incoming);
}

inline double BSDFSample::get_probability() const
{
    return m_probability;
}

inline void BSDFSample::set_probability(const double probability)
{
    m_probability = probability;
}

inline const Spectrum& BSDFSample::value() const
{
    return m_value;
}

inline Spectrum& BSDFSample::value()
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
