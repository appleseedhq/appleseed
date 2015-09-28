
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
#include "renderer/kernel/lighting/scatteringmode.h"
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
    // Constructor.
    BSDFSample(
        const ShadingPoint&         shading_point,
        const foundation::Dual3d&   outgoing);

    // Input fields.

    const foundation::Vector3d& get_geometric_normal() const;
    const foundation::Vector3d& get_shading_normal() const;
    const foundation::Vector3d& get_outgoing_vector() const;

    // Input / Output fields.

    const foundation::Basis3d& get_shading_basis() const;
    void set_shading_basis(const foundation::Basis3d& basis);

    // Output fields.

    ScatteringMode::Mode get_mode() const;
    void set_mode(const ScatteringMode::Mode mode);

    bool is_absorption() const;
    bool is_specular() const;

    const foundation::Dual3d& get_incoming() const;
    const foundation::Vector3d& get_incoming_vector() const;
    void set_incoming(const foundation::Vector3d& incoming);

    double get_probability() const;
    void set_probability(const double probability);

    const Spectrum& value() const;
    Spectrum& value();

    // Ray differentials.
    void compute_reflected_differentials();
    void compute_transmitted_differentials(const double eta);

  private:
    void compute_normal_derivatives(
        foundation::Vector3d&       dndx,
        foundation::Vector3d&       dndy,
        double&                     ddndx,
        double&                     ddndy) const;

    void apply_pdf_differentials_heuristic();

    const ShadingPoint&             m_shading_point;        // shading point at which the sampling is done
    foundation::Dual3d              m_outgoing;             // world space outgoing direction, unit-length
    ScatteringMode::Mode            m_mode;                 // scattering mode
    foundation::Dual3d              m_incoming;             // world space incoming direction, unit-length
    double                          m_probability;          // PDF value
    Spectrum                        m_value;                // BSDF value
};


//
// BSDFSample class implementation.
//

inline BSDFSample::BSDFSample(
    const ShadingPoint&             shading_point,
    const foundation::Dual3d&       outgoing)
  : m_shading_point(shading_point)
  , m_outgoing(outgoing)
  , m_mode(ScatteringMode::Absorption)
  , m_value(0.0f)
  , m_probability(0.0)
{
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

inline ScatteringMode::Mode BSDFSample::get_mode() const
{
    return m_mode;
}

inline void BSDFSample::set_mode(const ScatteringMode::Mode mode)
{
    m_mode = mode;
}

inline bool BSDFSample::is_absorption() const
{
    return m_mode == ScatteringMode::Absorption;
}

inline bool BSDFSample::is_specular() const
{
    return m_mode == ScatteringMode::Specular;
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

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_BSDF_BSDFSAMPLE_H
