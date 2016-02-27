
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
    // Inputs.
    const ShadingPoint&             m_shading_point;        // shading point at which the sampling is done
    const foundation::Dual3d        m_outgoing;             // world space outgoing direction, unit-length

    // Outputs.
    foundation::Dual3d              m_incoming;             // world space incoming direction, unit-length
    ScatteringMode::Mode            m_mode;                 // scattering mode
    double                          m_probability;          // PDF value
    Spectrum                        m_value;                // BSDF value

    // Constructor.
    BSDFSample(
        const ShadingPoint&         shading_point,
        const foundation::Dual3d&   outgoing);

    const foundation::Vector3d& get_geometric_normal() const;
    const foundation::Vector3d& get_shading_normal() const;
    const foundation::Basis3d& get_shading_basis() const;
    void set_shading_basis(const foundation::Basis3d& basis);

    void compute_reflected_differentials();
    void compute_transmitted_differentials(const double eta);

  private:
    void compute_normal_derivatives(
        foundation::Vector3d&       dndx,
        foundation::Vector3d&       dndy,
        double&                     ddndx,
        double&                     ddndy) const;

    void apply_pdf_differentials_heuristic();
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

inline const foundation::Basis3d& BSDFSample::get_shading_basis() const
{
    return m_shading_point.get_shading_basis();
}

inline void BSDFSample::set_shading_basis(const foundation::Basis3d& basis)
{
    m_shading_point.set_shading_basis(basis);
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_BSDF_BSDFSAMPLE_H
