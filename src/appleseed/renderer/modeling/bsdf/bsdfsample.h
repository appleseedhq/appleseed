
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2016 Esteban Tovagliari, The appleseedhq Organization
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
    const foundation::Vector3d      m_geometric_normal;     // world space geometric normal at the point where sampling is done, unit-length
    const foundation::Vector3d      m_shading_normal;       // world space shading normal at the point where sampling is done, unit-length
    foundation::Basis3d             m_shading_basis;        // world space shading basis at the point where sampling is done
    const foundation::Vector3d      m_dndu;
    const foundation::Vector3d      m_dndv;
    const foundation::Vector2f      m_duvdx;
    const foundation::Vector2f      m_duvdy;
    const foundation::Dual3d        m_outgoing;             // world space outgoing direction, unit-length

    // Outputs.
    ScatteringMode::Mode            m_mode;                 // scattering mode
    foundation::Dual3d              m_incoming;             // world space incoming direction, unit-length, defined only if m_mode != Absorption
    double                          m_probability;          // PDF value, defined only if m_mode != Absorption
    Spectrum                        m_value;                // BSDF value, defined only if m_mode != Absorption

    // Constructor.
    BSDFSample(
        const ShadingPoint&         shading_point,
        const foundation::Dual3d&   outgoing);

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
  : m_geometric_normal(shading_point.get_geometric_normal())
  , m_shading_normal(shading_point.get_shading_normal())
  , m_shading_basis(shading_point.get_shading_basis())
  , m_dndu(shading_point.get_dndu(0))
  , m_dndv(shading_point.get_dndv(0))
  , m_duvdx(shading_point.get_duvdx(0))
  , m_duvdy(shading_point.get_duvdy(0))
  , m_outgoing(outgoing)
  , m_mode(ScatteringMode::Absorption)
{
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_BSDF_BSDFSAMPLE_H
