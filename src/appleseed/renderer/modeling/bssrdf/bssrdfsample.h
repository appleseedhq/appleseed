
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015 Esteban Tovagliari, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_MODELING_BSSRDF_BSSRDFSAMPLE_H
#define APPLESEED_RENDERER_MODELING_BSSRDF_BSSRDFSAMPLE_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/shading/shadingpoint.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/vector.h"

// Standard headers
#include <cstddef>
#include <limits>

namespace renderer
{

class BSSRDFSample
{
  public:
    // Constructor.
    BSSRDFSample(
        const ShadingPoint& shading_point,
        SamplingContext&    sampling_context);

    // Input fields.

    SamplingContext& get_sampling_context();
    const ShadingPoint& get_shading_point() const;

    // Output fields.

    const foundation::Basis3d& get_sample_basis() const;
    void set_sample_basis(const foundation::Basis3d& basis);

    size_t get_channel() const;
    void set_channel(const size_t channel);

    const foundation::Vector3d& get_origin() const;
    void set_origin(const foundation::Vector3d& origin);

    bool get_use_offset_origin() const;
    void set_use_offset_origin(const bool use_offset_origin);

    double get_max_distance() const;
    void set_max_distance(double max_distance);

    bool is_directional() const;
    void set_is_directional(const bool is_directional);

    double get_eta() const;
    void set_eta(const double eta);

  private:
    const ShadingPoint&     m_shading_point;       // shading point at which the sampling is done
    SamplingContext&        m_sampling_context;    // sampling context used to sample BSSRDFs

    bool                    m_is_directional;
    foundation::Basis3d     m_sample_basis;
    size_t                  m_channel;
    double                  m_eta;
    foundation::Vector3d    m_origin;
    bool                    m_use_offset_origin;
    double                  m_max_distance;
};


//
// BSSRDFSample class implementation.
//

inline BSSRDFSample::BSSRDFSample(
    const ShadingPoint&     shading_point,
    SamplingContext&        sampling_context)
  : m_shading_point(shading_point)
  , m_sampling_context(sampling_context)
  , m_is_directional(false)
  , m_use_offset_origin(false)
  , m_max_distance(std::numeric_limits<double>::max())
{
}

inline SamplingContext& BSSRDFSample::get_sampling_context()
{
    return m_sampling_context;
}

inline const ShadingPoint& BSSRDFSample::get_shading_point() const
{
    return m_shading_point;
}

inline const foundation::Basis3d& BSSRDFSample::get_sample_basis() const
{
    return m_sample_basis;
}

inline void BSSRDFSample::set_sample_basis(const foundation::Basis3d& basis)
{
    m_sample_basis = basis;
}

inline size_t BSSRDFSample::get_channel() const
{
    return m_channel;
}

inline void BSSRDFSample::set_channel(const size_t channel)
{
    m_channel = channel;
}

inline const foundation::Vector3d&BSSRDFSample::get_origin() const
{
    return m_origin;
}

inline void BSSRDFSample::set_origin(const foundation::Vector3d& origin)
{
    m_origin = origin;
}

inline bool BSSRDFSample::get_use_offset_origin() const
{
    return m_use_offset_origin;
}

inline void BSSRDFSample::set_use_offset_origin(const bool use_offset_origin)
{
    m_use_offset_origin = use_offset_origin;
}

inline double BSSRDFSample::get_max_distance() const
{
    return m_max_distance;
}

inline void BSSRDFSample::set_max_distance(const double max_distance)
{
    m_max_distance = max_distance;
}

inline bool BSSRDFSample::is_directional() const
{
    return m_is_directional;
}

inline void BSSRDFSample::set_is_directional(const bool is_directional)
{
    m_is_directional = is_directional;
}

inline double BSSRDFSample::get_eta() const
{
    return m_eta;
}

inline void BSSRDFSample::set_eta(const double eta)
{
    m_eta = eta;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_BSSRDF_BSSRDFSAMPLE_H
