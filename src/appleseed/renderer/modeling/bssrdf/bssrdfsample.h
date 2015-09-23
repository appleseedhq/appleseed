
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

// appleseed.foundation headers.
#include "foundation/math/vector.h"

// Standard headers
#include <cstddef>

namespace renderer
{

class BSSRDFSample
{
  public:
    // Constructor.
    explicit BSSRDFSample(SamplingContext& sampling_context);

    // Input fields.

    SamplingContext& get_sampling_context();

    // Output fields.

    double get_eta() const;
    void set_eta(const double eta);

    size_t get_channel() const;
    void set_channel(const size_t channel);

    const foundation::Vector2d& get_point() const;
    void set_point(const foundation::Vector2d& point);

    double get_rmax2() const;
    void set_rmax2(const double rmax2);

  private:
    SamplingContext&        m_sampling_context;    // sampling context used to sample BSSRDFs
    double                  m_eta;
    size_t                  m_channel;
    foundation::Vector2d    m_point;
    double                  m_rmax2;
};


//
// BSSRDFSample class implementation.
//

inline BSSRDFSample::BSSRDFSample(SamplingContext& sampling_context)
  : m_sampling_context(sampling_context)
{
}

inline SamplingContext& BSSRDFSample::get_sampling_context()
{
    return m_sampling_context;
}

inline double BSSRDFSample::get_eta() const
{
    return m_eta;
}

inline void BSSRDFSample::set_eta(const double eta)
{
    m_eta = eta;
}

inline size_t BSSRDFSample::get_channel() const
{
    return m_channel;
}

inline void BSSRDFSample::set_channel(const size_t channel)
{
    m_channel = channel;
}

inline const foundation::Vector2d& BSSRDFSample::get_point() const
{
    return m_point;
}

inline void BSSRDFSample::set_point(const foundation::Vector2d& point)
{
    m_point = point;
}

inline double BSSRDFSample::get_rmax2() const
{
    return m_rmax2;
}

inline void BSSRDFSample::set_rmax2(const double rmax2)
{
    m_rmax2 = rmax2;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_BSSRDF_BSSRDFSAMPLE_H
