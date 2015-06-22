
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

#ifndef APPLESEED_RENDERER_MODELING_BSSRDF_BSSRDFSAMPLE_H
#define APPLESEED_RENDERER_MODELING_BSSRDF_BSSRDFSAMPLE_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/shading/shadingpoint.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/vector.h"

namespace renderer
{

class BSSRDFSample
{
  public:
    // Constructor.
    BSSRDFSample(
        const ShadingPoint& outgoing_point,
        SamplingContext&    sampling_context);

    // Input fields.

    SamplingContext& get_sampling_context();
    const ShadingPoint& get_outgoing_point() const;

    // Output fields.
    const ShadingPoint& get_incoming_point() const;

  private:
    const ShadingPoint&     m_outgoing_point;       // shading point at which the sampling is done
    SamplingContext&        m_sampling_context;     // sampling context used to sample BSDFs
    ShadingPoint            m_incoming_point;
};


//
// BSSRDFSample class implementation.
//

inline BSSRDFSample::BSSRDFSample(
    const ShadingPoint&         outgoing_point,
    SamplingContext&            sampling_context)
  : m_outgoing_point(outgoing_point)
  , m_sampling_context(sampling_context)
{
}

inline SamplingContext& BSSRDFSample::get_sampling_context()
{
    return m_sampling_context;
}

inline const ShadingPoint& BSSRDFSample::get_outgoing_point() const
{
    return m_outgoing_point;
}

inline const ShadingPoint& BSSRDFSample::get_incoming_point() const
{
    return m_incoming_point;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_BSSRDF_BSSRDFSAMPLE_H
