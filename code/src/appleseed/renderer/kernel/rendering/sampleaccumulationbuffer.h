
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_KERNEL_RENDERING_SAMPLEACCUMULATIONBUFFER_H
#define APPLESEED_RENDERER_KERNEL_RENDERING_SAMPLEACCUMULATIONBUFFER_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/platform/atomic.h"
#include "foundation/platform/types.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class Frame; }
namespace renderer      { class Sample; }

namespace renderer
{

class SampleAccumulationBuffer
  : public foundation::NonCopyable
{
  public:
    // Destructor.
    virtual ~SampleAccumulationBuffer() {}

    // Get the number of samples stored in the buffer. Thread-safe.
    foundation::uint64 get_sample_count() const;

    // Reset the buffer to its initial state. Thread-safe.
    virtual void clear() = 0;

    // Store a set of samples into the buffer. Thread-safe.
    virtual void store_samples(
        const size_t                sample_count,
        const Sample                samples[],
        foundation::IAbortSwitch&   abort_switch) = 0;

    // Develop the buffer to a frame. Thread-safe.
    virtual void develop_to_frame(
        Frame&                      frame,
        foundation::IAbortSwitch&   abort_switch) = 0;

  protected:
    boost::atomic<foundation::uint64> m_sample_count;
};


//
// SampleAccumulationBuffer class implementation.
//

inline foundation::uint64 SampleAccumulationBuffer::get_sample_count() const
{
    return m_sample_count;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_RENDERING_SAMPLEACCUMULATIONBUFFER_H
