
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz
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

#ifndef APPLESEED_RENDERER_KERNEL_RENDERING_ACCUMULATIONFRAMEBUFFER_H
#define APPLESEED_RENDERER_KERNEL_RENDERING_ACCUMULATIONFRAMEBUFFER_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/platform/thread.h"
#include "foundation/platform/types.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace renderer      { class Frame; }
namespace renderer      { class Sample; }

namespace renderer
{

class AccumulationFramebuffer
  : public foundation::NonCopyable
{
  public:
    // Constructor.
    AccumulationFramebuffer(
        const size_t    width,
        const size_t    height);

    // Destructor.
    virtual ~AccumulationFramebuffer() {}

    // Get the dimensions of the framebuffer.
    size_t get_width() const;
    size_t get_height() const;

    // Get the number of samples stored in the framebuffer.
    foundation::uint64 get_sample_count() const;

    // Reset the framebuffer to its initial state. Thread-safe.
    virtual void clear() = 0;

    // Store @samples into the framebuffer. Thread-safe.
    virtual void store_samples(
        const size_t    sample_count,
        const Sample    samples[]) = 0;

    // Develop the framebuffer to a frame. Thread-safe.
    void render_to_frame(Frame& frame);

  protected:
    const size_t                        m_width;
    const size_t                        m_height;
    const size_t                        m_pixel_count;
    mutable foundation::Spinlock        m_spinlock;
    foundation::uint64                  m_sample_count;

    void clear_no_lock();

    virtual void develop_to_frame(Frame& frame) const = 0;
};


//
// AccumulationFramebuffer class implementation.
//

inline size_t AccumulationFramebuffer::get_width() const
{
    return m_width;
}

inline size_t AccumulationFramebuffer::get_height() const
{
    return m_height;
}

inline foundation::uint64 AccumulationFramebuffer::get_sample_count() const
{
    return m_sample_count;
}

inline void AccumulationFramebuffer::clear_no_lock()
{
    m_sample_count = 0;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_RENDERING_ACCUMULATIONFRAMEBUFFER_H
