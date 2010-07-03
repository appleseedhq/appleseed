
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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

#ifndef APPLESEED_RENDERER_KERNEL_RENDERING_PROGRESSIVE_PROGRESSIVEFRAMEBUFFER_H
#define APPLESEED_RENDERER_KERNEL_RENDERING_PROGRESSIVE_PROGRESSIVEFRAMEBUFFER_H

// appleseed.renderer headers.
#include "renderer/global/global.h"
#include "renderer/kernel/rendering/progressive/accumulationframebuffer.h"

// appleseed.foundation headers.
#include "foundation/platform/thread.h"
#include "foundation/platform/timer.h"

// Forward declarations.
namespace foundation    { class Tile; }
namespace renderer      { class Frame; }
namespace renderer      { class Sample; }

namespace renderer
{

class ProgressiveFrameBuffer
  : public foundation::NonCopyable
{
  public:
    // Constructor.
    ProgressiveFrameBuffer(
        const size_t    width,
        const size_t    height);

    // Destructor.
    ~ProgressiveFrameBuffer();

    size_t get_width() const;
    size_t get_height() const;

    // Thread-safe.
    void clear();

    // Thread-safe.
    void store_samples(
        const size_t    sample_count,
        const Sample    samples[]);

    // Thread-safe.
    void render_to_frame(Frame& frame);

  private:
    AccumulationFrameBuffer             m_fb;
    mutable boost::mutex                m_fb_mutex;

    size_t                              m_sample_count;

    typedef std::vector<AccumulationFrameBuffer*>
        AccumulationFrameBufferVector;

    AccumulationFrameBufferVector       m_mipmaps;

    foundation::DefaultWallclockTimer   m_timer;
    foundation::uint64                  m_timer_frequency;
    
    foundation::uint64                  m_last_time;
    size_t                              m_last_sample_count;

    static void copy_to_frame(
        const AccumulationFrameBuffer&  fb,
        Frame&                          frame);

    static void copy_to_frame_tile(
        const AccumulationFrameBuffer&  fb,
        foundation::Tile&               tile,
        const size_t                    origin_x,
        const size_t                    origin_y,
        const size_t                    tile_x,
        const size_t                    tile_y);

    void allocate_mipmaps();
    void deallocate_mipmaps();

    void store_sample(const Sample& sample);

    void render_to_frame_resample(Frame& frame) const;

    void recursive_resample(const size_t level) const;

    void print_statistics(const Frame& frame);
};


//
// ProgressiveFrameBuffer class implementation.
//

inline size_t ProgressiveFrameBuffer::get_width() const
{
    return m_fb.get_width();
}

inline size_t ProgressiveFrameBuffer::get_height() const
{
    return m_fb.get_height();
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_RENDERING_PROGRESSIVE_PROGRESSIVEFRAMEBUFFER_H
