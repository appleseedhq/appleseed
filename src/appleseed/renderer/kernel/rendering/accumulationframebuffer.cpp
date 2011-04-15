
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

// Interface header.
#include "accumulationframebuffer.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/modeling/frame/frame.h"

// appleseed.foundation headers.
#include "foundation/utility/string.h"

using namespace boost;
using namespace foundation;
using namespace std;

namespace renderer
{

AccumulationFramebuffer::AccumulationFramebuffer(
    const size_t    width,
    const size_t    height)
  : m_width(width)
  , m_height(height)
  , m_pixel_count(width * height)
{
}

void AccumulationFramebuffer::render_to_frame(Frame& frame)
{
    Spinlock::ScopedLock lock(m_spinlock);
    do_render_to_frame(frame);
}

void AccumulationFramebuffer::try_render_to_frame(Frame& frame)
{
    if (m_spinlock.try_lock())
    {
        do_render_to_frame(frame);
        m_spinlock.unlock();
    }
}

void AccumulationFramebuffer::clear_no_lock()
{
    m_sample_count = 0;
    m_timer_frequency = m_timer.frequency();
    m_last_time = m_timer.read();
    m_last_sample_count = 0;
}

void AccumulationFramebuffer::do_render_to_frame(Frame& frame)
{
    develop_to_frame(frame);
    print_statistics(frame);
}

void AccumulationFramebuffer::print_statistics(const Frame& frame)
{
    const uint64 time = m_timer.read();
    const uint64 elapsed_ticks = time - m_last_time;
    const double elapsed_seconds = static_cast<double>(elapsed_ticks) / m_timer_frequency;

    if (elapsed_seconds >= 1.0)
    {
        const uint64 rendered_samples = m_sample_count - m_last_sample_count;

        const double average_luminance = frame.compute_average_luminance();

        RENDERER_LOG_INFO(
            "%s samples in the progressive framebuffer (%s samples/pixel, %s samples/second, "
            "average luminance is %s)",
            pretty_uint(m_sample_count).c_str(),
            pretty_ratio(m_sample_count, static_cast<uint64>(m_pixel_count)).c_str(),
            pretty_ratio(static_cast<double>(rendered_samples), elapsed_seconds).c_str(),
            pretty_scalar(average_luminance, 6).c_str());

        m_last_sample_count = m_sample_count;
        m_last_time = time;
    }
}

}   // namespace renderer
