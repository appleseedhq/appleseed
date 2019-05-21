
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "timedrenderercontroller.h"

// appleseed.foundation headers.
#include "foundation/platform/defaulttimers.h"
#include "foundation/utility/stopwatch.h"

using namespace foundation;

namespace renderer
{

//
// TimedRendererController class implementation.
//

struct TimedRendererController::Impl
{
    const double                        m_seconds;
    Stopwatch<DefaultWallclockTimer>    m_stopwatch;

    explicit Impl(const double seconds)
      : m_seconds(seconds)
    {
    }
};

TimedRendererController::TimedRendererController(const double seconds)
  : impl(new Impl(seconds))
{
}

TimedRendererController::~TimedRendererController()
{
    delete impl;
}

void TimedRendererController::on_frame_begin()
{
    impl->m_stopwatch.start();
}

void TimedRendererController::on_rendering_pause()
{
    impl->m_stopwatch.pause();
}

void TimedRendererController::on_rendering_resume()
{
    impl->m_stopwatch.resume();
}

IRendererController::Status TimedRendererController::get_status() const
{
    return
        impl->m_stopwatch.measure().get_seconds() >= impl->m_seconds
            ? TerminateRendering
            : ContinueRendering;
}

}   // namespace renderer
