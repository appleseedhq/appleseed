
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
#include "qtrenderercontroller.h"

namespace appleseed {
namespace studio {

//
// QtRendererController class implementation.
//

// Constructor.
QtRendererController::QtRendererController()
  : m_status(ContinueRendering)
{
}

// Set the status that will be returned by on_progress().
void QtRendererController::set_status(const Status status)
{
    m_status = status;
}

// This method is called before rendering begins.
void QtRendererController::on_rendering_begin()
{
    DefaultRendererController::on_rendering_begin();

    emit signal_rendering_begin();
}

// This method is called after rendering has succeeded.
void QtRendererController::on_rendering_success()
{
    DefaultRendererController::on_rendering_success();

    emit signal_rendering_success();
}

// This method is called after rendering was aborted.
void QtRendererController::on_rendering_abort()
{
    DefaultRendererController::on_rendering_abort();

    emit signal_rendering_abort();
}

// This method is called before rendering a single frame.
void QtRendererController::on_frame_begin()
{
    DefaultRendererController::on_frame_begin();

    emit signal_frame_begin();
}

// This method is called after rendering a single frame.
void QtRendererController::on_frame_end()
{
    DefaultRendererController::on_frame_end();

    emit signal_frame_end();
}

// This method is called continuously during rendering.
QtRendererController::Status QtRendererController::on_progress()
{
    DefaultRendererController::on_progress();

    return m_status;
}

}   // namespace studio
}   // namespace appleseed
