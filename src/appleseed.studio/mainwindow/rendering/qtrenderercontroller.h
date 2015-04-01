
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_RENDERING_QTRENDERERCONTROLLER_H
#define APPLESEED_STUDIO_MAINWINDOW_RENDERING_QTRENDERERCONTROLLER_H

// appleseed.renderer headers.
#include "renderer/api/rendering.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/platform/thread.h"

// Boost headers.
#include "boost/cstdint.hpp"

// Qt headers.
#include <QObject>

namespace appleseed {
namespace studio {

class QtRendererController
  : public QObject
  , public renderer::DefaultRendererController
{
    Q_OBJECT

  public:
    // Constructor.
    QtRendererController();

    // This method is called before rendering begins.
    virtual void on_rendering_begin() APPLESEED_OVERRIDE;

    // This method is called after rendering has succeeded.
    virtual void on_rendering_success() APPLESEED_OVERRIDE;

    // This method is called after rendering was aborted.
    virtual void on_rendering_abort() APPLESEED_OVERRIDE;

    // This method is called before rendering a single frame.
    virtual void on_frame_begin() APPLESEED_OVERRIDE;

    // This method is called after rendering a single frame.
    virtual void on_frame_end() APPLESEED_OVERRIDE;

    // Store a new status value.
    void set_status(const Status status);

    // Return the current rendering status.
    virtual Status get_status() const APPLESEED_OVERRIDE;

  signals:
    void signal_rendering_begin();
    void signal_rendering_success();
    void signal_rendering_abort();
    void signal_frame_begin();
    void signal_frame_end();

  private:
    mutable volatile boost::uint32_t m_status;
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_RENDERING_QTRENDERERCONTROLLER_H
