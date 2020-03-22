
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

#pragma once

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

namespace renderer
{

//
// Renderer controller, allows to react to various rendering events.
//

class APPLESEED_DLLSYMBOL IRendererController
  : public foundation::NonCopyable
{
  public:
    // Destructor.
    virtual ~IRendererController() = default;

    // This method is called before rendering begins or is reinitialized.
    virtual void on_rendering_begin() = 0;

    // This method is called after rendering has succeeded.
    virtual void on_rendering_success() = 0;

    // This method is called after rendering has failed or was aborted.
    virtual void on_rendering_abort() = 0;

    // This method is called after rendering was paused.
    virtual void on_rendering_pause() = 0;

    // This method is called after rendering was resumed.
    virtual void on_rendering_resume() = 0;

    // This method is called before rendering a single frame.
    virtual void on_frame_begin() = 0;

    // This method is called after rendering a single frame.
    virtual void on_frame_end() = 0;

    // This method is called continuously during rendering.
    virtual void on_progress() = 0;

    enum Status
    {
        // Continue/resume rendering.
        ContinueRendering,

        // Pause rendering.
        PauseRendering,

        // Terminate rendering, call on_rendering_success() on the renderer controller.
        TerminateRendering,

        // Terminate rendering, call on_rendering_abort() on the renderer controller.
        AbortRendering,

        // Restart rendering using the same configuration.
        RestartRendering,

        // Restart rendering from scratch, taking into account any configuration changes.
        ReinitializeRendering
    };

    // Return the current rendering status.
    virtual Status get_status() const = 0;
};

}   // namespace renderer
