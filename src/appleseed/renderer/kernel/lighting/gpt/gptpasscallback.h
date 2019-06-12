
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

// appleseed.renderer headers.
#include "renderer/kernel/lighting/gpt/gptparameters.h"
#include "renderer/kernel/lighting/sdtree.h"
#include "renderer/kernel/rendering/terminatablerenderercontroller.h"
#include "renderer/kernel/rendering/ipasscallback.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/platform/timers.h"
#include "foundation/utility/stopwatch.h"

// Standard headers.
#include <cstddef>
#include <memory>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace foundation    { class JobQueue; }
namespace renderer      { class Frame; }

namespace renderer
{

//
// This class is responsible for the path guiding budget balancing logic.
//

class GPTPassCallback
  : public IPassCallback
{
  public:
    // Constructor.
    GPTPassCallback(
        const GPTParameters&            params,
        TerminatableRendererController* renderer_controller,
        STree*                          sd_tree);

    // Delete this instance.
    void release() override;

    // This method is called at the beginning of a pass.
    void on_pass_begin(
        const Frame&                    frame,
        foundation::JobQueue&           job_queue,
        foundation::IAbortSwitch&       abort_switch) override;

    // This method is called at the end of a pass.
    void on_pass_end(
        const Frame&                    frame,
        foundation::JobQueue&           job_queue,
        foundation::IAbortSwitch&       abort_switch) override;

  private:
    const GPTParameters                 m_params;
    size_t                              m_pass_number;
    STree*                              m_sd_tree;
    TerminatableRendererController*     m_renderer_controller;
};

}   // namespace renderer
