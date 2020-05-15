
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2020 Tiago Chaves, The appleseedhq Organization
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
#include "renderer/modeling/postprocessingeffect/ipostprocessingeffect.h"

// appleseed.foundation headers.
#include "foundation/utility/job.h"

// Standard headers.
#include <cstddef>
#include <vector>

// Forward declarations.
namespace renderer  { class Frame; }

namespace renderer
{

//
// Post-processing effect applier job.
//

class EffectJob
  : public foundation::IJob
{
  public:
    typedef std::vector<IEffectApplier*> EffectApplierVector;

    // Constructor.
    EffectJob(
        const EffectApplierVector&  effect_appliers,
        const Frame&                frame,
        const size_t                tile_x,
        const size_t                tile_y,
        const size_t                thread_count,
        foundation::IAbortSwitch&   abort_switch);

    // Execute the job.
    void execute(const size_t thread_index);

  private:
    const EffectApplierVector&      m_effect_appliers;
    const Frame&                    m_frame;
    const size_t                    m_tile_x;
    const size_t                    m_tile_y;
    const size_t                    m_thread_count;
    foundation::IAbortSwitch&       m_abort_switch;
};

//
// Creates jobs to apply a post-processing effect to a complete frame.
//

class EffectJobFactory
{
  public:
    typedef std::vector<EffectJob*> EffectJobVector;

    // Create effect jobs for a given frame.
    void create(
        const Frame&                            frame,
        const EffectJob::EffectApplierVector&   effect_appliers,
        const size_t                            thread_count,
        EffectJobVector&                        effect_jobs,
        foundation::IAbortSwitch&               abort_switch);
};

}   // namespace renderer
