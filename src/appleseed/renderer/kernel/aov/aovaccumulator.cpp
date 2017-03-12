
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Esteban Tovagliari, The appleseedhq Organization
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
#include "aovaccumulator.h"

// appleseed.renderer headers.
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingresult.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"

// Standard headers.
#include <cstring>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// AOVAccumulator class implementation.
//

AOVAccumulator::AOVAccumulator(const size_t index)
  : m_index(index)
{
}

AOVAccumulator::~AOVAccumulator()
{
}

void AOVAccumulator::release()
{
    delete this;
}

void AOVAccumulator::reset()
{
    m_color.set(0.0f);
}

void AOVAccumulator::flush(ShadingResult& result)
{
    result.m_aovs[m_index].m_color = m_color;
}


//
// BeautyAOVAccumulator class implementation.
//

BeautyAOVAccumulator::BeautyAOVAccumulator()
  : AOVAccumulator(~0)
{
}

void BeautyAOVAccumulator::set_color_space(const ColorSpace& color_space)
{
    m_color_space = color_space;
}

void BeautyAOVAccumulator::set(const Spectrum& value)
{
    m_color = value;
}

void BeautyAOVAccumulator::set_to_pink_linear_rgb()
{
    m_color_space = ColorSpaceLinearRGB;
    m_color[0] = 1.0f;
    m_color[1] = 0.0f;
    m_color[2] = 1.0f;
}

void BeautyAOVAccumulator::accumulate(
  const ShadingPoint&       shading_point,
  const Spectrum&           value)
{
    m_color += value;
}

void BeautyAOVAccumulator::flush(ShadingResult& result)
{
    //result.m_color_space = m_color_space;
    //result.m_main.m_color = m_color;
}


//
// AlphaAOVAccumulator class implementation.
//

AlphaAOVAccumulator::AlphaAOVAccumulator()
  : AOVAccumulator(~0)
{
}

void AlphaAOVAccumulator::reset()
{
    m_alpha.set(0.0f);
}

void AlphaAOVAccumulator::set(const Alpha& alpha)
{
    m_color[0] = alpha[0];
}

void AlphaAOVAccumulator::mult(const Alpha& alpha)
{
    m_color[0] *= alpha[0];
}

void AlphaAOVAccumulator::accumulate(
  const ShadingPoint&       shading_point,
  const Spectrum&           value)
{
}

void AlphaAOVAccumulator::flush(ShadingResult& result)
{
    //result.m_main.m_alpha = m_alpha;
}


//
// AOVAccumulatorContainer class implementation.
//

AOVAccumulatorContainer::AOVAccumulatorContainer(const AOVContainer& aovs)
  : m_size(0)
{
    memset(m_accumulators, 0, MaxAOVCount * sizeof(AOVAccumulator*));

    create_beauty_accumulator();
    create_alpha_accumulator();
    // todo: create accumulators for all aovs here.
}

AOVAccumulatorContainer::~AOVAccumulatorContainer()
{
    for (size_t i = 0; i < m_size; ++i)
        m_accumulators[i]->release();
}

void AOVAccumulatorContainer::reset()
{
    for (size_t i = 0; i < m_size; ++i)
        m_accumulators[i]->reset();
}

void AOVAccumulatorContainer::accumulate(
    const ShadingPoint&       shading_point,
    const Spectrum&           value)
{
    for (size_t i = 0; i < m_size; ++i)
        m_accumulators[i]->accumulate(shading_point, value);
}

void AOVAccumulatorContainer::flush(ShadingResult& result)
{
    for (size_t i = 0; i < m_size; ++i)
        m_accumulators[i]->flush(result);
}

bool AOVAccumulatorContainer::insert(auto_release_ptr<AOVAccumulator>& aov_accum)
{
    if (m_size == MaxAOVCount - 1)
        return false;

    m_accumulators[m_size++] = aov_accum.release();
    return true;
}

void AOVAccumulatorContainer::create_beauty_accumulator()
{
    assert(m_size = 0);

    auto_release_ptr<AOVAccumulator> aov_accum(
        new BeautyAOVAccumulator());
    insert(aov_accum);
}

void AOVAccumulatorContainer::create_alpha_accumulator()
{
    assert(m_size = 1);

    auto_release_ptr<AOVAccumulator> aov_accum(
        new AlphaAOVAccumulator());
    insert(aov_accum);
}

}   // namespace renderer
