
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
#include "renderer/modeling/aov/aov.h"

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

void AOVAccumulator::write(
    const ShadingPoint&     shading_point,
    const Camera&           camera)
{
}


//
// BeautyAOVAccumulator class implementation.
//

BeautyAOVAccumulator::BeautyAOVAccumulator()
  : AOVAccumulator(~0)
{
}

void BeautyAOVAccumulator::set(const Spectrum& value)
{
    m_color_space = ColorSpaceSpectral;
    m_color = value;
}

void BeautyAOVAccumulator::set(const Color3f& color)
{
    m_color_space = ColorSpaceLinearRGB;
    m_color[0] = color[0];
    m_color[1] = color[1];
    m_color[2] = color[2];
}

void BeautyAOVAccumulator::set_to_pink_linear_rgb()
{
    set(Color3f(1.0f, 0.0f, 1.0f));
}

void BeautyAOVAccumulator::apply_multiplier(const float multiplier)
{
    m_color *= multiplier;
}

void BeautyAOVAccumulator::reset()
{
    m_color_space = ColorSpaceLinearRGB;
    m_color.set(0.0f);
}

void BeautyAOVAccumulator::flush(ShadingResult& result)
{
    result.m_color_space = m_color_space;
    result.m_main.m_color = m_color;
}


//
// AlphaAOVAccumulator class implementation.
//

AlphaAOVAccumulator::AlphaAOVAccumulator()
  : AOVAccumulator(~0)
{
}

void AlphaAOVAccumulator::set(const Alpha& alpha)
{
    m_alpha[0] = alpha[0];
}

void AlphaAOVAccumulator::apply_multiplier(const Alpha& multiplier)
{
    m_alpha *= multiplier;
}

void AlphaAOVAccumulator::reset()
{
    m_alpha.set(0.0f);
}

void AlphaAOVAccumulator::flush(ShadingResult& result)
{
    result.m_main.m_alpha = m_alpha;
}


//
// AOVAccumulatorContainer class implementation.
//

AOVAccumulatorContainer::AOVAccumulatorContainer(const AOVContainer& aovs)
  : m_size(0)
{
    memset(m_accumulators, 0, MaxAovAccumulators * sizeof(AOVAccumulator*));

    // Create beauty and alpha accumulators.
    auto_release_ptr<AOVAccumulator> aov_accum(
        new BeautyAOVAccumulator());
    insert(aov_accum);

    aov_accum.reset(
        new AlphaAOVAccumulator());
    insert(aov_accum);

    // Create the remaining accumulators.
    for (size_t i = 0, e = aovs.size(); i < e; ++i)
    {
        const AOV* aov = aovs.get_by_index(i);
        auto_release_ptr<AOVAccumulator> accum = aov->create_accumulator(i);
        insert(accum);
    }
}

AOVAccumulatorContainer::~AOVAccumulatorContainer()
{
    for (size_t i = 0, e = m_size; i < e; ++i)
        m_accumulators[i]->release();
}

void AOVAccumulatorContainer::reset()
{
    for (size_t i = 0, e = m_size; i < e; ++i)
        m_accumulators[i]->reset();
}

void AOVAccumulatorContainer::write(
    const ShadingPoint&       shading_point,
    const Camera&             camera)
{
    for (size_t i = 0, e = m_size; i < e; ++i)
        m_accumulators[i]->write(shading_point, camera);
}

void AOVAccumulatorContainer::flush(ShadingResult& result)
{
    for (size_t i = 0, e = m_size; i < e; ++i)
        m_accumulators[i]->flush(result);
}

bool AOVAccumulatorContainer::insert(
    auto_release_ptr<AOVAccumulator>& aov_accum)
{
    assert(aov_accum.get());

    if (m_size == MaxAovAccumulators - 1)
        return false;

    m_accumulators[m_size++] = aov_accum.release();
    return true;
}

}   // namespace renderer
