
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

void AOVAccumulator::flush()
{
}

namespace
{

class BeautyAOVAccumulator
  : public AOVAccumulator
{
  public:
    BeautyAOVAccumulator()
      : AOVAccumulator(~0)
    {
    }

    virtual void reset() APPLESEED_OVERRIDE
    {
    }

    virtual void accumulate(
      const ShadingPoint&       shading_point,
      const Spectrum&           value,
      const float               alpha) APPLESEED_OVERRIDE
    {
    }

    virtual void flush() APPLESEED_OVERRIDE
    {
    }
};

}

//
// AOVAccumulatorContainer class implementation.
//

AOVAccumulatorContainer::AOVAccumulatorContainer()
  : m_size(0)
{
    memset(m_accumulators, 0, MaxAOVCount * sizeof(AOVAccumulator*));
    create_beauty_accumulator();
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
    const Spectrum&           value,
    const float               alpha)
{
    for (size_t i = 0; i < m_size; ++i)
        m_accumulators[i]->accumulate(shading_point, value, alpha);
}

void AOVAccumulatorContainer::flush()
{
    for (size_t i = 0; i < m_size; ++i)
        m_accumulators[i]->flush();
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

}   // namespace renderer
