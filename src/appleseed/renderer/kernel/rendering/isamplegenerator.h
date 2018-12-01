
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
#include "foundation/core/concepts/iunknown.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace foundation    { class StatisticsVector; }
namespace renderer      { class SampleAccumulationBuffer; }

namespace renderer
{

//
// Sample generator interface.
//

class ISampleGenerator
  : public foundation::IUnknown
{
  public:
    // Print this component's settings to the renderer's global logger.
    virtual void print_settings() const = 0;

    // Reset the sample generator to its initial state.
    virtual void reset() = 0;

    // Generate a given number of samples and accumulate them into a buffer.
    virtual void generate_samples(
        const size_t                sample_count,
        SampleAccumulationBuffer&   buffer,
        foundation::IAbortSwitch&   abort_switch) = 0;

    // Retrieve performance statistics.
    virtual foundation::StatisticsVector get_statistics() const = 0;
};


//
// Interface of a ISampleGenerator factory that can cross DLL boundaries.
//

class ISampleGeneratorFactory
  : public foundation::IUnknown
{
  public:
    // Return a new sample generator instance.
    virtual ISampleGenerator* create(
        const size_t                generator_index,
        const size_t                generator_count) = 0;

    // Create an accumulation buffer for this sample generator.
    virtual SampleAccumulationBuffer* create_sample_accumulation_buffer() = 0;
};

}   // namespace renderer
