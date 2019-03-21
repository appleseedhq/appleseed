
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
#include "renderer/kernel/rendering/isamplegenerator.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace renderer  { class ForwardLightSampler; }
namespace renderer  { class Frame; }
namespace renderer  { class OIIOTextureSystem; }
namespace renderer  { class OSLShadingSystem; }
namespace renderer  { class Project; }
namespace renderer  { class SampleAccumulationBuffer; }
namespace renderer  { class TextureStore; }
namespace renderer  { class TraceContext; }

namespace renderer
{

class LightTracingSampleGeneratorFactory
  : public ISampleGeneratorFactory
{
  public:
    // Constructor.
    LightTracingSampleGeneratorFactory(
        const Project&              project,
        const Frame&                frame,
        const TraceContext&         trace_context,
        TextureStore&               texture_store,
        const ForwardLightSampler&  light_sampler,
        OIIOTextureSystem&          oiio_texture_system,
        OSLShadingSystem&           shading_system,
        const ParamArray&           params);

    // Delete this instance.
    void release() override;

    // Return a new sample generator instance.
    ISampleGenerator* create(
        const size_t                generator_index,
        const size_t                generator_count) override;

    // Create an accumulation buffer for this sample generator.
    SampleAccumulationBuffer* create_sample_accumulation_buffer() override;

  private:
    const Project&                  m_project;
    const Frame&                    m_frame;
    const TraceContext&             m_trace_context;
    TextureStore&                   m_texture_store;
    const ForwardLightSampler&      m_light_sampler;
    OIIOTextureSystem&              m_oiio_texture_system;
    OSLShadingSystem&               m_shading_system;
    const ParamArray                m_params;
};

}   // namespace renderer
