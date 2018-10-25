
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "progressivegpuframerenderer.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/rendering/gpuframerendererbase.h"
#include "renderer/kernel/rendering/irenderercontroller.h"
#include "renderer/kernel/rendering/itilecallback.h"
#include "renderer/modeling/project/project.h"

// Standard headers.
#include <cassert>
#include <cstddef>

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Progressive GPU frame renderer.
    //

    class ProgressiveGPUFrameRenderer
      : public GPUFrameRendererBase
    {
      public:
        ProgressiveGPUFrameRenderer(
            const Project&                  project,
            IRendererController*            renderer_controller,
            IGPULightingEngineFactory*      lighting_engine_factory,
            ITileCallbackFactory*           callback_factory,
            const ParamArray&               params)
          : GPUFrameRendererBase(project, renderer_controller, lighting_engine_factory, callback_factory)
          , m_params(params)
        {
        }

        void release() override
        {
            delete this;
        }

        void print_settings() const override
        {
        }

        void render() override
        {
            start_rendering();
            // wait until complete
        }

        bool is_rendering() const override
        {
            return false;
        }

        void start_rendering() override
        {
        }

        void stop_rendering() override
        {
        }

        void pause_rendering() override
        {
        }

        void resume_rendering() override
        {
        }

        void terminate_rendering() override
        {
        }

      private:
        //
        // Progressive GPU frame renderer parameters.
        //

        struct Parameters
        {
            explicit Parameters(const ParamArray& params)
            {
            }
        };

        //
        // Progressive GPU frame renderer implementation details.
        //

        const Parameters m_params;
    };
}


//
// ProgressiveGPUFrameRendererFactory class implementation.
//

ProgressiveGPUFrameRendererFactory::ProgressiveGPUFrameRendererFactory(
    const Project&              project,
    IRendererController*        renderer_controller,
    IGPULightingEngineFactory*  lighting_engine_factory,
    ITileCallbackFactory*       callback_factory,
    const ParamArray&           params)
  : m_project(project)
  , m_renderer_controller(renderer_controller)
  , m_lighting_engine_factory(lighting_engine_factory)
  , m_callback_factory(callback_factory)
  , m_params(params)
{
}

void ProgressiveGPUFrameRendererFactory::release()
{
    delete this;
}

IFrameRenderer* ProgressiveGPUFrameRendererFactory::create()
{
    return
        new ProgressiveGPUFrameRenderer(
            m_project,
            m_renderer_controller,
            m_lighting_engine_factory,
            m_callback_factory,
            m_params);
}

IFrameRenderer* ProgressiveGPUFrameRendererFactory::create(
    const Project&              project,
    IRendererController*        renderer_controller,
    IGPULightingEngineFactory*  lighting_engine_factory,
    ITileCallbackFactory*       callback_factory,
    const ParamArray&           params)
{
    return
        new ProgressiveGPUFrameRenderer(
            project,
            renderer_controller,
            lighting_engine_factory,
            callback_factory,
            params);
}

Dictionary ProgressiveGPUFrameRendererFactory::get_params_metadata()
{
    Dictionary metadata;
    return metadata;
}

}   // namespace renderer
