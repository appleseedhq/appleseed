
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
#include "genericgpuframerenderer.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/intersection/optixtracecontext.h"
#include "renderer/kernel/rendering/generic/tilejobfactory.h"
#include "renderer/kernel/rendering/gpuframerendererbase.h"
#include "renderer/kernel/rendering/irenderercontroller.h"
#include "renderer/kernel/rendering/itilecallback.h"
#include "renderer/modeling/camera/camera.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/math/ordering.h"
#include "foundation/math/rng/mersennetwister.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <chrono>
#include <thread>
#include <vector>

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Generic GPU frame renderer.
    //

    class GenericGPUFrameRenderer
      : public GPUFrameRendererBase
    {
      public:
        GenericGPUFrameRenderer(
            const Project&                  project,
            IRendererController*            renderer_controller,
            IGPULightingEngineFactory*      lighting_engine_factory,
            ITileCallbackFactory*           callback_factory,
            const ParamArray&               params)
          : GPUFrameRendererBase(project, renderer_controller, lighting_engine_factory, callback_factory)
          , m_params(params)
          , m_is_rendering(false)
          , m_abort(false)
        {
        }

        void release() override
        {
            delete this;
        }

        void print_settings() const override
        {
            RENDERER_LOG_INFO(
                "Generic GPU frame renderer settings:\n"
                "  tile ordering                 %s\n"
                "  passes                        %s\n"
                "  samples                       %s",
                m_params.m_tile_ordering == TileJobFactory::TileOrdering::LinearOrdering ? "linear" :
                m_params.m_tile_ordering == TileJobFactory::TileOrdering::SpiralOrdering ? "spiral" :
                m_params.m_tile_ordering == TileJobFactory::TileOrdering::HilbertOrdering ? "hilbert" : "random",
                pretty_uint(m_params.m_pass_count).c_str(),
                pretty_uint(m_params.m_sample_count).c_str());
        }

        void render() override
        {
            start_rendering();

            // Wait until render is done.
            while (is_rendering())
                std::this_thread::sleep_for(std::chrono::milliseconds(100));

            if (m_render_thread.joinable())
                m_render_thread.join();
        }

        bool is_rendering() const override
        {
            return m_is_rendering;
        }

        void start_rendering() override
        {
            m_tiles.clear();

            const Frame& frame = *m_project.get_frame();
            const CanvasProperties& props = frame.image().properties();

            TileJobFactory::generate_tile_ordering(props, m_params.m_tile_ordering, m_rng, m_tiles);

            // Launch the rendering thread.
            m_is_rendering = true;
            m_render_thread = std::thread(&GenericGPUFrameRenderer::do_render, this);
        }

        void stop_rendering() override
        {
            m_abort = false;

            if (m_render_thread.joinable())
                m_render_thread.join();
        }

        void pause_rendering() override
        {
            // Not supported.
        }

        void resume_rendering() override
        {
            // Not supported.
        }

        void terminate_rendering() override
        {
            stop_rendering();
        }

      private:
        //
        // Generic GPU frame renderer parameters.
        //

        struct Parameters
        {
            const TileJobFactory::TileOrdering  m_tile_ordering;    // tile rendering order
            const size_t                        m_sample_count;     // number of samples per pixel
            const size_t                        m_pass_count;       // number of rendering passes

            explicit Parameters(const ParamArray& params)
              : m_tile_ordering(get_tile_ordering(params))
              , m_sample_count(get_sample_count(params))
              , m_pass_count(get_pass_count(params))
            {
            }

            static TileJobFactory::TileOrdering get_tile_ordering(const ParamArray& params)
            {
                const string tile_ordering =
                    params.get_optional<string>("tile_ordering", "spiral");

                if (tile_ordering == "linear")
                {
                    return TileJobFactory::LinearOrdering;
                }
                else if (tile_ordering == "spiral")
                {
                    return TileJobFactory::SpiralOrdering;
                }
                else if (tile_ordering == "hilbert")
                {
                    return TileJobFactory::HilbertOrdering;
                }
                else if (tile_ordering == "random")
                {
                    return TileJobFactory::RandomOrdering;
                }
                else
                {
                    RENDERER_LOG_ERROR(
                        "invalid value \"%s\" for parameter \"%s\", using default value \"%s\".",
                        tile_ordering.c_str(),
                        "tile_ordering",
                        "hilbert");

                    return TileJobFactory::HilbertOrdering;
                }
            }

            static size_t get_pass_count(const ParamArray& params)
            {
                ParamArray p = params.child("generic_frame_renderer");
                return p.get_optional<size_t>("passes", 1);
            }

            static size_t get_sample_count(const ParamArray& params)
            {
                ParamArray p = params.child("uniform_pixel_renderer");
                return p.get_required<size_t>("samples", 64);
            }
        };

        //
        // Generic GPU frame renderer implementation details.
        //

        const Parameters                m_params;

        std::thread                     m_render_thread;
        bool                            m_is_rendering;
        bool                            m_abort;

        std::vector<size_t>             m_tiles;
        MersenneTwister                 m_rng;

        void do_render()
        {
            try
            {
                for (size_t pass = 0; pass < m_params.m_pass_count; ++pass)
                {
                    if (m_params.m_pass_count > 1)
                        RENDERER_LOG_INFO("--- beginning rendering pass %s ---", pretty_uint(pass + 1).c_str());

                    do_render_pass(pass);

                    if (check_abort())
                        break;
                }
            }
            catch (const optix::Exception& e)
            {
                RENDERER_LOG_ERROR("OptiX error: %s", e.getErrorString().c_str());
                m_is_rendering = false;
            }
            catch (const Exception& e)
            {
                RENDERER_LOG_ERROR("Exception: %s", e.what());
                m_is_rendering = false;
            }
            catch (...)
            {
                RENDERER_LOG_ERROR("Unknown exception!");
                m_is_rendering = false;
            }

            m_is_rendering = false;
        }

        void do_render_pass(const size_t pass)
        {
            const Frame& frame = *m_project.get_frame();
            const CanvasProperties& props = frame.image().properties();

            const size_t sample_increment = get_sample_increment(
                props.m_tile_width,
                props.m_tile_height,
                m_params.m_sample_count);

            const AABB2u& crop = frame.get_crop_window();

            for (size_t i = 0, e = m_tiles.size(); i < e; ++i)
            {
                if (check_abort())
                    return;

                // Compute coordinates of the tile in the frame.
                const size_t tile_index = m_tiles[i];
                const size_t tile_x = tile_index % props.m_tile_count_x;
                const size_t tile_y = tile_index / props.m_tile_count_x;
                assert(tile_x < props.m_tile_count_x);
                assert(tile_y < props.m_tile_count_y);

                // Call the pre-render tile callback.
                if (m_tile_callback.get())
                    m_tile_callback->on_tile_begin(&frame, tile_x, tile_y);

                // Render the tile.
                {
                    Tile& tile = frame.image().tile(tile_x, tile_y);

                    const int tile_origin_x = static_cast<int>(props.m_tile_width  * tile_x);
                    const int tile_origin_y = static_cast<int>(props.m_tile_height * tile_y);

                    // Compute the image space bounding box of the pixels to render.
                    AABB2i tile_bbox;
                    tile_bbox.min.x = tile_origin_x;
                    tile_bbox.min.y = tile_origin_y;
                    tile_bbox.max.x = tile_origin_x + static_cast<int>(tile.get_width()) - 1;
                    tile_bbox.max.y = tile_origin_y + static_cast<int>(tile.get_height()) - 1;
                    tile_bbox = AABB2i::intersect(tile_bbox, AABB2i(crop));

                    if (tile_bbox.is_valid())
                    {
                        for (size_t sample = 0; sample < m_params.m_pass_count; sample += sample_increment)
                        {
                            // render sample batch here...
                            // accum results.
                        }

                        // render potentially missing samples.
                        // accum results.
                    }
                }

                // Call the post-render tile callback.
                if (m_tile_callback.get())
                    m_tile_callback->on_tile_end(&frame, tile_x, tile_y);
            }
        }

        size_t get_sample_increment(
            const size_t tile_width,
            const size_t tile_height,
            const size_t total_samples) const
        {
            // todo: implement this...
            return 1;
        }

        bool check_abort() const
        {
            if (m_abort)
                return true;

            if (m_renderer_controller)
            {
                IRendererController::Status status = m_renderer_controller->get_status();

                if (status == IRendererController::AbortRendering)
                    return true;

                if (status == IRendererController::TerminateRendering)
                    return true;
            }

            return false;
        }
    };
}


//
// GenericGPUFrameRendererFactory class implementation.
//

GenericGPUFrameRendererFactory::GenericGPUFrameRendererFactory(
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

void GenericGPUFrameRendererFactory::release()
{
    delete this;
}

IFrameRenderer* GenericGPUFrameRendererFactory::create()
{
    return
        new GenericGPUFrameRenderer(
            m_project,
            m_renderer_controller,
            m_lighting_engine_factory,
            m_callback_factory,
            m_params);
}

IFrameRenderer* GenericGPUFrameRendererFactory::create(
    const Project&              project,
    IRendererController*        renderer_controller,
    IGPULightingEngineFactory*  lighting_engine_factory,
    ITileCallbackFactory*       callback_factory,
    const ParamArray&           params)
{
    return
        new GenericGPUFrameRenderer(
            project,
            renderer_controller,
            lighting_engine_factory,
            callback_factory,
            params);
}

Dictionary GenericGPUFrameRendererFactory::get_params_metadata()
{
    Dictionary metadata;

    return metadata;
}

}   // namespace renderer
