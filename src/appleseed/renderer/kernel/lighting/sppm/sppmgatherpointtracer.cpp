
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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
#include "sppmgatherpointtracer.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/lighting/sppm/sppmgatherpoint.h"
#include "renderer/kernel/lighting/pathtracer.h"
#include "renderer/kernel/lighting/pathvertex.h"
#include "renderer/kernel/rendering/pixelcontext.h"
#include "renderer/kernel/rendering/pixelrendererbase.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/kernel/texturing/texturecache.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/camera/camera.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/math/aabb.h"
#include "foundation/math/hash.h"
#include "foundation/math/vector.h"
#include "foundation/platform/types.h"
#include "foundation/utility/memory.h"
#include "foundation/utility/statistics.h"

// Standard headers.
#include <cassert>

// Forward declarations.
namespace foundation    { class Tile; }
namespace renderer      { class ShadingPoint; }
namespace renderer      { class TileStack; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Gather point tracer.
    //

    class SPPMGatherPointTracer
      : public PixelRendererBase
    {
      public:
        SPPMGatherPointTracer(
            const bool                  primary,
            const Scene&                scene,
            const TraceContext&         trace_context,
            TextureStore&               texture_store,
            const size_t                pass_hash,
            SPPMGatherPointVector&      global_gather_points)
          : m_scene(scene)
          , m_texture_cache(texture_store)
          , m_intersector(trace_context, m_texture_cache /*, m_params.m_report_self_intersections*/)
          , m_pass_hash(pass_hash)
          , m_global_gather_points(global_gather_points)
        {
        }

        virtual void release() OVERRIDE
        {
            delete this;
        }

        virtual void on_tile_begin(
            const Frame&                frame,
            Tile&                       tile,
            TileStack&                  aov_tiles) OVERRIDE
        {
            clear_keep_memory(m_local_gather_points);
        }

        virtual void on_tile_end(
            const Frame&                frame,
            Tile&                       tile,
            TileStack&                  aov_tiles) OVERRIDE
        {
            m_global_gather_points.append(m_local_gather_points);
        }

        virtual void render_pixel(
            const Frame&                frame,
            Tile&                       tile,
            TileStack&                  aov_tiles,
            const AABB2i&               tile_bbox,
            const PixelContext&         pixel_context,
            const int                   tx,
            const int                   ty,
            SamplingContext::RNGType&   rng,
            ShadingResultFrameBuffer&   framebuffer) OVERRIDE
        {
            if (!tile_bbox.contains(Vector2i(tx, ty)))
                return;

            const size_t frame_width = frame.image().properties().m_canvas_width;
            const int ix = pixel_context.m_ix;
            const int iy = pixel_context.m_iy;
            const size_t pixel_index = iy * frame_width + ix;

            // Create a sampling context.
            const size_t instance = mix32(
                static_cast<uint32>(m_pass_hash),
                static_cast<uint32>(pixel_index));
            SamplingContext sampling_context(
                rng,
                2,                      // number of dimensions
                0,                      // number of samples -- unknown
                instance);              // initial instance number

            // Compute the sample position in NDC.
            const Vector2d s = sampling_context.next_vector2<2>();
            const Vector2d sample_position = frame.get_sample_position(ix + s.x, iy + s.y);

            // Construct a primary ray.
            ShadingRay primary_ray;
            m_scene.get_camera()->generate_ray(
                sampling_context,
                sample_position,
                primary_ray);

            // Build the path tracer.
            PathVisitor path_visitor(pixel_index, m_local_gather_points);
            PathTracer<PathVisitor, false> path_tracer(     // false = not adjoint
                path_visitor,
                3,                      // m_params.m_rr_min_path_length
                ~0,                     // m_params.m_max_path_length
                1000);                  // m_params.m_max_iterations

            // Trace the eye path.
            SamplingContext child_sampling_context(sampling_context);
            const size_t path_length =
                path_tracer.trace(
                    child_sampling_context,
                    m_intersector,
                    m_texture_cache,
                    primary_ray);
        }

        virtual StatisticsVector get_statistics() const OVERRIDE
        {
            return StatisticsVector();
        }

      private:
        const Scene&                    m_scene;
        TextureCache                    m_texture_cache;
        Intersector                     m_intersector;
        const size_t                    m_pass_hash;
        SPPMGatherPointVector&          m_global_gather_points;
        SPPMGatherPointVector           m_local_gather_points;

        struct PathVisitor
        {
            const size_t                m_pixel_index;
            SPPMGatherPointVector&      m_local_gather_points;

            explicit PathVisitor(
                const size_t            pixel_index,
                SPPMGatherPointVector&  local_gather_points)
              : m_pixel_index(pixel_index)
              , m_local_gather_points(local_gather_points)
            {
            }

            bool accept_scattering_mode(
                const BSDF::Mode        prev_bsdf_mode,
                const BSDF::Mode        bsdf_mode) const
            {
                assert(bsdf_mode != BSDF::Absorption);
                return bsdf_mode == BSDF::Specular;
            }

            bool visit_vertex(const PathVertex& vertex)
            {
                if (vertex.m_bsdf)
                {
                    SPPMGatherPoint gather_point;
                    gather_point.m_position = vertex.get_point();
                    gather_point.m_uv = vertex.get_uv(0);
                    gather_point.m_geometric_normal = vertex.get_geometric_normal();
                    gather_point.m_shading_basis = vertex.get_shading_basis();
                    gather_point.m_incoming = vertex.m_outgoing;
                    gather_point.m_bsdf = vertex.m_bsdf;
                    gather_point.m_throughput = vertex.m_throughput;
                    gather_point.m_pixel_index = m_pixel_index;
                    m_local_gather_points.push_back(gather_point);
                }

                return true;
            }

            void visit_environment(
                const ShadingPoint&     shading_point,
                const Vector3d&         outgoing,
                const BSDF::Mode        prev_bsdf_mode,
                const double            prev_bsdf_prob,
                const Spectrum&         throughput)
            {
                // The ray escapes, nothing to do.
            }
        };
    };
}


//
// SPPMGatherPointTracerFactory class implementation.
//

SPPMGatherPointTracerFactory::SPPMGatherPointTracerFactory(
    const Scene&            scene,
    const TraceContext&     trace_context,
    TextureStore&           texture_store,
    const size_t            pass_hash,
    SPPMGatherPointVector&  global_gather_points)
  : m_scene(scene)
  , m_trace_context(trace_context)
  , m_texture_store(texture_store)
  , m_pass_hash(pass_hash)
  , m_global_gather_points(global_gather_points)
{
}

void SPPMGatherPointTracerFactory::release()
{
    delete this;
}

IPixelRenderer* SPPMGatherPointTracerFactory::create(const bool primary)
{
    return
        new SPPMGatherPointTracer(
            primary,
            m_scene,
            m_trace_context,
            m_texture_store,
            m_pass_hash,
            m_global_gather_points);
}

}   // namespace renderer
