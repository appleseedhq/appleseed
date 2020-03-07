
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Francois Beaune, The appleseedhq Organization
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

// appleseed.renderer headers.
#include "renderer/api/object.h"
#include "renderer/api/project.h"
#include "renderer/api/rendering.h"
#include "renderer/api/scene.h"
#include "renderer/api/types.h"

// todo: fix.
#include "renderer/kernel/shading/shadingray.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/hash/hash.h"
#include "foundation/math/intersection/rayaabb.h"
#include "foundation/math/ray.h"
#include "foundation/math/rng/distribution.h"
#include "foundation/math/rng/xoroshiro128plus.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/types.h"
#include "foundation/string/string.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/casts.h"
#include "foundation/utility/job/iabortswitch.h"
#include "foundation/utility/searchpaths.h"

// appleseed.main headers.
#include "main/dllvisibility.h"

// Standard headers.
#include <algorithm>
#include <cmath>
#include <cstddef>

namespace asf = foundation;
namespace asr = renderer;

namespace
{
    //
    // An object whose surface is defined by a signed distance field.
    //

    const char* Model = "distance_field_object";

    class DistanceFieldObject
      : public asr::ProceduralObject
    {
      public:
        // Constructor.
        DistanceFieldObject(
            const char*                 name,
            const asr::ParamArray&      params)
          : asr::ProceduralObject(name, params)
        {
        }

        // Delete this instance.
        void release() override
        {
            delete this;
        }

        // Return a string identifying this object model.
        const char* get_model() const override
        {
            return Model;
        }

        // Compute the local space bounding box of the object over the shutter interval.
        asr::GAABB3 compute_local_bbox() const override
        {
            return asr::GAABB3(asr::GVector3(-2.0f), asr::GVector3(2.0f));
        }

        // Access materials slots.
        std::size_t get_material_slot_count() const override
        {
            return 1;
        }
        const char* get_material_slot(const std::size_t index) const override
        {
            return "default";
        }

        // Compute the intersection between a ray expressed in object space and
        // the surface of this object and return detailed intersection results.
        void intersect(
            const asr::ShadingRay&  ray,
            IntersectionResult&     result) const override
        {
            double t;
            asf::Vector3d p;
            result.m_hit = raymarch(ray, t, p);

            if (result.m_hit)
            {
                result.m_distance = t;

                const float H = 1.0e-4f;

                asf::Vector3f n(
                    compute_distance(p.x + H, p.y, p.z) - compute_distance(p.x - H, p.y, p.z),
                    compute_distance(p.x, p.y + H, p.z) - compute_distance(p.x, p.y - H, p.z),
                    compute_distance(p.x, p.y, p.z + H) - compute_distance(p.x, p.y, p.z - H));
                n = asf::normalize(n);

                result.m_geometric_normal = asf::Vector3d(n);
                result.m_shading_normal = asf::Vector3d(n);

                result.m_uv = asf::Vector2f(0.0f);
                result.m_material_slot = 0;
            }
        }

        // Compute the intersection between a ray expressed in object space and
        // the surface of this object and simply return whether there was a hit.
        bool intersect(
            const asr::ShadingRay&  ray) const override
        {
            double t;
            asf::Vector3d p;
            return raymarch(ray, t, p);
        }

      private:

        //
        // Signed distance function.
        //
        // References:
        //
        //   http://iquilezles.org/www/articles/distfunctions/distfunctions.htm
        //
        //   http://blog.hvidtfeldts.net/index.php/2011/09/distance-estimated-3d-fractals-v-the-mandelbulb-different-de-approximations/
        //

        static float compute_distance(asf::Vector3f p)
        {
            /*return
                op_substraction(
                    prim_cube(p + asf::Vector3f(0.5f, 0.0f, 0.0f), 0.5f),
                    prim_sphere(p, 0.5f));*/

            return prim_mandelbulb(p);
        }

        //
        // Modeling utilities.
        //

        static asf::Xoroshiro128plus make_rng(const asr::ShadingRay& ray)
        {
            return
                asf::Xoroshiro128plus(
                    asf::hash_uint64(asf::binary_cast<asf::uint64>(ray.m_org.x)) ^
                    asf::hash_uint64(asf::binary_cast<asf::uint64>(ray.m_org.y)) ^
                    asf::hash_uint64(asf::binary_cast<asf::uint64>(ray.m_org.z)),
                    asf::hash_uint64(asf::binary_cast<asf::uint64>(ray.m_dir.x)) ^
                    asf::hash_uint64(asf::binary_cast<asf::uint64>(ray.m_dir.y)) ^
                    asf::hash_uint64(asf::binary_cast<asf::uint64>(ray.m_dir.z)));
        }

        static float op_union(const float a, const float b)
        {
            return std::min(a, b);
        }

        static float op_substraction(const float a, const float b)
        {
            return std::max(a, -b);
        }

        static float op_intersection(const float a, const float b)
        {
            return std::max(a, b);
        }

        static float prim_sphere(const asf::Vector3f& p, const float radius)
        {
            return asf::norm(p) - radius;
        }

        static float prim_cube(asf::Vector3f p, const float half_size)
        {
            p.x = std::max(abs(p.x) - half_size, 0.0f);
            p.y = std::max(abs(p.y) - half_size, 0.0f);
            p.z = std::max(abs(p.z) - half_size, 0.0f);
            return asf::norm(p);
        }

        static float prim_mandelbulb(const asf::Vector3f& p)
        {
            constexpr float Power = 8.0f;
            constexpr float Bailout = 4.0f;
            constexpr std::size_t Iterations = 20;

            asf::Vector3f z = p;
            float dr = 1.0f;
            float r = 0.0f;

            for (std::size_t i = 0; i < Iterations; ++i)
            {
                r = asf::norm(z);
                if (r > Bailout)
                    break;

                // Convert to polar coordinates.
                float theta = std::acos(z.z / r);
                float phi = std::atan2(z.y, z.x);
                dr = std::pow(r, Power - 1.0f) * Power * dr + 1.0f;

                // Scale and rotate the point.
                const float zr = std::pow(r, Power);
                theta *= Power;
                phi *= Power;

                // Convert back to cartesian coordinates.
                z = zr * asf::Vector3f(std::sin(theta) * std::cos(phi), std::sin(phi) * std::sin(theta), std::cos(theta));
                z += p;
            }

            return 0.5f * std::log(r) * r / dr;
        }

        //
        // Raymarcher.
        //
        // References:
        //
        //   http://jamie-wong.com/2016/07/15/ray-marching-signed-distance-functions/
        //
        //   http://erleuchtet.org/~cupe/permanent/enhanced_sphere_tracing.pdf
        //

        static float compute_distance(const double x, const double y, const double z)
        {
            return compute_distance(asf::Vector3d(x, y, z));
        }

        static float compute_distance(const asf::Vector3d& p)
        {
            return compute_distance(asf::Vector3f(p));
        }

        bool raymarch(
            const asr::ShadingRay&  ray,
            double&                 t_out,
            asf::Vector3d&          p_out) const
        {
            const auto bbox = asf::AABB3d(compute_local_bbox());

            asr::ShadingRay clipped_ray(ray);
            const asf::RayInfo3d clipped_ray_info(clipped_ray);

            if (!asf::clip(clipped_ray, clipped_ray_info, bbox))
                return false;

            const double Epsilon = 1.0e-3;

            auto t = std::max(clipped_ray.m_tmin, Epsilon);

            for (std::size_t i = 0; t < clipped_ray.m_tmax; ++i)
            {
                const auto p = clipped_ray.point_at(t);
                const auto d = compute_distance(p);

                if (d < 0.0001 || i == 64)
                {
                    t_out = t;
                    p_out = p;
                    return true;
                }

                t += d;
            }

            return false;
        }
    };


    //
    // Factory for the new object model.
    //

    class DistanceFieldObjectFactory
      : public asr::IObjectFactory
    {
      public:
        // Delete this instance.
        void release() override
        {
            delete this;
        }

        // Return a string identifying this object model.
        const char* get_model() const override
        {
            return Model;
        }

        // Return metadata for this object model.
        asf::Dictionary get_model_metadata() const override
        {
            return
                asf::Dictionary()
                    .insert("name", Model)
                    .insert("label", "Distance Field Object");
        }

        // Return metadata for the inputs of this object model.
        asf::DictionaryArray get_input_metadata() const override
        {
            asf::DictionaryArray metadata;
            return metadata;
        }

        // Create a new single empty object.
        asf::auto_release_ptr<asr::Object> create(
            const char*                 name,
            const asr::ParamArray&      params) const override
        {
            return asf::auto_release_ptr<asr::Object>(new DistanceFieldObject(name, params));
        }

        // Create objects, potentially from external assets.
        bool create(
            const char*                 name,
            const asr::ParamArray&      params,
            const asf::SearchPaths&     search_paths,
            const bool                  omit_loading_assets,
            asr::ObjectArray&           objects) const override
        {
            objects.push_back(create(name, params).release());
            return true;
        }
    };
}


//
// Plugin entry point.
//

extern "C"
{
    APPLESEED_DLL_EXPORT asr::IObjectFactory* appleseed_create_object_factory()
    {
        return new DistanceFieldObjectFactory();
    }
}
