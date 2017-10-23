
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Francois Beaune, The appleseedhq Organization
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
#include "renderer/api/rendering.h"
#include "renderer/api/types.h"

#include "renderer/kernel/shading/shadingray.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/ray.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/string.h"

// appleseed.main headers.
#include "main/dllvisibility.h"

// Standard headers.
#include <cmath>
#include <vector>

namespace asf = foundation;
namespace asr = renderer;

namespace
{
    //
    // New object model.
    //

    const char* Model = "sphere_object";

    class SphereObject
      : public asr::ProceduralObject
    {
      public:
        // Constructor.
        explicit SphereObject(const char* name)
          : asr::ProceduralObject(name, asr::ParamArray())
          , m_lazy_region_kit(&m_region_kit)
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
            return asr::GAABB3(asr::GVector3(-0.5f), asr::GVector3(0.5f));
        }

        // Return the region kit of the object.
        asf::Lazy<asr::RegionKit>& get_region_kit() override
        {
            return m_lazy_region_kit;
        }

        // Access materials slots.
        size_t get_material_slot_count() const override
        {
            return 1;
        }
        const char* get_material_slot(const size_t index) const override
        {
            return "default";
        }

        // Compute the intersection between a ray and the surface of this object.
        void intersect(
            const asr::ShadingRay&  ray,
            const asf::RayInfo3d&   ray_info,
            IntersectionResult&     result) const override
        {
            const asf::Vector3d SphereCenter(0.0, 0.0, 0.0);
            const double SphereRadius = 0.05;

            const asf::Vector3d v = SphereCenter - ray.m_org;
            const double a = asf::dot(v, ray.m_dir);
            const double b = a * a - dot(v, v) + asf::square(SphereRadius);

            if (b < 0.0)
            {
                result.m_hit = false;
                return;
            }

            const double c = std::sqrt(b);
            double t = a - c;

            if (t < 0.0)
            {
                t = a + c;
                if (t < 0.0)
                {
                    result.m_hit = false;
                    return;
                }
            }

            const asf::Vector3d n = asf::normalize(ray.point_at(t) - SphereCenter);

            result.m_hit = true;
            result.m_distance = t;
            result.m_geometric_normal = n;
            result.m_shading_basis.build(n);
            result.m_uv = asf::Vector2f(0.0f);
        }

      private:
        asr::RegionKit              m_region_kit;
        asf::Lazy<asr::RegionKit>   m_lazy_region_kit;
    };


    //
    // Factory for the new object model.
    //

    class SphereObjectFactory
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
                    .insert("label", "Sphere Object");
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
            return asf::auto_release_ptr<asr::Object>(new SphereObject(name));
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
        return new SphereObjectFactory();
    }
}
