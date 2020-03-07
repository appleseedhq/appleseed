
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
#include "foundation/math/fp.h"
#include "foundation/math/ray.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/string/string.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/job/iabortswitch.h"
#include "foundation/utility/searchpaths.h"

// appleseed.main headers.
#include "main/dllvisibility.h"

// Standard headers.
#include <algorithm>
#include <cmath>

namespace asf = foundation;
namespace asr = renderer;

namespace
{
    //
    // An infinite plane in the X-Z plane.
    //

    const char* Model = "infinite_plane_object";

    class InfinitePlaneObject
      : public asr::ProceduralObject
    {
      public:
        // Constructor.
        InfinitePlaneObject(
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
            const asr::GScalar Eps = 1.0e-4f;
            const auto Inf = asf::FP<asr::GScalar>::pos_inf();
            return
                asr::GAABB3(
                    asr::GVector3(-Inf, -Eps, -Inf),
                    asr::GVector3(+Inf, +Eps, +Inf));
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

        // Compute the intersection between a ray expressed in object space and
        // the surface of this object and return detailed intersection results.
        void intersect(
            const asr::ShadingRay&  ray,
            IntersectionResult&     result) const override
        {
            const double Epsilon = 1.0e-6;

            if (ray.m_dir.y == 0.0)
            {
                result.m_hit = false;
                return;
            }

            const double t = -ray.m_org.y / ray.m_dir.y;
            if (t < std::max(ray.m_tmin, Epsilon) || t >= ray.m_tmax)
            {
                result.m_hit = false;
                return;
            }

            result.m_hit = true;
            result.m_distance = t;
            result.m_geometric_normal = asf::Vector3d(0.0, 1.0, 0.0);
            result.m_shading_normal = asf::Vector3d(0.0, 1.0, 0.0);
            result.m_uv = asf::Vector2f(0.0f);
            result.m_material_slot = 0;
        }

        // Compute the intersection between a ray expressed in object space and
        // the surface of this object and simply return whether there was a hit.
        bool intersect(
            const asr::ShadingRay&  ray) const override
        {
            const double Epsilon = 1.0e-6;

            if (ray.m_dir.y == 0.0)
                return false;

            const double t = -ray.m_org.y / ray.m_dir.y;
            if (t < std::max(ray.m_tmin, Epsilon) || t >= ray.m_tmax)
                return false;

            return true;
        }
    };


    //
    // Factory for the new object model.
    //

    class InfinitePlaneObjectFactory
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
                    .insert("label", "Infinite Plane Object");
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
            return asf::auto_release_ptr<asr::Object>(new InfinitePlaneObject(name, params));
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
        return new InfinitePlaneObjectFactory();
    }
}
