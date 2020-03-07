
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
    // A sphere object.
    //
    // The sphere is assumed to be centered at the origin.
    //

    const char* Model = "example_sphere_object";

    class SphereObject
      : public asr::ProceduralObject
    {
      public:
        // Constructor.
        SphereObject(
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

        // This method is called once before rendering each frame.
        // Returns true on success, false otherwise.
        bool on_frame_begin(
            const asr::Project&         project,
            const asr::BaseGroup*       parent,
            asr::OnFrameBeginRecorder&  recorder,
            asf::IAbortSwitch*          abort_switch) override
        {
            if (!asr::ProceduralObject::on_frame_begin(project, parent, recorder, abort_switch))
                return false;

            m_radius = get_uncached_radius();
            m_rcp_radius = 1.0 / m_radius;

            return true;
        }

        // Compute the local space bounding box of the object over the shutter interval.
        asr::GAABB3 compute_local_bbox() const override
        {
            const auto r = static_cast<asr::GScalar>(get_uncached_radius());
            return asr::GAABB3(asr::GVector3(-r), asr::GVector3(r));
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

            const double a = asf::dot(ray.m_org, ray.m_dir);
            const double b = asf::square(a) - dot(ray.m_org, ray.m_org) + asf::square(m_radius);

            if (b < 0.0)
            {
                result.m_hit = false;
                return;
            }

            const double c = std::sqrt(b);

            double t = -a - c;
            if (t < std::max(ray.m_tmin, Epsilon) || t >= ray.m_tmax)
            {
                t = -a + c;
                if (t < std::max(ray.m_tmin, Epsilon) || t >= ray.m_tmax)
                {
                    result.m_hit = false;
                    return;
                }
            }

            result.m_hit = true;
            result.m_distance = t;

            const asf::Vector3d n = asf::normalize(ray.point_at(t));
            result.m_geometric_normal = n;
            result.m_shading_normal = n;

            const asf::Vector3f p(ray.point_at(t) * m_rcp_radius);
            result.m_uv[0] = std::acos(p.y) * asf::RcpPi<float>();
            result.m_uv[1] = std::atan2(-p.z, p.x) * asf::RcpTwoPi<float>();

            result.m_material_slot = 0;
        }

        // Compute the intersection between a ray expressed in object space and
        // the surface of this object and simply return whether there was a hit.
        bool intersect(
            const asr::ShadingRay&  ray) const override
        {
            const double Epsilon = 1.0e-6;

            const double a = asf::dot(ray.m_org, ray.m_dir);
            const double b = asf::square(a) - dot(ray.m_org, ray.m_org) + asf::square(m_radius);

            if (b < 0.0)
                return false;

            const double c = std::sqrt(b);

            const double t1 = -a - c;
            if (t1 >= std::max(ray.m_tmin, Epsilon) && t1 < ray.m_tmax)
                return true;

            const double t2 = -a + c;
            if (t2 >= std::max(ray.m_tmin, Epsilon) && t2 < ray.m_tmax)
                return true;

            return false;
        }

      private:
        double  m_radius;
        double  m_rcp_radius;

        double get_uncached_radius() const
        {
            return m_params.get_optional<double>("radius");
        }
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

            metadata.push_back(
                asf::Dictionary()
                    .insert("name", "radius")
                    .insert("label", "Radius")
                    .insert("type", "numeric")
                    .insert("min",
                        asf::Dictionary()
                            .insert("value", "0.0")
                            .insert("type", "hard"))
                    .insert("max",
                        asf::Dictionary()
                            .insert("value", "10.0")
                            .insert("type", "soft"))
                    .insert("use", "optional")
                    .insert("default", "1.0"));

            return metadata;
        }

        // Create a new single empty object.
        asf::auto_release_ptr<asr::Object> create(
            const char*                 name,
            const asr::ParamArray&      params) const override
        {
            return asf::auto_release_ptr<asr::Object>(new SphereObject(name, params));
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
