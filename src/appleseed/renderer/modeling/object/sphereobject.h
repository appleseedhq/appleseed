
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) Mayank Dhiman, The appleseedhq Organization
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

// appleseed.main headers.
#include "main/dllvisibility.h"

// Standard headers.
#include <algorithm>
#include <cmath>

// Forward Declarations
namespace asf = foundation;
namespace asr = renderer;
namespace
{
    //
    // A sphere object.
    //
    // The sphere is assumed to be centered at the origin.
    //

    const char* Model = "sphere_object";

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
        void release() override;

        // Return a string identifying this object model.
        const char* get_model() const override;

        double m_radius;
        double m_rcp_radius;

        // This method is called once before rendering each frame.
        // Returns true on success, false otherwise.
        virtual bool on_frame_begin(
            const asr::Project&         project,
            const asr::BaseGroup*       parent,
            asr::OnFrameBeginRecorder&  recorder,
            asf::IAbortSwitch*          abort_switch) override;

        // Compute the local space bounding box of the object over the shutter interval.
        asr::GAABB3 compute_local_bbox() const override;

        // Access materials slots.
        size_t get_material_slot_count() const override;

        const char* get_material_slot(const size_t index) const override;

        // Compute the intersection between a ray expressed in object space and
        // the surface of this object and return detailed intersection results.
        void intersect(
            const asr::ShadingRay&  ray,
            IntersectionResult&     result) const override;

        // Compute the intersection between a ray expressed in object space and
        // the surface of this object and simply return whether there was a hit.
        bool intersect(const asr::ShadingRay&  ray) const override;

        double get_uncached_radius() const;
    };

    //
    // Factory for the new object model.
    //

    class SphereObjectFactory
      : public asr::IObjectFactory
    {
      public:
        // Delete this instance.
        void release() override;

        // Return a string identifying this object model.
        const char* get_model() const override;

        // Return metadata for this object model.
        asf::Dictionary get_model_metadata() const override;

        // Return metadata for the inputs of this object model.
        asf::DictionaryArray get_input_metadata() const override;

        // Create a new single empty object.
        asf::auto_release_ptr<asr::Object> create(
            const char*                 name,
            const asr::ParamArray&      params) const override;        

        // Create objects, potentially from external assets.
        bool create(
            const char*                 name,
            const asr::ParamArray&      params,
            const asf::SearchPaths&     search_paths,
            const bool                  omit_loading_assets,
            asr::ObjectArray&           objects) const override;
    };
}

