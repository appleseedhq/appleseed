
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
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/entity/connectableentity.h"

// appleseed.foundation headers.
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/uid.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class BaseGroup; }
namespace renderer      { class LightTargetArray; }
namespace renderer      { class OnFrameBeginRecorder; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Project; }
namespace renderer      { class ShadingContext; }

namespace renderer
{

//
// Shape-less light.
//

class APPLESEED_DLLSYMBOL Light
  : public ConnectableEntity
{
  public:
    // Return the unique ID of this class of entities.
    static foundation::UniqueID get_class_uid();

    // Constructor.
    Light(
        const char*                     name,
        const ParamArray&               params);

    // Destructor.
    ~Light() override;

    // Return a string identifying the model of this entity.
    virtual const char* get_model() const = 0;

    enum Flags
    {
        CastShadows         = 1UL << 0,     // does this light cast shadows?
        CastIndirectLight   = 1UL << 1,     // does this light generate indirect lighting?
        LightTreeCompatible = 1UL << 2,     // can this light be used by the LightTree?
    };

    // Retrieve the flags.
    int get_flags() const;

    // Retrieve the importance multiplier.
    float get_uncached_importance_multiplier() const;

    // Set the light transformation.
    void set_transform(const foundation::Transformd& transform);

    // Get the light transformation.
    const foundation::Transformd& get_transform() const;

    bool on_frame_begin(
        const Project&                  project,
        const BaseGroup*                parent,
        OnFrameBeginRecorder&           recorder,
        foundation::IAbortSwitch*       abort_switch = nullptr) override;

    // Sample the light and compute the emission position, the emission direction,
    // its probability density and the value of the light for this direction.
    virtual void sample(
        const ShadingContext&           shading_context,
        const foundation::Transformd&   light_transform,            // light space to world space transform
        const foundation::Vector3d&     target_point,               // world space target point
        const foundation::Vector2d&     s,                          // sample in [0,1)^2
        foundation::Vector3d&           position,                   // world space emission position
        foundation::Vector3d&           outgoing,                   // world space emission direction, unit-length
        Spectrum&                       value,                      // light value
        float&                          probability) const = 0;     // PDF value
    virtual void sample(
        const ShadingContext&           shading_context,
        const foundation::Transformd&   light_transform,            // light space to world space transform
        const foundation::Vector2d&     s,                          // sample in [0,1)^2
        foundation::Vector3d&           position,                   // world space emission position
        foundation::Vector3d&           outgoing,                   // world space emission direction, unit-length
        Spectrum&                       value,                      // light value
        float&                          probability) const = 0;     // PDF value
    virtual void sample(
        const ShadingContext&           shading_context,
        const foundation::Transformd&   light_transform,            // light space to world space transform
        const foundation::Vector2d&     s,                          // sample in [0,1)^2
        const LightTargetArray&         targets,
        foundation::Vector3d&           position,                   // world space emission position
        foundation::Vector3d&           outgoing,                   // world space emission direction, unit-length
        Spectrum&                       value,                      // light value
        float&                          probability) const;         // PDF value

    // Compute the distance attenuation of this light.
    virtual float compute_distance_attenuation(
        const foundation::Vector3d&     target,                     // world space target point
        const foundation::Vector3d&     position) const = 0;        // world space emission position

  protected:
    int m_flags;

  private:
    struct Impl;
    Impl* impl;
};


//
// Light class implementation.
//

inline int Light::get_flags() const
{
    return m_flags;
}

}   // namespace renderer
