
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_MODELING_LIGHT_LIGHT_H
#define APPLESEED_RENDERER_MODELING_LIGHT_LIGHT_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/entity/connectableentity.h"

// appleseed.foundation headers.
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/utility/uid.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class Assembly; }
namespace renderer      { class InputEvaluator; }
namespace renderer      { class LightTargetArray; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Project; }

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
    ~Light();

    // Return a string identifying the model of this entity.
    virtual const char* get_model() const = 0;

    enum Flags
    {
        CastIndirectLight = 1 << 0                                  // does this light generate indirect lighting?
    };

    // Retrieve the flags.
    int get_flags() const;

    // Retrieve the importance multiplier.
    double get_uncached_importance_multiplier() const;

    // Set the light transformation.
    void set_transform(const foundation::Transformd& transform);

    // Get the light transformation.
    const foundation::Transformd& get_transform() const;

    // This method is called once before rendering each frame.
    // Returns true on success, false otherwise.
    virtual bool on_frame_begin(
        const Project&                  project,
        const Assembly&                 assembly,
        foundation::IAbortSwitch*       abort_switch = 0);

    // This method is called once after rendering each frame.
    virtual void on_frame_end(
        const Project&                  project,
        const Assembly&                 assembly);

    // Sample the light and compute the emission position, the emission direction,
    // its probability density and the value of the light for this direction.
    virtual void sample(
        InputEvaluator&                 input_evaluator,
        const foundation::Transformd&   light_transform,            // light space to world space transform
        const foundation::Vector2d&     s,                          // sample in [0,1)^2
        foundation::Vector3d&           position,                   // world space emission position
        foundation::Vector3d&           outgoing,                   // world space emission direction, unit-length
        Spectrum&                       value,                      // light value
        double&                         probability) const = 0;     // PDF value
    virtual void sample(
        InputEvaluator&                 input_evaluator,
        const foundation::Transformd&   light_transform,            // light space to world space transform
        const foundation::Vector2d&     s,                          // sample in [0,1)^2
        const LightTargetArray&         targets,
        foundation::Vector3d&           position,                   // world space emission position
        foundation::Vector3d&           outgoing,                   // world space emission direction, unit-length
        Spectrum&                       value,                      // light value
        double&                         probability) const;         // PDF value

    // Evaluate the light for a given target point.
    virtual void evaluate(
        InputEvaluator&                 input_evaluator,
        const foundation::Transformd&   light_transform,            // light space to world space transform
        const foundation::Vector3d&     target,                     // world space target point
        foundation::Vector3d&           position,                   // world space emission position
        foundation::Vector3d&           outgoing,                   // world space emission direction, unit-length
        Spectrum&                       value) const = 0;           // light value

    // Compute the distance attenuation of this light.
    virtual double compute_distance_attenuation(
        const foundation::Vector3d&     target,                     // world space target point
        const foundation::Vector3d&     position) const = 0;        // world space emission position

  private:
    struct Impl;
    Impl* impl;

    int m_flags;
};


//
// Light class implementation.
//

inline int Light::get_flags() const
{
    return m_flags;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_LIGHT_LIGHT_H
