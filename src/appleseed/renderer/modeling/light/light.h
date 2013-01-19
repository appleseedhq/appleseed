
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace renderer  { class Assembly; }
namespace renderer  { class InputEvaluator; }
namespace renderer  { class ParamArray; }
namespace renderer  { class Project; }

namespace renderer
{

//
// Shape-less light.
//

class DLLSYMBOL Light
  : public ConnectableEntity
{
  public:
    // Constructor.
    Light(
        const char*                     name,
        const ParamArray&               params);

    // Destructor.
    ~Light();

    // Return a string identifying the model of this entity.
    virtual const char* get_model() const = 0;

    // Set the light transformation.
    void set_transform(const foundation::Transformd& transform);

    // Get the light transformation.
    const foundation::Transformd& get_transform() const;

    // This method is called once before rendering each frame.
    // Returns true on success, false otherwise.
    virtual bool on_frame_begin(
        const Project&                  project,
        const Assembly&                 assembly);

    // This method is called once after rendering each frame.
    virtual void on_frame_end(
        const Project&                  project,
        const Assembly&                 assembly);

    // Sample the light and compute the emission position, the emission direction,
    // the probability density with which this direction was chosen and the value
    // of the light for this direction.
    virtual void sample(
        InputEvaluator&                 input_evaluator,
        const foundation::Vector2d&     s,                          // sample in [0,1)^2
        foundation::Vector3d&           position,                   // world space emission position
        foundation::Vector3d&           outgoing,                   // world space emission direction, unit-length
        Spectrum&                       value,                      // light value
        double&                         probability) const = 0;     // PDF value

    // Evaluate the light for a given target point.
    virtual void evaluate(
        InputEvaluator&                 input_evaluator,
        const foundation::Vector3d&     target,                     // world space target point
        foundation::Vector3d&           position,                   // world space emission position
        foundation::Vector3d&           outgoing,                   // world space emission direction, unit-length
        Spectrum&                       value) const = 0;           // light value

  private:
    struct Impl;
    Impl* impl;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_LIGHT_LIGHT_H
