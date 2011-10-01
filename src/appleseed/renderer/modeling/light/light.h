
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

// Forward declarations.
namespace renderer      { class Assembly; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Project; }

namespace renderer
{

//
// Shape-less light.
//

class RENDERERDLL Light
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

    // Return the transform of this light.
    const foundation::Transformd& get_transform() const;

    // This method is called once before rendering each frame.
    virtual void on_frame_begin(
        const Project&                  project,
        const Assembly&                 assembly,
        const void*                     data);                      // input values

    // This method is called once after rendering each frame.
    virtual void on_frame_end(
        const Project&                  project,
        const Assembly&                 assembly);

    // Sample the light and compute the emission direction, the probability
    // density with which it was chosen and the value of the light for this
    // direction.
    virtual void sample(
        const void*                     data,                       // input values
        const foundation::Vector2d&     s,                          // sample in [0,1)^2
        foundation::Vector3d&           outgoing,                   // world space emission direction, unit-length
        Spectrum&                       value,                      // light value for this direction
        double&                         probability) const = 0;     // PDF value

    // Evaluate the light for a given emission direction.
    virtual void evaluate(
        const void*                     data,                       // input values
        const foundation::Vector3d&     outgoing,                   // world space emission direction, unit-length
        Spectrum&                       value) const = 0;           // light value for this direction
    virtual void evaluate(
        const void*                     data,                       // input values
        const foundation::Vector3d&     outgoing,                   // world space emission direction, unit-length
        Spectrum&                       value,                      // EDF value for this direction
        double&                         probability) const = 0;     // light value

    // Evaluate the PDF for a given emission direction.
    virtual double evaluate_pdf(
        const void*                     data,                       // input values
        const foundation::Vector3d&     outgoing) const = 0;        // world space emission direction, unit-length

  private:
    struct Impl;
    Impl* impl;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_LIGHT_LIGHT_H
