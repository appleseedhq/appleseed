
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "renderer/global/global.h"
#include "renderer/modeling/entity/entity.h"
#include "renderer/modeling/scene/containers.h"

// appleseed.foundation headers.
#include "foundation/math/transform.h"

// Forward declarations.
namespace renderer      { class EDF; }

namespace renderer
{

//
// Shape-less light.
//

class RENDERERDLL Light
  : public Entity
{
  public:
    // Delete this instance.
    virtual void release();

    // Return a string identifying the model of this light.
    virtual const char* get_model() const;

    // Return the transform of this light.
    const foundation::Transformd& get_transform() const;

    // Return the EDF of this light.
    const EDF* get_edf() const;

  private:
    friend class LightFactory;

    // Private implementation.
    struct Impl;
    Impl* impl;

    // Constructors.
    Light(
        const char*                     name,
        const foundation::Transformd&   transform,
        const EDF*                      edf);
    Light(
        const char*                     name,
        const ParamArray&               params,
        const foundation::Transformd&   transform,
        const EDFContainer&             edfs);

    // Destructor.
    ~Light();
};


//
// Light factory.
//

class RENDERERDLL LightFactory
{
  public:
    // Return a string identifying this light model.
    static const char* get_model();

    // Create a new light.
    static foundation::auto_release_ptr<Light> create(
        const char*                     name,
        const foundation::Transformd&   transform,
        const EDF*                      edf);
    static foundation::auto_release_ptr<Light> create(
        const char*                     name,
        const ParamArray&               params,
        const foundation::Transformd&   transform,
        const EDFContainer&             edfs);
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_LIGHT_LIGHT_H
