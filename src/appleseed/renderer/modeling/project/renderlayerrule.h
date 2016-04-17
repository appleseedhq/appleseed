
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_MODELING_PROJECT_RENDERLAYERRULE_H
#define APPLESEED_RENDERER_MODELING_PROJECT_RENDERLAYERRULE_H

// appleseed.renderer headers.
#include "renderer/modeling/entity/entity.h"

// appleseed.foundation headers.
#include "foundation/utility/uid.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace foundation    { class DictionaryArray; }
namespace renderer      { class ParamArray; }

namespace renderer
{

//
// Base class for render layer rules.
//

class APPLESEED_DLLSYMBOL RenderLayerRule
  : public Entity
{
  public:
    // Return the unique ID of this class of entities.
    static foundation::UniqueID get_class_uid();

    // Constructor.
    RenderLayerRule(
        const char*         name,
        const ParamArray&   params);

    // Destructor.
    ~RenderLayerRule();

    // Return a string identifying the model of this entity.
    virtual const char* get_model() const = 0;

    // Return the name of the render layer.
    const char* get_render_layer() const;

    // Return the entity type this rule is restricted to, or ~0 if there is no type restriction.
    const foundation::UniqueID get_entity_type_uid() const;

    // Return the order value of this rule.
    int get_order() const;

    // Return true if this rule applies to a given entity.
    virtual bool applies(const Entity& entity) const = 0;

  private:
    struct Impl;
    Impl* impl;
};


//
// An incomplete factory class whose main purpose is to factorize the code
// common to all render layer rule models.
//

class APPLESEED_DLLSYMBOL RenderLayerRuleFactory
{
  public:
    // Return a set of input metadata common to all render layer rule models.
    static foundation::DictionaryArray get_input_metadata();
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_PROJECT_RENDERLAYERRULE_H
