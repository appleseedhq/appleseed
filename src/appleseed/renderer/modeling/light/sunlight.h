
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_MODELING_LIGHT_SUNLIGHT_H
#define APPLESEED_RENDERER_MODELING_LIGHT_SUNLIGHT_H

// appleseed.renderer headers.
#include "renderer/modeling/light/ilightfactory.h"
#include "renderer/modeling/light/light.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/utility/autoreleaseptr.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace foundation    { class Dictionary; }
namespace foundation    { class DictionaryArray; }
namespace renderer      { class ParamArray; }

namespace renderer
{

//
// Sun light factory.
//

class APPLESEED_DLLSYMBOL SunLightFactory
  : public ILightFactory
{
  public:
    // Return a string identifying this light model.
    virtual const char* get_model() const APPLESEED_OVERRIDE;

    // Return metadata for this light model.
    virtual foundation::Dictionary get_model_metadata() const APPLESEED_OVERRIDE;

    // Return metadata for the inputs of this light model.
    virtual foundation::DictionaryArray get_input_metadata() const APPLESEED_OVERRIDE;

    // Create a new light instance.
    virtual foundation::auto_release_ptr<Light> create(
        const char*         name,
        const ParamArray&   params) const APPLESEED_OVERRIDE;

    // Static variant of the create() method above.
    static foundation::auto_release_ptr<Light> static_create(
        const char*         name,
        const ParamArray&   params);
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_LIGHT_SUNLIGHT_H
