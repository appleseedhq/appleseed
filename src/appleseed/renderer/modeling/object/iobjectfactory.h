
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

#pragma once

// appleseed.foundation headers.
#include "foundation/core/concepts/iunknown.h"
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/utility/api/apiarray.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace foundation    { class Dictionary; }
namespace foundation    { class DictionaryArray; }
namespace foundation    { class SearchPaths; }
namespace renderer      { class Object; }
namespace renderer      { class ParamArray; }

namespace renderer
{

//
// An array of objects.
//

APPLESEED_DECLARE_APIARRAY(ObjectArray, Object*);


//
// Object factory interface.
//

class APPLESEED_DLLSYMBOL IObjectFactory
  : public foundation::IUnknown
{
  public:
    // Return a string identifying this object model.
    virtual const char* get_model() const = 0;

    // Return metadata for this object model.
    virtual foundation::Dictionary get_model_metadata() const = 0;

    // Return metadata for the inputs of this object model.
    virtual foundation::DictionaryArray get_input_metadata() const = 0;

    // Create a new single empty object.
    virtual foundation::auto_release_ptr<Object> create(
        const char*                     name,
        const ParamArray&               params) const = 0;

    // Create objects, potentially from external assets.
    virtual bool create(
        const char*                     name,
        const ParamArray&               params,
        const foundation::SearchPaths&  search_paths,
        const bool                      omit_loading_assets,
        ObjectArray&                    objects) const = 0;
};

}   // namespace renderer
