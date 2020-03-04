
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2018 Francois Beaune, The appleseedhq Organization
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
#include "renderer/modeling/texture/itexturefactory.h"

// appleseed.foundation headers.
#include "foundation/image/image.h"
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/platform/compiler.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace foundation    { class Dictionary; }
namespace foundation    { class DictionaryArray; }
namespace foundation    { class SearchPaths; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Texture; }

namespace renderer
{

//
// Factory for 2D in-memory textures.
//

class APPLESEED_DLLSYMBOL MemoryTexture2dFactory
  : public ITextureFactory
{
  public:
    // Delete this instance.
    void release() override;

    // Return a string identifying this texture model.
    const char* get_model() const override;

    // Return metadata for this texture model.
    foundation::Dictionary get_model_metadata() const override;

    // Return metadata for the inputs of this texture model.
    foundation::DictionaryArray get_input_metadata() const override;

    // Create a new dummy 1x1 blank texture.
    // This method does not allow to pass texture data.
    foundation::auto_release_ptr<Texture> create(
        const char*                                     name,
        const ParamArray&                               params,
        const foundation::SearchPaths&                  search_paths) const override;

    // Create a texture with texture data.
    foundation::auto_release_ptr<Texture> create(
        const char*                                     name,
        const ParamArray&                               params,
        foundation::auto_release_ptr<foundation::Image> image) const;
};

}   // namespace renderer
