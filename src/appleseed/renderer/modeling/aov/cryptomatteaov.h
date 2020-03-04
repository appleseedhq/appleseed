
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Sergo Pogosyan, The appleseedhq Organization
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
#include "renderer/modeling/aov/aov.h"
#include "renderer/modeling/aov/iaovfactory.h"

// appleseed.foundation headers.
#include "foundation/memory/autoreleaseptr.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class ImageAttributes; }
namespace foundation    { class Dictonary; }
namespace foundation    { class DictonaryArray; }
namespace renderer      { class ImageStack; }
namespace renderer      { class ParamArray; }

namespace renderer
{

//
// Cryptomatte AOV.
//

class APPLESEED_DLLSYMBOL CryptomatteAOV
  : public AOV
{
  public:
    enum class CryptomatteType
    {
        ObjectNames,
        MaterialNames,
    };

    explicit CryptomatteAOV(const ParamArray& params);

    ~CryptomatteAOV() override;

    const char* get_model() const override;

    size_t get_channel_count() const override;

    const char** get_channel_names() const override;

    bool has_color_data() const override;

    foundation::Image* get_cryptomatte_image() const;

    void create_image(
        const size_t    canvas_width,
        const size_t    canvas_height,
        const size_t    tile_width,
        const size_t    tile_height,
        ImageStack&     aov_images) override;

    void clear_image() override;

    foundation::auto_release_ptr<AOVAccumulator> create_accumulator() const override;

    bool write_images(
        const char*                         file_path,
        const foundation::ImageAttributes&  image_attributes) const override;

  private:
    friend class CryptomatteAOVFactory;

    struct Impl;
    Impl* impl;
};


//
// A factory for Cryptomatte AOVs.
//

class APPLESEED_DLLSYMBOL CryptomatteAOVFactory
  : public IAOVFactory
{
  public:
    CryptomatteAOVFactory(const CryptomatteAOV::CryptomatteType aov_type);

    // Delete this instance.
    void release() override;

    // Return a string identifying this AOV model.
    const char* get_model() const override;

    // Return metadata for this AOV model.
    foundation::Dictionary get_model_metadata() const override;

    // Return metadata for the inputs of this AOV model.
    foundation::DictionaryArray get_input_metadata() const override;

    // Create a new AOV instance.
    foundation::auto_release_ptr<AOV> create(const ParamArray& params) const override;

  private:
    CryptomatteAOV::CryptomatteType m_aov_type;
};

}       // namespace renderer

