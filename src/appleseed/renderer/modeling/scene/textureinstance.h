
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
#include "renderer/modeling/entity/entity.h"
#include "renderer/modeling/scene/containers.h"

// appleseed.foundation headers.
#include "foundation/math/transform.h"
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/uid.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cassert>
#include <cstdint>

// Forward declarations.
namespace foundation    { class DictionaryArray; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Texture; }

namespace renderer
{

//
// Texture modes.
//

enum TextureAddressingMode
{
    TextureAddressingClamp,
    TextureAddressingWrap
};

enum TextureFilteringMode
{
    TextureFilteringNearest,
    TextureFilteringBilinear,
    TextureFilteringBicubic,
    TextureFilteringFeline,             // Reference: http://www.hpl.hp.com/techreports/Compaq-DEC/WRL-99-1.pdf
    TextureFilteringEWA
};

enum TextureAlphaMode
{
    TextureAlphaModeAlphaChannel,
    TextureAlphaModeLuminance,
    TextureAlphaModeDetect
};


//
// An instance of a texture.
//
// todo: allow to specify the lighting conditions of a texture.
//

class APPLESEED_DLLSYMBOL TextureInstance
  : public Entity
{
  public:
    // Return the unique ID of this class of entities.
    static foundation::UniqueID get_class_uid();

    // Delete this instance.
    void release() override;

    // Compute and return the unique signature of this instance.
    std::uint64_t compute_signature() const override;

    // Return the name of the instantiated texture.
    const char* get_texture_name() const;

    // Return the texture transform.
    const foundation::Transformf& get_transform() const;

    // Return the modes.
    TextureAddressingMode get_addressing_mode() const;
    TextureFilteringMode get_filtering_mode() const;
    TextureAlphaMode get_alpha_mode() const;

    // Find the texture bound to this instance.
    Texture* find_texture() const;

    // Texture binding.
    void unbind_texture();
    void bind_texture(const TextureContainer& textures);
    void check_texture() const;

    // Return the texture bound to this instance.
    Texture& get_texture() const;

    // Return the effective (detected) alpha mode.
    // A texture must be bound to this instance.
    TextureAlphaMode get_effective_alpha_mode() const;

  private:
    friend class TextureInstanceFactory;

    struct Impl;
    Impl* impl;

    TextureAddressingMode   m_addressing_mode;
    TextureFilteringMode    m_filtering_mode;
    TextureAlphaMode        m_alpha_mode;
    TextureAlphaMode        m_effective_alpha_mode;
    Texture*                m_texture;

    // Constructor.
    TextureInstance(
        const char*                     name,
        const ParamArray&               params,
        const char*                     texture_name,
        const foundation::Transformf&   transform);

    // Destructor.
    ~TextureInstance() override;
};


//
// Texture instance factory.
//

class APPLESEED_DLLSYMBOL TextureInstanceFactory
{
  public:
    // Return a set of input metadata for texture instance entities.
    static foundation::DictionaryArray get_input_metadata();

    // Create a new texture instance.
    static foundation::auto_release_ptr<TextureInstance> create(
        const char*                     name,
        const ParamArray&               params,
        const char*                     texture_name,
        const foundation::Transformf&   transform = foundation::Transformf::identity());
};


//
// TextureInstance class implementation.
//

inline TextureAddressingMode TextureInstance::get_addressing_mode() const
{
    return m_addressing_mode;
}

inline TextureFilteringMode TextureInstance::get_filtering_mode() const
{
    return m_filtering_mode;
}

inline TextureAlphaMode TextureInstance::get_alpha_mode() const
{
    return m_alpha_mode;
}

inline Texture& TextureInstance::get_texture() const
{
    assert(m_texture);

    return *m_texture;
}

inline TextureAlphaMode TextureInstance::get_effective_alpha_mode() const
{
    assert(m_texture);

    return m_effective_alpha_mode;
}

}   // namespace renderer
