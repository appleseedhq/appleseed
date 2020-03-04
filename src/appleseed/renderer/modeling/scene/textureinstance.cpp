
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

// Interface header.
#include "textureinstance.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/modeling/scene/basegroup.h"
#include "renderer/modeling/texture/texture.h"
#include "renderer/modeling/texture/tileptr.h"
#include "renderer/utility/messagecontext.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/tile.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/makevector.h"

// Standard headers.
#include <cstddef>
#include <string>

using namespace foundation;

namespace renderer
{

//
// TextureInstance class implementation.
//

namespace
{
    const UniqueID g_class_uid = new_guid();
}

UniqueID TextureInstance::get_class_uid()
{
    return g_class_uid;
}

struct TextureInstance::Impl
{
    // Order of data members impacts performance, preserve it.
    Transformf              m_transform;
    std::string             m_texture_name;
};

TextureInstance::TextureInstance(
    const char*             name,
    const ParamArray&       params,
    const char*             texture_name,
    const Transformf&       transform)
  : Entity(g_class_uid, params)
  , impl(new Impl())
{
    set_name(name);

    impl->m_transform = transform;
    impl->m_texture_name = texture_name;

    // No bound texture yet.
    m_texture = nullptr;

    const EntityDefMessageContext context("texture instance", this);

    // Retrieve the texture addressing mode.
    const std::string addressing_mode =
        m_params.get_optional<std::string>("addressing_mode", "wrap", make_vector("clamp", "wrap"), context);
    if (addressing_mode == "clamp")
        m_addressing_mode = TextureAddressingClamp;
    else m_addressing_mode = TextureAddressingWrap;

    // Retrieve the texture filtering mode.
    const std::string filtering_mode =
        m_params.get_optional<std::string>("filtering_mode", "bilinear", make_vector("nearest", "bilinear"), context);
    if (filtering_mode == "nearest")
        m_filtering_mode = TextureFilteringNearest;
    else m_filtering_mode = TextureFilteringBilinear;

    // Retrieve the texture alpha mode.
    const std::string alpha_mode =
        m_params.get_optional<std::string>("alpha_mode", "alpha_channel", make_vector("alpha_channel", "luminance", "detect"), context);
    if (alpha_mode == "alpha_channel")
        m_alpha_mode = TextureAlphaModeAlphaChannel;
    else if (alpha_mode == "luminance")
        m_alpha_mode = TextureAlphaModeLuminance;
    else m_alpha_mode = TextureAlphaModeDetect;

    // Until a texture is bound, the effective alpha mode is simply the user-selected alpha mode.
    m_effective_alpha_mode = m_alpha_mode;
}

TextureInstance::~TextureInstance()
{
    delete impl;
}

void TextureInstance::release()
{
    delete this;
}

std::uint64_t TextureInstance::compute_signature() const
{
    return
        m_texture
            ? combine_signatures(Entity::compute_signature(), m_texture->compute_signature())
            : Entity::compute_signature();
}

const char* TextureInstance::get_texture_name() const
{
    return impl->m_texture_name.c_str();
}

const Transformf& TextureInstance::get_transform() const
{
    return impl->m_transform;
}

Texture* TextureInstance::find_texture() const
{
    const Entity* parent = get_parent();

    while (parent)
    {
        const BaseGroup* parent_base_group = dynamic_cast<const BaseGroup*>(parent);
        assert(parent_base_group);

        Texture* texture =
            parent_base_group->textures().get_by_name(impl->m_texture_name.c_str());

        if (texture)
            return texture;

        parent = parent->get_parent();
    }

    return nullptr;
}

void TextureInstance::unbind_texture()
{
    m_texture = nullptr;
}

namespace
{
    bool has_transparent_pixels(const Tile& tile)
    {
        for (size_t i = 0, e = tile.get_pixel_count(); i < e; ++i)
        {
            if (tile.get_component<float>(i, 3) < 1.0f)
                return true;
        }

        return false;
    }

    TextureAlphaMode detect_alpha_mode(Texture& texture)
    {
        const CanvasProperties& props = texture.properties();

        if (props.m_channel_count >= 4)
        {
            for (size_t y = 0; y < props.m_tile_count_y; ++y)
            {
                for (size_t x = 0; x < props.m_tile_count_x; ++x)
                {
                    const TilePtr tile_ptr = texture.load_tile(x, y);
                    const bool has_transparency = has_transparent_pixels(*tile_ptr.get_tile());
                    if (tile_ptr.has_ownership())
                        delete tile_ptr.get_tile();

                    if (has_transparency)
                        return TextureAlphaModeAlphaChannel;
                }
            }
        }

        return TextureAlphaModeLuminance;
    }
}

void TextureInstance::bind_texture(const TextureContainer& textures)
{
    if (m_texture == nullptr)
    {
        m_texture = textures.get_by_name(impl->m_texture_name.c_str());

        // We need to resolve the alpha mode as soon as a texture is bound to this instance.
        // We cannot do it in on_frame_begin() because the texture instance might be needed
        // before it gets called. For instance, updating the trace context implies updating
        // the intersection filters, and those need to be able to sample texture instances.
        if (m_effective_alpha_mode == TextureAlphaModeDetect)
        {
            m_effective_alpha_mode = detect_alpha_mode(*m_texture);

            RENDERER_LOG_DEBUG(
                "texture instance \"%s\" was detected to use the \"%s\" alpha mode.",
                get_path().c_str(),
                m_effective_alpha_mode == TextureAlphaModeAlphaChannel ? "alpha_channel" : "luminance");
        }
    }
}

void TextureInstance::check_texture() const
{
    if (m_texture == nullptr)
        throw ExceptionUnknownEntity(impl->m_texture_name.c_str(), this);
}


//
// TextureInstanceFactory class implementation.
//

DictionaryArray TextureInstanceFactory::get_input_metadata()
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "addressing_mode")
            .insert("label", "Addressing Mode")
            .insert("type", "enumeration")
            .insert("items",
                Dictionary()
                    .insert("Clamp", "clamp")
                    .insert("Wrap/Tile", "wrap"))
            .insert("use", "optional")
            .insert("default", "wrap"));

    metadata.push_back(
        Dictionary()
            .insert("name", "filtering_mode")
            .insert("label", "Filtering Mode")
            .insert("type", "enumeration")
            .insert("items",
                Dictionary()
                    .insert("Nearest", "nearest")
                    .insert("Bilinear", "bilinear"))
            .insert("use", "optional")
            .insert("default", "bilinear"));

    metadata.push_back(
        Dictionary()
            .insert("name", "alpha_mode")
            .insert("label", "Alpha Mode")
            .insert("type", "enumeration")
            .insert("items",
                Dictionary()
                    .insert("Alpha Channel", "alpha_channel")
                    .insert("Luminance", "luminance")
                    .insert("Detect", "detect"))
            .insert("use", "optional")
            .insert("default", "alpha_channel"));

    return metadata;
}

auto_release_ptr<TextureInstance> TextureInstanceFactory::create(
    const char*             name,
    const ParamArray&       params,
    const char*             texture_name,
    const Transformf&       transform)
{
    return
        auto_release_ptr<TextureInstance>(
            new TextureInstance(
                name,
                params,
                texture_name,
                transform));
}

}   // namespace renderer
