
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz
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

// appleseed.foundation headers.
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"

using namespace foundation;
using namespace std;

namespace renderer
{

//
// TextureInstance class implementation.
//

struct TextureInstance::Impl
{
    size_t                  m_texture_index;
    TextureAddressingMode   m_addressing_mode;
    TextureFilteringMode    m_filtering_mode;
    float                   m_multiplier;
};

namespace
{
    const UniqueID g_class_uid = new_guid();
}

TextureInstance::TextureInstance(
    const char*             name,
    const ParamArray&       params,
    const size_t            texture_index)
  : Entity(g_class_uid, params)
  , impl(new Impl())
{
    set_name(name);

    impl->m_texture_index = texture_index;

    // Retrieve the texture addressing mode.
    const string addressing_mode = m_params.get_required<string>("addressing_mode", "wrap");
    if (addressing_mode == "clamp")
        impl->m_addressing_mode = TextureAddressingClamp;
    else if (addressing_mode == "wrap")
        impl->m_addressing_mode = TextureAddressingWrap;
    else
    {
        RENDERER_LOG_ERROR(
            "invalid value \"%s\" for parameter \"addressing_mode\", ",
            "using default value \"wrap\"",
            addressing_mode.c_str());
        impl->m_addressing_mode = TextureAddressingWrap;
    }

    // Retrieve the texture filtering mode.
    const string filtering_mode = m_params.get_required<string>("filtering_mode", "bilinear");
    if (filtering_mode == "nearest")
        impl->m_filtering_mode = TextureFilteringNearest;
    else if (filtering_mode == "bilinear")
        impl->m_filtering_mode = TextureFilteringBilinear;
    else
    {
        RENDERER_LOG_ERROR(
            "invalid value \"%s\" for parameter \"filtering_mode\", ",
            "using default value \"bilinear\"",
            filtering_mode.c_str());
        impl->m_filtering_mode = TextureFilteringBilinear;
    }

    // Retrieve multiplier.
    impl->m_multiplier = m_params.get_optional<float>("multiplier", 1.0f);
}

TextureInstance::~TextureInstance()
{
    delete impl;
}

void TextureInstance::release()
{
    delete this;
}

size_t TextureInstance::get_texture_index() const
{
    return impl->m_texture_index;
}

TextureAddressingMode TextureInstance::get_addressing_mode() const
{
    return impl->m_addressing_mode;
}

TextureFilteringMode TextureInstance::get_filtering_mode() const
{
    return impl->m_filtering_mode;
}

float TextureInstance::get_multiplier() const
{
    return impl->m_multiplier;
}


//
// TextureInstanceFactory class implementation.
//

DictionaryArray TextureInstanceFactory::get_widget_definitions()
{
    DictionaryArray definitions;

    definitions.push_back(
        Dictionary()
            .insert("name", "addressing_mode")
            .insert("label", "Addressing Mode")
            .insert("widget", "dropdown_list")
            .insert("dropdown_items",
                Dictionary()
                    .insert("Clamp", "clamp")
                    .insert("Wrap/Tile", "wrap"))
            .insert("use", "required")
            .insert("default", "wrap"));

    definitions.push_back(
        Dictionary()
            .insert("name", "filtering_mode")
            .insert("label", "Filtering Mode")
            .insert("widget", "dropdown_list")
            .insert("dropdown_items",
                Dictionary()
                    .insert("Nearest", "nearest")
                    .insert("Bilinear", "bilinear"))
            .insert("use", "required")
            .insert("default", "bilinear"));

    definitions.push_back(
        Dictionary()
            .insert("name", "multiplier")
            .insert("label", "Multiplier")
            .insert("widget", "text_box")
            .insert("default", "1.0")
            .insert("use", "optional"));

    return definitions;
}

auto_release_ptr<TextureInstance> TextureInstanceFactory::create(
    const char*             name,
    const ParamArray&       params,
    const size_t            texture_index)
{
    return
        auto_release_ptr<TextureInstance>(
            new TextureInstance(name, params, texture_index));
}

}   // namespace renderer
