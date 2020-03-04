
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

// Interface header.
#include "memorytexture2d.h"

// appleseed.renderer headers.
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/input/texturesource.h"
#include "renderer/modeling/texture/texture.h"
#include "renderer/modeling/texture/tileptr.h"
#include "renderer/utility/messagecontext.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"
#include "foundation/image/pixel.h"
#include "foundation/image/tile.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/makevector.h"
#include "foundation/utility/uid.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <memory>
#include <string>

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // 2D in-memory texture.
    //

    const char* Model = "memory_texture_2d";

    class MemoryTexture2d
      : public Texture
    {
      public:
        MemoryTexture2d(
            const char*             name,
            const ParamArray&       params)
          : Texture(name, params)
          , m_dummy_texture(new DummyTexture())
        {
            extract_parameters();
        }

        MemoryTexture2d(
            const char*             name,
            const ParamArray&       params,
            auto_release_ptr<Image> image)
          : Texture(name, params)
          , m_image(image)
        {
            extract_parameters();
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return Model;
        }

        ColorSpace get_color_space() const override
        {
            return m_color_space;
        }

        bool on_frame_begin(
            const Project&          project,
            const BaseGroup*        parent,
            OnFrameBeginRecorder&   recorder,
            IAbortSwitch*           abort_switch) override
        {
            if (!Texture::on_frame_begin(project, parent, recorder, abort_switch))
                return false;

            if (m_image.get() == nullptr)
                RENDERER_LOG_WARNING("in-memory 2d texture \"%s\" has no texture data.", get_path().c_str());

            assert(m_image.get() != nullptr || m_dummy_texture.get() != nullptr);

            return true;
        }

        const CanvasProperties& properties() override
        {
            return
                m_image.get()
                    ? m_image->properties()
                    : m_dummy_texture->m_props;
        }

        Source* create_source(
            const UniqueID          assembly_uid,
            const TextureInstance&  texture_instance) override
        {
            return new TextureSource(assembly_uid, texture_instance);
        }

        TilePtr load_tile(
            const size_t            tile_x,
            const size_t            tile_y) override
        {
            Tile* tile =
                m_image.get()
                    ? &m_image->tile(tile_x, tile_y)
                    : &m_dummy_texture->m_tile;
            return TilePtr::make_non_owning(tile);  // the tile is owned by `m_image`
        }

      private:
        struct DummyTexture
        {
            CanvasProperties    m_props;
            Tile                m_tile;

            DummyTexture()
              : m_props(1, 1, 1, 1, 4, PixelFormatFloat)
              , m_tile(1, 1, 4, PixelFormatFloat)
            {
                m_tile.set_pixel(0, Color4f(0.0f));
            }
        };

        std::unique_ptr<DummyTexture>    m_dummy_texture;
        auto_release_ptr<Image>          m_image;
        ColorSpace                       m_color_space;

        void extract_parameters()
        {
            const EntityDefMessageContext context("texture", this);

            // Retrieve the color space.
            const std::string color_space =
                m_params.get_required<std::string>(
                    "color_space",
                    "linear_rgb",
                    make_vector("linear_rgb", "srgb", "ciexyz"),
                    context);
            if (color_space == "linear_rgb")
                m_color_space = ColorSpaceLinearRGB;
            else if (color_space == "srgb")
                m_color_space = ColorSpaceSRGB;
            else m_color_space = ColorSpaceCIEXYZ;
        }
    };
}


//
// MemoryTexture2dFactory class implementation.
//

void MemoryTexture2dFactory::release()
{
    delete this;
}

const char* MemoryTexture2dFactory::get_model() const
{
    return Model;
}

Dictionary MemoryTexture2dFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "2D In-Memory Texture");
}

DictionaryArray MemoryTexture2dFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "color_space")
            .insert("label", "Color Space")
            .insert("type", "enumeration")
            .insert("items",
                Dictionary()
                    .insert("Linear RGB", "linear_rgb")
                    .insert("sRGB", "srgb")
                    .insert("CIE XYZ", "ciexyz"))
            .insert("use", "required")
            .insert("default", "srgb"));

    return metadata;
}

auto_release_ptr<Texture> MemoryTexture2dFactory::create(
    const char*             name,
    const ParamArray&       params,
    const SearchPaths&      search_paths) const
{
    return auto_release_ptr<Texture>(new MemoryTexture2d(name, params));
}

auto_release_ptr<Texture> MemoryTexture2dFactory::create(
    const char*             name,
    const ParamArray&       params,
    auto_release_ptr<Image> image) const
{
    return auto_release_ptr<Texture>(new MemoryTexture2d(name, params, image));
}

}   // namespace renderer
