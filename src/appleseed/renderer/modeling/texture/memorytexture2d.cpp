
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016 Francois Beaune, The appleseedhq Organization
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
#include "renderer/modeling/texture/texture.h"
#include "renderer/utility/messagecontext.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"
#include "foundation/image/pixel.h"
#include "foundation/image/tile.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/makevector.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <memory>
#include <string>

using namespace foundation;
using namespace std;

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

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const APPLESEED_OVERRIDE
        {
            return Model;
        }

        virtual ColorSpace get_color_space() const APPLESEED_OVERRIDE
        {
            return m_color_space;
        }

        virtual bool on_frame_begin(
            const Project&          project,
            const BaseGroup*        parent,
            OnFrameBeginRecorder&   recorder,
            IAbortSwitch*           abort_switch) APPLESEED_OVERRIDE
        {
            if (!Texture::on_frame_begin(project, parent, recorder, abort_switch))
                return false;

            assert(m_image.get() != 0 || m_dummy_texture.get() != 0);

            if (m_image.get() == 0)
                RENDERER_LOG_WARNING("in-memory 2d texture \"%s\" has no texture data.", get_path().c_str());

            return true;
        }

        virtual const CanvasProperties& properties() APPLESEED_OVERRIDE
        {
            return
                m_image.get()
                    ? m_image->properties()
                    : m_dummy_texture->m_props;
        }

        virtual Tile* load_tile(
            const size_t            tile_x,
            const size_t            tile_y) APPLESEED_OVERRIDE
        {
            return
                m_image.get()
                    ? &m_image->tile(tile_x, tile_y)
                    : &m_dummy_texture->m_tile;
        }

        virtual void unload_tile(
            const size_t            tile_x,
            const size_t            tile_y,
            const Tile*             tile) APPLESEED_OVERRIDE
        {
            // Nothing to do, the tile is owned by the source image.
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

        auto_ptr<DummyTexture>  m_dummy_texture;
        auto_release_ptr<Image> m_image;
        ColorSpace              m_color_space;

        void extract_parameters()
        {
            const EntityDefMessageContext message_context("texture", this);

            // Retrieve the color space.
            const string color_space =
                m_params.get_required<string>(
                    "color_space",
                    "linear_rgb",
                    make_vector("linear_rgb", "srgb", "ciexyz"),
                    message_context);
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

auto_release_ptr<Texture> MemoryTexture2dFactory::static_create(
    const char*             name,
    const ParamArray&       params,
    auto_release_ptr<Image> image)
{
    return auto_release_ptr<Texture>(new MemoryTexture2d(name, params, image));
}

}   // namespace renderer
