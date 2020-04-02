
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
#include "disktexture2d.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/input/texturesource.h"
#include "renderer/modeling/texture/texture.h"
#include "renderer/modeling/texture/tileptr.h"
#include "renderer/utility/messagecontext.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/colorspace.h"
#include "foundation/image/genericprogressiveimagefilereader.h"
#include "foundation/image/tile.h"
#include "foundation/platform/thread.h"
#include "foundation/string/string.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/makevector.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/uid.h"

// Standard headers.
#include <cstddef>
#include <string>

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // 2D on-disk texture.
    //

    const char* Model = "disk_texture_2d";

    class DiskTexture2d
      : public Texture
    {
      public:
        DiskTexture2d(
            const char*             name,
            const ParamArray&       params,
            const SearchPaths&      search_paths)
          : Texture(name, params)
          , m_reader(&global_logger())
        {
            const EntityDefMessageContext context("texture", this);

            // Establish and store the qualified path to the texture file.
            m_filepath = to_string(search_paths.qualify(m_params.get_required<std::string>("filename", "")));

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

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return Model;
        }

        void on_render_end(
            const Project&          project,
            const BaseGroup*        parent) override
        {
            if (m_reader.is_open())
            {
                RENDERER_LOG_INFO("closing texture file %s...", m_filepath.c_str());
                m_reader.close();
            }

            Texture::on_render_end(project, parent);
        }

        ColorSpace get_color_space() const override
        {
            return m_color_space;
        }

        void collect_asset_paths(StringArray& paths) const override
        {
            if (m_params.strings().exist("filename"))
            {
                const char* filename = m_params.get("filename");
                if (!is_empty_string(filename))
                    paths.push_back(filename);
            }
        }

        void update_asset_paths(const StringDictionary& mappings) override
        {
            m_params.set("filename", mappings.get(m_params.get("filename")));
        }

        const CanvasProperties& properties() override
        {
            boost::mutex::scoped_lock lock(m_mutex);
            open_image_file();
            return m_props;
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
            boost::mutex::scoped_lock lock(m_mutex);
            open_image_file();
            return TilePtr::make_owning(m_reader.read_tile(tile_x, tile_y));
        }

      private:
        std::string                         m_filepath;
        ColorSpace                          m_color_space;

        mutable boost::mutex                m_mutex;
        GenericProgressiveImageFileReader   m_reader;
        CanvasProperties                    m_props;

        void open_image_file()
        {
            if (!m_reader.is_open())
            {
                RENDERER_LOG_INFO(
                    "opening texture file %s and reading metadata...",
                    m_filepath.c_str());

                m_reader.open(m_filepath.c_str());
                m_reader.read_canvas_properties(m_props);
            }
        }
    };
}


//
// DiskTexture2dFactory class implementation.
//

void DiskTexture2dFactory::release()
{
    delete this;
}

const char* DiskTexture2dFactory::get_model() const
{
    return Model;
}

Dictionary DiskTexture2dFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "2D Texture File")
            .insert("default_model", "true");
}

DictionaryArray DiskTexture2dFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "filename")
            .insert("label", "File Path")
            .insert("type", "file")
            .insert("file_picker_mode", "open")
            .insert("file_picker_type", "image")
            .insert("use", "required"));

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

auto_release_ptr<Texture> DiskTexture2dFactory::create(
    const char*         name,
    const ParamArray&   params,
    const SearchPaths&  search_paths) const
{
    return auto_release_ptr<Texture>(new DiskTexture2d(name, params, search_paths));
}

}   // namespace renderer
