
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "disktexture.h"

// appleseed.renderer headers.
#include "renderer/modeling/texture/texture.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/colorspace.h"
#include "foundation/image/genericprogressiveimagefilereader.h"
#include "foundation/image/pixel.h"
#include "foundation/image/tile.h"
#include "foundation/platform/thread.h"
#include "foundation/utility/searchpaths.h"

using namespace boost;
using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{

    //
    // Disk texture.
    //

    class DiskTexture
      : public Texture
    {
      public:
        DiskTexture(
            const char*         name,
            const ParamArray&   params,
            const SearchPaths&  search_paths)
          : Texture(params)
          , m_name(name)
          , m_reader(&global_logger())
        {
            extract_parameters(search_paths);
        }

        virtual void release()
        {
            delete this;
        }

        virtual const char* get_model() const
        {
            return DiskTextureFactory::get_model();
        }

        virtual const char* get_name() const
        {
            return m_name.c_str();
        }

        virtual ColorSpace get_color_space() const
        {
            return m_color_space;
        }

        virtual const CanvasProperties& properties()
        {
            mutex::scoped_lock lock(m_mutex);
            open_image_file();
            return m_props;
        }

        virtual Tile* load_tile(
            const size_t    tile_x,
            const size_t    tile_y)
        {
            mutex::scoped_lock lock(m_mutex);
            open_image_file();
            return m_reader.read_tile(tile_x, tile_y);
        }

        virtual void unload_tile(
            const size_t    tile_x,
            const size_t    tile_y,
            Tile*           tile)
        {
            delete tile;
        }

      private:
        const string                        m_name;
        string                              m_filename;
        ColorSpace                          m_color_space;

        mutable mutex                       m_mutex;
        GenericProgressiveImageFileReader   m_reader;
        CanvasProperties                    m_props;

        void extract_parameters(const SearchPaths& search_paths)
        {
            // Retrieve the texture filename.
            m_filename = search_paths.qualify(m_params.get_required<string>("filename", ""));

            // Retrieve the color space.
            const string color_space = m_params.get_required<string>("color_space", "linear_rgb");
            if (color_space == "linear_rgb")
                m_color_space = ColorSpaceLinearRGB;
            else if (color_space == "srgb")
                m_color_space = ColorSpaceSRGB;
            else if (color_space == "ciexyz")
                m_color_space = ColorSpaceCIEXYZ;
            else
            {
                RENDERER_LOG_ERROR(
                    "invalid value \"%s\" for parameter \"color_space\", "
                    "using default value \"linear_rgb\"",
                    color_space.c_str());
                m_color_space = ColorSpaceLinearRGB;
            }
        }

        // Open the image file.
        void open_image_file()
        {
            if (!m_reader.is_open())
            {
                RENDERER_LOG_INFO(
                    "opening texture file %s for reading...",
                    m_filename.c_str());

                m_reader.open(m_filename.c_str());
                m_reader.read_canvas_properties(m_props);
            }
        }
    };

}   // anonymous namespace


//
// DiskTextureFactory class implementation.
//

const char* DiskTextureFactory::get_model()
{
    return "2d_texture";
}

auto_release_ptr<Texture> DiskTextureFactory::create(
    const char*         name,
    const ParamArray&   params,
    const SearchPaths&  search_paths) const
{
    return
        auto_release_ptr<Texture>(
            new DiskTexture(name, params, search_paths));
}

}   // namespace renderer
