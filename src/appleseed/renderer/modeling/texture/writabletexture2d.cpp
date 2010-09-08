
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
#include "writabletexture2d.h"

// appleseed.renderer headers.
#include "renderer/modeling/texture/texture.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/colorspace.h"
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
    // 2D writable texture.
    //

    class WritableTexture2d
      : public Texture
    {
      public:
        WritableTexture2d(
            const char*         name,
            const ParamArray&   params,
            const SearchPaths&  search_paths)
          : Texture(params)
          , m_name(name)
          , m_props_defined(false)
        {
        }

        virtual void release()
        {
            delete this;
        }

        virtual const char* get_model() const
        {
            return WritableTexture2dFactory::get_model();
        }

        virtual const char* get_name() const
        {
            return m_name.c_str();
        }

        virtual ColorSpace get_color_space() const
        {
            // todo: implement.
            return ColorSpaceLinearRGB;
        }

        virtual const CanvasProperties& properties()
        {
            mutex::scoped_lock lock(m_mutex);

            if (!m_props_defined)
            {
                set_canvas_properties();
                m_props_defined = true;
            }

            return m_props;
        }

        virtual Tile* load_tile(
            const size_t    tile_x,
            const size_t    tile_y)
        {
            mutex::scoped_lock lock(m_mutex);

            // todo: create a blank tile, or get the tile from the texture.
            return new Tile(
                32,
                32,
                4,
                PixelFormatFloat);
        }

        virtual void unload_tile(
            const size_t    tile_x,
            const size_t    tile_y,
            Tile*           tile)
        {
            mutex::scoped_lock lock(m_mutex);

            // todo: store the tile into the texture.
        }

      private:
        const string        m_name;
        mutable mutex       m_mutex;
        bool                m_props_defined;
        CanvasProperties    m_props;

        // Set canvas properties.
        void set_canvas_properties()
        {
            // todo: open texture file and set canvas properties.
        }
    };

}   // anonymous namespace


//
// WritableTexture2dFactory class implementation.
//

const char* WritableTexture2dFactory::get_model()
{
    return "writable_texture_2d";
}

auto_release_ptr<Texture> WritableTexture2dFactory::create(
    const char*         name,
    const ParamArray&   params,
    const SearchPaths&  search_paths) const
{
    return
        auto_release_ptr<Texture>(
            new WritableTexture2d(name, params, search_paths));
}

}   // namespace renderer
