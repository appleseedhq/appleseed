
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Esteban Tovagliari, The appleseedhq Organization
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

// appleseed.python headers.
#include "gillocks.h"

// appleseed.renderer headers.
#include "renderer/api/frame.h"
#include "renderer/api/rendering.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/platform/opengl.h"
#include "foundation/platform/python.h"
#include "foundation/utility/autoreleaseptr.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <vector>

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

// Some GL headers do not define GL_RGBA32F.
// Instead of adding an OpenGL extension library like glew
// as a dependency, manually define it if needed.
#ifndef GL_RGBA32F
    #define GL_RGBA32F  0x8814
#endif

namespace
{
    class BlenderProgressiveTileCallback
      : public ITileCallback
    {
      public:
        explicit BlenderProgressiveTileCallback(const bpy::object& request_redraw_callback)
          : m_buffer_width(0)
          , m_buffer_height(0)
          , m_updated_buffer(false)
          , m_texture_width(0)
          , m_texture_height(0)
          , m_request_redraw_callback(request_redraw_callback)
        {
        }

        ~BlenderProgressiveTileCallback() override
        {
            delete_texture();
        }

        void release() override
        {
            delete this;
        }

        void on_tiled_frame_begin(const Frame*) override
        {
            PyErr_SetString(PyExc_RuntimeError, "BlenderProgressiveTileCallback cannot be used for final renders");
            bpy::throw_error_already_set();
        }

        void on_tiled_frame_end(const Frame*) override
        {
            PyErr_SetString(PyExc_RuntimeError, "BlenderProgressiveTileCallback cannot be used for final renders");
            bpy::throw_error_already_set();
        }

        void on_tile_begin(const Frame*, const size_t, const size_t) override
        {
            PyErr_SetString(PyExc_RuntimeError, "BlenderProgressiveTileCallback cannot be used for final renders");
            bpy::throw_error_already_set();
        }

        void on_tile_end(const Frame*, const size_t, const size_t) override
        {
            PyErr_SetString(PyExc_RuntimeError, "BlenderProgressiveTileCallback cannot be used for final renders");
            bpy::throw_error_already_set();
        }

        void on_progressive_frame_update(const Frame* frame) override
        {
            Image& image = frame->image();

            // Realloc the buffer if the image size changed since the last time.
            const CanvasProperties& props = image.properties();

            if (props.m_canvas_width != m_buffer_width || props.m_canvas_height != m_buffer_height)
            {
                m_buffer_width = props.m_canvas_width;
                m_buffer_height = props.m_canvas_height;
                m_buffer.resize(m_buffer_width * m_buffer_height * 4);
            }

            // Copy the pixels to the buffer.
            for (size_t ty = 0; ty < props.m_tile_count_y; ++ty)
            {
                for (size_t tx = 0; tx < props.m_tile_count_x; ++tx)
                    copy_tile(image.tile(tx, ty), props, tx, ty);
            }

            m_updated_buffer = true;

            // Call the request redraw Python callback.
            if (m_request_redraw_callback)
            {
                ScopedGILLock lock;

                try
                {
                    m_request_redraw_callback();
                }
                catch (...)
                {
                    // Don't let Python exceptions propagate into C++.
                }
            }
        }

        void draw_pixels(const float x, const float y, const float w, const float h)
        {
            if (m_texture_width != m_buffer_width || m_texture_height != m_buffer_height)
                delete_texture();

            if (m_texture_id == 0)
                allocate_texture();

            if (m_texture_id != 0)
            {
                if (m_updated_buffer)
                {
                    glBindTexture(GL_TEXTURE_2D, m_texture_id);
                    glTexImage2D(
                        GL_TEXTURE_2D,
                        0,
                        GL_RGBA32F,
                        static_cast<GLsizei>(m_texture_width),
                        static_cast<GLsizei>(m_texture_height),
                        0,
                        GL_RGBA,
                        GL_FLOAT,
                        m_buffer.data());
                    glBindTexture(GL_TEXTURE_2D, 0);
                    m_updated_buffer = false;
                }

                glColor4f(1.0f, 1.0f, 1.0f, 1.0f);

                glEnable(GL_TEXTURE_2D);
                glBindTexture(GL_TEXTURE_2D, m_texture_id);
                glBegin(GL_QUADS);
                    glTexCoord2f(0.0f, 1.0f); glVertex3f(x    ,  y    , 0.0f);
                    glTexCoord2f(1.0f, 1.0f); glVertex3f(x + w,  y    , 0.0f);
                    glTexCoord2f(1.0f, 0.0f); glVertex3f(x + w,  y + h, 0.0f);
                    glTexCoord2f(0.0f, 0.0f); glVertex3f(x    ,  y + h, 0.0f);
                glEnd();
                glBindTexture(GL_TEXTURE_2D, 0);
                glDisable(GL_TEXTURE_2D);
            }
        }

      private:
        std::vector<float>  m_buffer;
        size_t              m_buffer_width;
        size_t              m_buffer_height;
        bool                m_updated_buffer;

        GLuint              m_texture_id;
        size_t              m_texture_width;
        size_t              m_texture_height;

        bpy::object         m_request_redraw_callback;

        void delete_texture()
        {
            if (m_texture_id != 0)
            {
                glDeleteTextures(1, &m_texture_id);

                m_texture_id = 0;
                m_texture_width = 0;
                m_texture_height = 0;
            }
        }

        void allocate_texture()
        {
            assert(m_texture_id == 0);

            // We didn't receive any pixel yet.
            if (m_buffer_width == 0 || m_buffer_height == 0)
                return;

            glGenTextures(1, &m_texture_id);

            m_texture_width = m_buffer_width;
            m_texture_height = m_buffer_height;

            glBindTexture(GL_TEXTURE_2D, m_texture_id);
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
            glTexImage2D(
                GL_TEXTURE_2D,
                0,
                GL_RGBA32F,
                static_cast<GLsizei>(m_texture_width),
                static_cast<GLsizei>(m_texture_height),
                0,
                GL_RGBA,
                GL_FLOAT,
                nullptr);
            glBindTexture(GL_TEXTURE_2D, 0);
        }

        void copy_tile(const Tile& tile, const CanvasProperties& props, const size_t tile_x, const size_t tile_y)
        {
            const size_t x0 = tile_x * props.m_tile_width;
            const size_t y0 = tile_y * props.m_tile_height;

            for (size_t y = 0, ye = tile.get_height(); y < ye; ++y)
            {
                const size_t offset = (((y0 + y) * props.m_canvas_width) + x0) * 4;

                float* p = m_buffer.data() + offset;

                for (size_t x = 0, xe = tile.get_width(); x < xe; ++x)
                {
                    Color4f c;
                    tile.get_pixel(x, y, c);

                    *p++ = c.r;
                    *p++ = c.g;
                    *p++ = c.b;
                    *p++ = c.a;
                }
            }
        }
    };

    auto_release_ptr<BlenderProgressiveTileCallback> create_blender_progressive_tile_callback(const bpy::object& request_redraw_callback)
    {
        return auto_release_ptr<BlenderProgressiveTileCallback>(new BlenderProgressiveTileCallback(request_redraw_callback));
    }
}

// Work around a regression in Visual Studio 2015 Update 3.
#if defined(_MSC_VER) && _MSC_VER == 1900
namespace boost
{
    template <> BlenderProgressiveTileCallback const volatile* get_pointer<BlenderProgressiveTileCallback const volatile>(BlenderProgressiveTileCallback const volatile* p) { return p; }
}
#endif

void bind_blender_progressive_tile_callback()
{
    bpy::class_<BlenderProgressiveTileCallback, auto_release_ptr<BlenderProgressiveTileCallback>, bpy::bases<ITileCallback>, boost::noncopyable>("BlenderProgressiveTileCallback", bpy::no_init)
        .def("__init__", bpy::make_constructor(create_blender_progressive_tile_callback))
        .def("draw_pixels", &BlenderProgressiveTileCallback::draw_pixels);
}
