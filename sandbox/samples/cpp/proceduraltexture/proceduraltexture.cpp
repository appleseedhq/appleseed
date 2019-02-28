
//
// This sample is a simplified version of the osl.imageio sample OIIO plugin.
// Original license follows.
//

/*
Copyright (c) 2009-2015 Sony Pictures Imageworks Inc., et al.
All Rights Reserved.
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:
* Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer in the
  documentation and/or other materials provided with the distribution.
* Neither the name of Sony Pictures Imageworks nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include "OpenImageIO/dassert.h"
#include "OpenImageIO/filesystem.h"
#include "OpenImageIO/imagebufalgo.h"
#include "OpenImageIO/imageio.h"
#include "OpenImageIO/strutil.h"
#include "OpenImageIO/thread.h"
#include "OpenImageIO/typedesc.h"

#include <algorithm>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <string>
#include <utility>
#include <vector>

using namespace OIIO;

//
// ProceduralInput is an ImageInput that behaves as if it's reading an image,
// but actually it is executing C++ code to generate pixel values.
//
// The filename is a "URI" form: name?opt1=val&opt2=val2...
//
// Special options in the options list include:
//    RES=%dx%d        Set the resolution of the image (default: 1024x1024)
//    TILE=%dx%d       Set the tile size
//

//
// ProceduralInput class.
//

class ProceduralInput : public ImageInput
{
  public:
    ProceduralInput();
    ~ProceduralInput() override;

    const char* format_name() const override;

    int supports(string_view feature) const override;

    bool valid_file(const std::string& filename) const override;

    bool open(const std::string& name, ImageSpec& newspec) override;
    bool open(const std::string& name, ImageSpec& newspec, const ImageSpec& config) override;

    bool close() override;

    int current_subimage() const override;
    int current_miplevel() const override;

#if OIIO_PLUGIN_VERSION < 21 /* OIIO < 1.9 */
    bool seek_subimage(int subimage, int miplevel, ImageSpec& newspec) override;

    bool read_native_scanline(int y, int z, void* data) override;
    bool read_native_scanlines(int ybegin, int yend, int z, void* data) override;

    bool read_native_tile(int x, int y, int z, void* data) override;
    bool read_native_tiles(
        int   xbegin,
        int   xend,
        int   ybegin,
        int   yend,
        int   zbegin,
        int   zend,
        void* data) override;
#else
    bool seek_subimage(int subimage, int miplevel) override;

    bool read_native_scanline(int subimage, int miplevel, int y, int z, void* data) override;
    bool read_native_scanlines(
        int   subimage,
        int   miplevel,
        int   ybegin,
        int   yend,
        int   z,
        void* data) override;

    bool read_native_tile(int subimage, int miplevel, int x, int y, int z, void* data) override;
    bool read_native_tiles(
        int   subimage,
        int   miplevel,
        int   xbegin,
        int   xend,
        int   ybegin,
        int   yend,
        int   zbegin,
        int   zend,
        void* data) override;
#endif

  private:
    std::string m_filename;

    bool generate_pixels(
        const int         checks_size,
        const ImageSpec&  spec,
        const ROI&        roi,
        ImageBuf&         pixels) const;
};

namespace
{
    /// Deconstruct a "URI" string into the "filename" part (returned) and turn
    /// the "query" part into a series of pairs of id and value. For example,
    ///     deconstruct_uri("foo.tif?bar=1&blah=\"hello world\"", args)
    /// would be expected to return "foo.tif" and *args would contain two
    /// pairs: ("foo","1") and ("bar","\"hello world\"").
    string_view deconstruct_uri(
        string_view                                       uri,
        std::vector<std::pair<string_view, string_view>>* args = nullptr)
    {
        if (args)
            args->clear();
        size_t arg_start = uri.find('?');
        if (arg_start == string_view::npos)
            return uri;
        string_view argstring = uri.substr(arg_start + 1);
        string_view filename = uri.substr(0, arg_start);
        if (!args)
            return filename;
        while (!argstring.empty())
        {
            string_view id = Strutil::parse_until(argstring, "=&");
            string_view value;
            if (!id.size())
                break;
            if (!Strutil::parse_char(argstring, '=') || argstring.empty())
                break;
            if (argstring[0] == '\"')
                Strutil::parse_string(argstring, value, true, Strutil::KeepQuotes);
            else
                value = Strutil::parse_until(argstring, "&\t\r\n");
            args->push_back(std::make_pair(id, value));
            Strutil::parse_char(argstring, '&');
        }
        return filename;
    }

    void parse_res(string_view res, int& x, int& y, int& z)
    {
        if (Strutil::parse_int(res, x))
        {
            if (Strutil::parse_char(res, 'x') && Strutil::parse_int(res, y))
            {
                if (!(Strutil::parse_char(res, 'x') && Strutil::parse_int(res, z)))
                    z = 1;
            }
            else
            {
                y = x;
                z = 1;
            }
        }
    }
}


//
// ProceduralInput class implementation.
//

ProceduralInput::ProceduralInput()
{
}

ProceduralInput::~ProceduralInput()
{
    close();
}

const char* ProceduralInput::format_name() const
{
    return "ProceduralTxSample";
}

int ProceduralInput::supports(string_view feature) const
{
    return feature == "procedural";
}

bool ProceduralInput::valid_file(const std::string& filename) const
{
    string_view name = deconstruct_uri(filename);
    return Strutil::ends_with(name, ".prtx");
}

bool ProceduralInput::open(const std::string& name, ImageSpec& newspec)
{
    ImageSpec config;
    return open(name, newspec, config);
}

bool ProceduralInput::open(
    const std::string& name,
    ImageSpec&         newspec,
    const ImageSpec&   config)
{
    std::vector<std::pair<string_view, string_view>> args;
    string_view basename = deconstruct_uri(name, &args);

    if (basename.empty())
        return false;

    if (!Strutil::ends_with(basename, ".prtx"))
        return false;

    m_filename = name;
    m_spec = ImageSpec(1024, 1024, 4, TypeDesc::FLOAT);

    for (size_t i = 0; i < args.size(); ++i)
    {
        if (args[i].first == "RES")
        {
            parse_res(args[i].second, m_spec.width, m_spec.height, m_spec.depth);
        }
        else if (args[i].first == "TILE" || args[i].first == "TILES")
        {
            parse_res(
                args[i].second,
                m_spec.tile_width,
                m_spec.tile_height,
                m_spec.tile_depth);
        }
    }

    m_spec.full_x = m_spec.x;
    m_spec.full_y = m_spec.y;
    m_spec.full_z = m_spec.z;
    m_spec.full_width = m_spec.width;
    m_spec.full_height = m_spec.height;
    m_spec.full_depth = m_spec.depth;

#if OIIO_PLUGIN_VERSION < 21
    return seek_subimage(0, 0, newspec);
#else
    if (seek_subimage(0, 0))
    {
        newspec = spec();
        return true;
    }
    else
    {
        close();
        return false;
    }
#endif
}

bool ProceduralInput::close()
{
    return true;
}

int ProceduralInput::current_subimage() const
{
    return 0;
}

int ProceduralInput::current_miplevel() const
{
    return 0;
}

bool ProceduralInput::seek_subimage(
    int subimage,
    int miplevel
#if OIIO_PLUGIN_VERSION < 21
    ,
    ImageSpec& newspec
#endif
)
{
    if (subimage == current_subimage() && miplevel == current_miplevel())
    {
#if OIIO_PLUGIN_VERSION < 21
        newspec = spec();
#endif
        return true;
    }

    if (subimage != 0)
        return false;  // We only make one subimage

    if (miplevel > 0)
        return false;  // We don't support Mipmaps

#if OIIO_PLUGIN_VERSION < 21
    newspec = spec();
#endif
    return true;
}

bool ProceduralInput::read_native_scanlines(
#if OIIO_PLUGIN_VERSION >= 21
    int subimage,
    int miplevel,
#endif
    int   ybegin,
    int   yend,
    int   z,
    void* data)
{
#if OIIO_PLUGIN_VERSION >= 21
    lock_guard lock(m_mutex);
    if (!seek_subimage(subimage, miplevel))
        return false;
#endif

    // Make a spec and a roi that describes just this scanline
    ImageSpec spec = m_spec;
    spec.y = ybegin;
    spec.z = z;
    spec.height = yend - ybegin;
    spec.depth = 1;

    ROI roi(
        spec.x,
        spec.x + spec.width,
        spec.y,
        spec.y + spec.height,
        spec.z,
        spec.z + spec.depth);

    // Create an ImageBuf wrapper of the user's data
    ImageBuf ibwrapper(spec, data);

    // Now generate a checkerboard on the ImageBuf pixels, which really point to
    // the caller's data buffer.
    return generate_pixels(64, spec, roi, ibwrapper);
}

bool ProceduralInput::read_native_scanline(
#if OIIO_PLUGIN_VERSION >= 21
    int subimage,
    int miplevel,
#endif
    int   y,
    int   z,
    void* data)
{
#if OIIO_PLUGIN_VERSION >= 21
    return read_native_scanlines(subimage, miplevel, y, y + 1, z, data);
#else
    return read_native_scanlines(y, y + 1, z, data);
#endif
}

bool ProceduralInput::read_native_tiles(
#if OIIO_PLUGIN_VERSION >= 21
    int subimage,
    int miplevel,
#endif
    int   xbegin,
    int   xend,
    int   ybegin,
    int   yend,
    int   zbegin,
    int   zend,
    void* data)
{
#if OIIO_PLUGIN_VERSION >= 21
    lock_guard lock(m_mutex);
    if (!seek_subimage(subimage, miplevel))
        return false;
#endif

    // Make a spec and a roi that describes just this tile
    ImageSpec spec = m_spec;
    spec.x = xbegin;
    spec.y = ybegin;
    spec.z = zbegin;
    spec.width = xend - xbegin;
    spec.height = yend - ybegin;
    spec.depth = zend - zbegin;

    ROI roi(
        spec.x,
        spec.x + spec.width,
        spec.y,
        spec.y + spec.height,
        spec.z,
        spec.z + spec.depth);

    // Create an ImageBuf wrapper of the user's data
    ImageBuf ibwrapper(spec, data);

    // Now generate a checkerboard on the ImageBuf pixels, which really point to
    // the caller's data buffer.
    return generate_pixels(64, spec, roi, ibwrapper);
}

bool ProceduralInput::read_native_tile(
#if OIIO_PLUGIN_VERSION >= 21
    int subimage,
    int miplevel,
#endif
    int   x,
    int   y,
    int   z,
    void* data)
{
#if OIIO_PLUGIN_VERSION >= 21
    lock_guard lock(m_mutex);
    if (!seek_subimage(subimage, miplevel))
        return false;
#endif

    return read_native_tiles(
#if OIIO_PLUGIN_VERSION >= 21
        subimage,
        miplevel,
#endif
        x,
        std::min(x + m_spec.tile_width, m_spec.x + m_spec.width),
        y,
        std::min(y + m_spec.tile_height, m_spec.y + m_spec.height),
        z,
        std::min(z + m_spec.tile_depth, m_spec.z + m_spec.depth),
        data);
}

bool ProceduralInput::generate_pixels(
    const int         checks_size,
    const ImageSpec&  spec,
    const ROI&        roi,
    ImageBuf&         pixels) const
{
    const float black[] = {0.0f, 0.0f, 0.0f, 1.0f};
    const float white[] = {1.0f, 1.0f, 1.0f, 1.0f};

    return ImageBufAlgo::checker(
        pixels,
        checks_size,
        checks_size,
        checks_size,
        black,
        white,
        0,
        0,
        0,
        roi);
}


//
// OpenImageIO plugin registration.
//

OIIO_PLUGIN_EXPORTS_BEGIN

OIIO_EXPORT int proceduraltexture_imageio_version = OIIO_PLUGIN_VERSION;

OIIO_EXPORT const char* proceduraltexture_input_extensions[] = { "prtx", nullptr };

OIIO_EXPORT ImageInput* proceduraltexture_input_imageio_create()
{
    return new ProceduralInput;
}

OIIO_EXPORT void proceduraltexture_input_imageio_delete(ImageInput* p)
{
    delete p;
}

OIIO_PLUGIN_EXPORTS_END
