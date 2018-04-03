
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
#include "exrimagefilewriter.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/exceptionunsupportedimageformat.h"
#include "foundation/image/exrutils.h"
#include "foundation/image/icanvas.h"
#include "foundation/image/pixel.h"
#include "foundation/image/tile.h"
#include "foundation/utility/foreach.h"

// OpenEXR headers.
#include "foundation/platform/_beginexrheaders.h"
#include "OpenEXR/IexBaseExc.h"
#include "OpenEXR/ImathBox.h"
#include "OpenEXR/ImathVec.h"
#include "OpenEXR/ImfChannelList.h"
#include "OpenEXR/ImfFrameBuffer.h"
#include "OpenEXR/ImfHeader.h"
#include "OpenEXR/ImfMultiPartOutputFile.h"
#include "OpenEXR/ImfPartType.h"
#include "OpenEXR/ImfPixelType.h"
#include "OpenEXR/ImfTileDescription.h"
#include "OpenEXR/ImfTiledOutputPart.h"
#include "OpenEXR/ImfTiledOutputFile.h"
#include "foundation/platform/_endexrheaders.h"

// openimageio headers.
#include "foundation/platform/_beginoiioheaders.h"
#include "OpenImageIO/imageio.h"
#include "foundation/platform/_endoiioheaders.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <vector>

using namespace Iex;
using namespace Imath;
using namespace Imf;
using namespace std;

namespace foundation
{

//
// EXRImageFileWriter class implementation.
//

struct EXRImageFileWriter::Impl
{
    vector<const ICanvas*>  m_images;
    vector<Header>          m_image_headers;
    vector<size_t>          m_channel_counts;
    vector<const char**>    m_channel_names;
};

EXRImageFileWriter::EXRImageFileWriter()
  : impl(nullptr)
{
}

EXRImageFileWriter::~EXRImageFileWriter()
{
    delete impl;
}

void EXRImageFileWriter::write(
    const char*             filename,
    const ICanvas&          image,
    const ImageAttributes&  image_attributes)
{
    static const char* ChannelNames[] = {"R", "G", "B", "A"};
    const CanvasProperties& props = image.properties();
    write(filename, image, image_attributes, props.m_channel_count, ChannelNames);
}

namespace
{

PixelType get_imf_pixel_type(const CanvasProperties& props)
{
    switch (props.m_pixel_format)
    {
        case PixelFormatUInt32: return PixelType::UINT; break;
        case PixelFormatHalf: return PixelType::HALF; break;
        case PixelFormatFloat: return PixelType::FLOAT; break;
        default: throw ExceptionUnsupportedImageFormat();
    }
}

Header build_header(
    const CanvasProperties& props,
    const ImageAttributes&  image_attributes,
    const size_t            channel_count,
    const char**            channel_names,
    PixelType&              pixel_type)
{
    // Construct Header object.
    Header header(
        static_cast<int>(props.m_canvas_width),
        static_cast<int>(props.m_canvas_height));

    // Construct TileDescription object.
    const TileDescription tile_desc(
        static_cast<unsigned int>(props.m_tile_width),
        static_cast<unsigned int>(props.m_tile_height),
        ONE_LEVEL);

    header.setTileDescription(tile_desc);

    // Construct ChannelList object.
    pixel_type = get_imf_pixel_type(props);
    ChannelList channels;
    for (size_t c = 0; c < channel_count; ++c)
        channels.insert(channel_names[c], Channel(pixel_type));

    header.channels() = channels;

    // Add image attributes to the Header object.
    add_attributes(image_attributes, header);

    return header;
}

template <typename TiledOutputFileOrPart>
void write_tiles(
    TiledOutputFileOrPart&  file,
    const ICanvas&          image,
    const size_t            channel_count,
    const char**            channel_names)
{
    const CanvasProperties& props = image.properties();
    const PixelType pixel_type = get_imf_pixel_type(props);

    // Write tiles.
    for (size_t y = 0; y < props.m_tile_count_y; ++y)
    {
        for (size_t x = 0; x < props.m_tile_count_x; ++x)
        {
            const int ix              = static_cast<int>(x);
            const int iy              = static_cast<int>(y);
            const Box2i range         = file.dataWindowForTile(ix, iy);
            const Tile& tile          = image.tile(x, y);
            const size_t channel_size = Pixel::size(tile.get_pixel_format());
            const size_t stride_x     = channel_size * props.m_channel_count;
            const size_t stride_y     = stride_x * tile.get_width();
            const size_t tile_origin  = range.min.x * stride_x + range.min.y * stride_y;
            const char* tile_base     = reinterpret_cast<const char*>(tile.pixel(0, 0)) - tile_origin;

            // Construct FrameBuffer object.
            FrameBuffer framebuffer;
            for (size_t c = 0; c < channel_count; ++c)
            {
                const char* base = tile_base + c * channel_size;
                framebuffer.insert(
                    channel_names[c],
                    Slice(
                        pixel_type,
                        const_cast<char*>(base),
                        stride_x,
                        stride_y));
            }

            // Write tile.
            file.setFrameBuffer(framebuffer);
            file.writeTile(ix, iy);
        }
    }
}

}

static void exr_set_image_desc(OIIO::ImageSpec& spec, const CanvasProperties& props, const size_t channel_count, const char** channel_names);
static void exr_set_image_attributes(OIIO::ImageSpec& spec, const ImageAttributes& image_attributes);
static OIIO::TypeDesc exr_convert_pixel_format(PixelFormat format);
static void write_tiles(const ICanvas& image, OIIO::ImageOutput* out);

void EXRImageFileWriter::write(
    const char*             filename,
    const ICanvas&          image,
    const ImageAttributes&  image_attributes,
    const size_t            channel_count,
    const char**            channel_names)
{
    // todo: lift this limitation.
    assert(channel_count <= 4);

    assert(channel_names);

    initialize_openexr();

    try
    {
        // Retrieve canvas properties.
        const CanvasProperties& props = image.properties();

        // Construct the header object.
        PixelType pixel_type;
        Header header = build_header(props, image_attributes, channel_count, channel_names, pixel_type);

        // Create the output file.
        TiledOutputFile file(filename, header);
        //write_tiles(file, image, channel_count, channel_names);
    }
    catch (const BaseExc& e)
    {
        // I/O error.
        throw ExceptionIOError(e.what());
    }

    // ============================================================== TMP ===================================================================================

    // Creates an ImageOutput structure, which is the main class to write an image file in OpenImageIO.
    OIIO::ImageOutput* out = OIIO::ImageOutput::create(filename);
    if (out == nullptr)
    {
        const std::string msg = OpenImageIO::geterror();
        throw ExceptionIOError(msg.c_str());
    }

    // Create an ImageSpec, which describe the internal structure and attributes of the image file.
    OIIO::ImageSpec spec;
    exr_set_image_desc(spec, image.properties(), channel_count, channel_names);
    exr_set_image_attributes(spec, image_attributes);

    // Opens the image file at the specified file path and fills the image header with spec data.
    if (!out->open(filename, spec))
    {
        const std::string msg = out->geterror();
        OIIO::ImageOutput::destroy(out);
        throw ExceptionIOError(msg.c_str());
    }

    // Write tiles
    write_tiles(image, out);

    // Closes the image file.
    if (!out->close())
    {
        const std::string msg = out->geterror();
        OIIO::ImageOutput::destroy(out);
        throw ExceptionIOError(msg.c_str());
    }

    // Destroy the ImageOutput stucture.
    OIIO::ImageOutput::destroy(out);
}

OIIO::TypeDesc exr_convert_pixel_format(PixelFormat format)
{
    switch (format)
    {
        case PixelFormatUInt32: 
            return OIIO::TypeDesc::UINT32; 
            break;

        case PixelFormatHalf:
            return OIIO::TypeDesc::HALF; 
            break;

        case PixelFormatFloat: 
            return OIIO::TypeDesc::FLOAT; 
            break;

        default: 
            return OIIO::TypeDesc::HALF;
    }
}

void exr_set_image_desc(
    OIIO::ImageSpec&		spec, 
    const CanvasProperties& props, 
    const size_t		    channel_count,
    const char**		    channel_names)
{
    // Size of the data of the image.
    spec.width = static_cast<int>(props.m_canvas_width);
    spec.height = static_cast<int>(props.m_canvas_height);

    // Origin of the pixel data of the image.
    spec.x = 0;
    spec.y = 0;
    spec.z = 0;

    // Full size of the data of the image.
    spec.full_width = spec.width;
    spec.full_height = spec.height;

    // Origin of the pixel data of the full image.
    spec.full_x = spec.x;
    spec.full_y = spec.y;

    // Size of a tile.
    spec.tile_width = static_cast<int>(props.m_tile_width);
    spec.tile_height = static_cast<int>(props.m_tile_height);

    // Number of channels.
    spec.nchannels = static_cast<int>(channel_count);
    for (size_t i = 0; i < channel_count; i++)
    {
        const char* name = channel_names[i];

        spec.channelnames.push_back(name);

        if (name == "A")
            spec.alpha_channel = static_cast<int>(i);
    }

    // Format of the pixel data.
    spec.set_format(exr_convert_pixel_format(props.m_pixel_format));
}

// See OpenImageIO reference documentation for an exhaustive attribute names list.
// https://github.com/OpenImageIO/oiio/blob/master/src/doc/openimageio.pdf
//
void exr_set_image_attributes(
	OIIO::ImageSpec&	    spec, 
	const ImageAttributes&	image_attributes)
{
    for (const_each<ImageAttributes> i = image_attributes; i; ++i)
    {
        // Fetch the name and the value of the attribute.
        const std::string attr_name = i->key();
        const std::string attr_value = i->value<std::string>();

        if (attr_name == "author")
            spec.attribute("Copyright", attr_value);

        else if (attr_name == "comment")
            spec.attribute("ImageDescription", attr_value);

        else if (attr_name == "creation_time")
            spec.attribute("DateTime", attr_value);

        else
            spec.attribute(attr_name, attr_value);
    }
}

void write_tiles(
	const ICanvas&		image, 
	OIIO::ImageOutput*	out)
{
	assert(out);

	// Retrieves canvas properties
	const CanvasProperties& props = image.properties();

	// Loops over the columns of tiles
	for (size_t tile_y = 0; tile_y < props.m_tile_count_y; tile_y++)
	{
		// Loops over the rows of tiles
		for (size_t tile_x = 0; tile_x < props.m_tile_count_x; tile_x++)
		{
			// Computes the offset of the tile in pixels from the origin (origin: x=0;y=0).
			const size_t tile_offset_x = tile_x * props.m_tile_width;
			const size_t tile_offset_y = tile_y * props.m_tile_height;

			assert(tile_offset_x <= props.m_canvas_width);
			assert(tile_offset_y <= props.m_canvas_height);

			// Retrieves the (tile_x, tile_y) tile.
			const Tile& tile = image.tile(tile_x, tile_y);

			if (!out->write_tile(
                    static_cast<int>(tile_offset_x), 
				    static_cast<int>(tile_offset_y), 
				    0, 
				    exr_convert_pixel_format(props.m_pixel_format), 
				    tile.get_storage()))
			{
				const std::string msg = out->geterror();
				out->close();
				OIIO::ImageOutput::destroy(out);
				throw ExceptionIOError(msg.c_str());
			}
		}
	}
}

void EXRImageFileWriter::begin_multipart_exr()
{
    assert(impl == nullptr);
    impl = new Impl();
}

void EXRImageFileWriter::append_part(
    const char*             part_name,
    const ICanvas&          image,
    const ImageAttributes&  image_attributes,
    const size_t            channel_count,
    const char**            channel_names)
{
    assert(impl);

    impl->m_images.push_back(&image);
    impl->m_channel_counts.push_back(channel_count);
    impl->m_channel_names.push_back(channel_names);

    // Retrieve canvas properties.
    const CanvasProperties& props = image.properties();

    // Build the header.
    PixelType pixel_type;
    Header header = build_header(props, image_attributes, channel_count, channel_names, pixel_type);
    header.setName(part_name);
    header.setType(TILEDIMAGE);

    impl->m_image_headers.push_back(header);
}

void EXRImageFileWriter::write_multipart_exr(const char *filename)
{
    if (!impl->m_images.empty())
    {
        initialize_openexr();
        MultiPartOutputFile multipart_exr(
            filename,
            impl->m_image_headers.data(),
            static_cast<int>(impl->m_images.size()));

        for (size_t i = 0, e = impl->m_images.size(); i < e; ++i)
        {
            TiledOutputPart file(multipart_exr, static_cast<int>(i));
            write_tiles(file, *impl->m_images[i], impl->m_channel_counts[i], impl->m_channel_names[i]);
        }
    }

    delete impl;
}

}   // namespace foundation
