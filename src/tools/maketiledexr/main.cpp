
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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

// Project headers.
#include "commandlinehandler.h"

// appleseed.shared headers.
#include "application/application.h"
#include "application/superlogger.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exception.h"
#include "foundation/core/exceptions/stringexception.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/exceptionunsupportedimageformat.h"
#include "foundation/image/genericprogressiveimagefilereader.h"
#include "foundation/image/imageattributes.h"
#include "foundation/image/pixel.h"
#include "foundation/image/progressiveexrimagefilewriter.h"
#include "foundation/image/tile.h"
#include "foundation/platform/types.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/log.h"

// OpenEXR headers.
#include "OpenEXR/ImfChannelList.h"
#include "OpenEXR/ImfFrameBuffer.h"
#include "OpenEXR/ImfHeader.h"
#include "OpenEXR/ImfPixelType.h"
#include "OpenEXR/ImfTileDescription.h"

// Standard headers.
#include <cstddef>
#include <memory>

using namespace appleseed::maketiledexr;
using namespace appleseed::shared;
using namespace foundation;
using namespace std;


//
// Entry point of maketiledexr.
//

int main(int argc, const char* argv[])
{
    SuperLogger logger;
    Application::check_installation(logger);

    CommandLineHandler cl;
    cl.parse(argc, argv, logger);

    // Retrieve the tile size.
    size_t tile_width = 32;
    size_t tile_height = 32;
    if (cl.m_tile_size.is_set())
    {
        const int tw = cl.m_tile_size.values()[0];
        const int th = cl.m_tile_size.values()[1];
        if (tw > 0 && th > 0)
        {
            tile_width = static_cast<size_t>(tw);
            tile_height = static_cast<size_t>(th);
        }
        else
        {
            LOG_ERROR(
                logger,
                "invalid tile size, using default size %dx%d pixels.",
                tile_width,
                tile_height);
        }
    }

    try
    {
        // Open the input file.
        GenericProgressiveImageFileReader reader(&logger, tile_width, tile_height);
        reader.open(cl.m_filenames.values()[0].c_str());

        // Read canvas properties from the input file.
        CanvasProperties props;
        reader.read_canvas_properties(props);

        // Read image attributes from the input file.
        ImageAttributes attrs;
        reader.read_image_attributes(attrs);

        // Open the output file.
        ProgressiveEXRImageFileWriter writer(&logger);
        bool require_format_conversion = false;
        try
        {
            writer.open(cl.m_filenames.values()[1].c_str(), props, attrs);
        }
        catch (const ExceptionUnsupportedImageFormat&)
        {
            props =
                CanvasProperties(
                    props.m_canvas_width,
                    props.m_canvas_height,
                    props.m_tile_width,
                    props.m_tile_height,
                    props.m_channel_count,
                    PixelFormatFloat);
            writer.open(cl.m_filenames.values()[1].c_str(), props, attrs);
            require_format_conversion = true;
        }

        // Copy the tiles.
        for (size_t y = 0; y < props.m_tile_count_y; ++y)
        {
            // Print a progress message for every row.
            if (cl.m_progress_messages.is_set())
            {
                LOG_INFO(
                    logger,
                    "processing tile row " FMT_SIZE_T "/" FMT_SIZE_T "...",
                    y + 1,
                    props.m_tile_count_y);
            }

            // Copy the tiles from this row.
            for (size_t x = 0; x < props.m_tile_count_x; ++x)
            {
                // Read the tile. Use auto_release_ptr<> because the image
                // was allocated in appleseed's shared library.
                auto_release_ptr<Tile> tile(reader.read_tile(x, y));

                if (require_format_conversion)
                {
                    // Convert and write the tile.
                    auto_ptr<Tile> converted_tile(new Tile(tile.ref(), PixelFormatFloat));
                    writer.write_tile(*converted_tile.get(), x, y);
                }
                else
                {
                    // Write the tile.
                    writer.write_tile(tile.ref(), x, y);
                }
            }
        }

        // Close the files.
        writer.close();
        reader.close();
    }
    catch (const StringException& e)
    {
        LOG_FATAL(logger, "%s: %s", e.what(), e.string());
        return 1;
    }
    catch (const Exception& e)
    {
        LOG_FATAL(logger, "%s", e.what());
        return 1;
    }

    return 0;
}
