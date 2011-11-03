
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "foundation/image/genericprogressiveimagefilereader.h"
#include "foundation/image/imageattributes.h"
#include "foundation/image/progressiveexrimagefilewriter.h"
#include "foundation/image/tile.h"
#include "foundation/platform/types.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/log.h"

// OpenEXR headers.
#include "openexr/ImfChannelList.h"
#include "openexr/ImfFrameBuffer.h"
#include "openexr/ImfHeader.h"
#include "openexr/ImfPixelType.h"
#include "openexr/ImfTileDescription.h"

// Standard headers.
#include <cstddef>

using namespace appleseed::maketiledexr;
using namespace appleseed::shared;
using namespace foundation;
using namespace std;

namespace
{
    CommandLineHandler g_cl;
}


//
// Entry point of maketiledexr.
//

int main(int argc, const char* argv[])
{
    SuperLogger logger;
    logger.get_log_target().set_formatting_flags(LogMessage::DisplayMessage);

    // Make sure the application is properly installed, bail out if not.
    Application::check_installation(logger);

    // Parse the command line.
    g_cl.parse(argc, argv, logger);

    logger.get_log_target().reset_formatting_flags();

    // Retrieve the tile size.
    size_t tile_width = 32;
    size_t tile_height = 32;
    if (g_cl.m_tile_size.found())
    {
        const int tw = g_cl.m_tile_size.values()[0];
        const int th = g_cl.m_tile_size.values()[1];
        if (tw > 0 && th > 0)
        {
            tile_width = static_cast<size_t>(tw);
            tile_height = static_cast<size_t>(th);
        }
        else
        {
            LOG_ERROR(
                logger,
                "invalid tile size, using default size %dx%d pixels",
                tile_width,
                tile_height);
        }
    }

    try
    {
        // Open the input file.
        GenericProgressiveImageFileReader reader(&logger, tile_width, tile_height);
        reader.open(g_cl.m_filenames.values()[0].c_str());

        // Read canvas properties from the input file.
        CanvasProperties props;
        reader.read_canvas_properties(props);

        // Read image attributes from the input file.
        ImageAttributes attrs;
        reader.read_image_attributes(attrs);

        // Open the output file.
        ProgressiveEXRImageFileWriter writer(&logger);
        writer.open(g_cl.m_filenames.values()[1].c_str(), props, attrs);

        // Copy the tiles.
        for (size_t y = 0; y < props.m_tile_count_y; ++y)
        {
            // Print a progress message.
            if (g_cl.m_progress_messages.found())
            {
                LOG_INFO(
                    logger,
                    "processing tile row " FMT_SIZE_T "/" FMT_SIZE_T "...",
                    y + 1,
                    props.m_tile_count_y);
            }

            for (size_t x = 0; x < props.m_tile_count_x; ++x)
            {
                // Read the tile.
                const Tile* tile = reader.read_tile(x, y);
                if (tile == 0)
                    throw Exception("couldn't read a tile");

                // Write the tile.
                writer.write_tile(*tile, x, y);

                // Deallocate the tile.
                delete tile;
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
