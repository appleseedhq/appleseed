
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
#include "frame.h"

// appleseed.foundation headers.
#include "foundation/core/exception.h"
#include "foundation/image/colorspace.h"
#include "foundation/image/exrimagefilewriter.h"
#include "foundation/image/genericimagefilewriter.h"
#include "foundation/image/image.h"
#include "foundation/image/imageattributes.h"
#include "foundation/image/pixel.h"
#include "foundation/image/tile.h"
#include "foundation/utility/stopwatch.h"
#include "foundation/utility/string.h"
#include "foundation/utility/test.h"

// boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <algorithm>
#include <cstring>

using namespace boost;
using namespace foundation;
using namespace std;

namespace renderer
{

//
// Frame class implementation.
//

struct Frame::Impl
{
    // Parameters.
    struct Parameters
    {
        ParamArray      m_params;
        size_t          m_frame_width;
        size_t          m_frame_height;
        size_t          m_tile_width;
        size_t          m_tile_height;
        PixelFormat     m_pixel_format;
        ColorSpace      m_color_space;
        bool            m_gamma_correct;
        float           m_target_gamma;
        float           m_rcp_target_gamma;

        // Constructor, extract parameters.
        explicit Parameters(const ParamArray& params)
          : m_params(params)
        {
            // Retrieve frame resolution parameter.
            const Vector2i DefaultResolution(512, 512);
            Vector2i resolution = params.get_required<Vector2i>("resolution", DefaultResolution);
            if (resolution[0] < 1 || resolution[1] < 1)
            {
                // Invalid value for resolution parameter, use default.
                RENDERER_LOG_ERROR(
                    "invalid value \"%d %d\" for parameter \"%s\", using default value \"%d %d\"",
                    resolution[0],
                    resolution[1],
                    "resolution",
                    DefaultResolution[0],
                    DefaultResolution[1]);
                resolution = DefaultResolution;
            }
            m_frame_width = static_cast<size_t>(resolution[0]);
            m_frame_height = static_cast<size_t>(resolution[1]);

            // Retrieve tile size parameter.
            const Vector2i DefaultTileSize(32, 32);
            Vector2i tile_size = params.get_optional<Vector2i>("tile_size", DefaultTileSize);
            if (tile_size[0] < 1 || tile_size[1] < 1)
            {
                // Invalid value for tile_size parameter, use default.
                RENDERER_LOG_ERROR(
                    "invalid value \"%d %d\" for parameter \"%s\", using default value \"%d %d\"",
                    tile_size[0],
                    tile_size[1],
                    "tile_size",
                    DefaultTileSize[0],
                    DefaultTileSize[1]);
                tile_size = DefaultTileSize;
            }
            m_tile_width = static_cast<size_t>(tile_size[0]);
            m_tile_height = static_cast<size_t>(tile_size[1]);

            // Retrieve pixel format parameter.
            const PixelFormat DefaultPixelFormat = PixelFormatFloat;
            const char* DefaultPixelFormatString = "float";
            const string pixel_format =
                params.get_optional<string>("pixel_format", DefaultPixelFormatString);
            if (pixel_format == "uint8")
                m_pixel_format = PixelFormatUInt8;
            else if (pixel_format == "uint16")
                m_pixel_format = PixelFormatUInt16;
            else if (pixel_format == "uint32")
                m_pixel_format = PixelFormatUInt32;
            else if (pixel_format == "half")
                m_pixel_format = PixelFormatHalf;
            else if (pixel_format == "float")
                m_pixel_format = PixelFormatFloat;
            else if (pixel_format == "double")
                m_pixel_format = PixelFormatDouble;
            else
            {
                // Invalid value for pixel_format parameter, use default.
                RENDERER_LOG_ERROR(
                    "invalid value \"%s\" for parameter \"%s\", using default value \"%s\"",
                    pixel_format.c_str(),
                    "pixel_format",
                    DefaultPixelFormatString);
                m_pixel_format = DefaultPixelFormat;
            }

            // Retrieve color space parameter.
            const ColorSpace DefaultColorSpace = ColorSpaceLinearRGB;
            const char* DefaultColorSpaceString = "linear_rgb";
            const string color_space =
                params.get_optional<string>("color_space", DefaultColorSpaceString);
            if (color_space == "linear_rgb")
                m_color_space = ColorSpaceLinearRGB;
            else if (color_space == "srgb")
                m_color_space = ColorSpaceSRGB;
            else if (color_space == "ciexyz")
                m_color_space = ColorSpaceCIEXYZ;
            else
            {
                // Invalid value for color_space parameter, use default.
                RENDERER_LOG_ERROR(
                    "invalid value \"%s\" for parameter \"%s\", using default value \"%s\"",
                    color_space.c_str(),
                    "color_space",
                    DefaultColorSpaceString);
                m_color_space = DefaultColorSpace;
            }

            // Retrieve gamma correction parameter.
            m_gamma_correct = params.strings().exist("gamma_correction");
            if (m_gamma_correct)
            {
                m_target_gamma = params.get_required<float>("gamma_correction", 2.2f);
                m_rcp_target_gamma = 1.0f / m_target_gamma;
            }
            else
            {
                m_target_gamma = 1.0f;
                m_rcp_target_gamma = 1.0f;
            }
        }
    };

    UniqueID            m_uid;
    string              m_name;
    Parameters          m_params;
    auto_ptr<Image>     m_image;
    LightingConditions  m_lighting_conditions;

    // Constructor.
    Impl(const ParamArray& params)
      : m_params(params)
      , m_lighting_conditions(IlluminantCIED65, XYZCMFCIE196410Deg)
    {
    }
};

// Constructor.
Frame::Frame(
    const char*         name,
    const ParamArray&   params)
  : impl(new Impl(params))
{
    assert(name);

    impl->m_uid = g_uid_source.get();
    impl->m_name = name;

    // Create the underlying image.
    impl->m_image.reset(
        new Image(
            impl->m_params.m_frame_width,
            impl->m_params.m_frame_height,
            impl->m_params.m_tile_width,
            impl->m_params.m_tile_height,
            4,
            impl->m_params.m_pixel_format));

    // Retrieve the image properties.
    m_props = impl->m_image->properties();
}

// Destructor.
Frame::~Frame()
{
    delete impl;
}

// Return the unique ID of this object.
UniqueID Frame::get_uid() const
{
    return impl->m_uid;
}

// Return the name of this entity.
const char* Frame::get_name() const
{
    return impl->m_name.c_str();
}

// Return the parameters of this object.
ParamArray& Frame::get_parameters()
{
    return impl->m_params.m_params;
}
const ParamArray& Frame::get_parameters() const
{
    return impl->m_params.m_params;
}

// Access canvas properties.
const CanvasProperties& Frame::properties() const
{
    return m_props;
}

// Return the lighting conditions for spectral to RGB conversion.
const LightingConditions& Frame::get_lighting_conditions() const
{
    return impl->m_lighting_conditions;
}

// Direct access to a given tile.
Tile& Frame::tile(
    const size_t        tile_x,
    const size_t        tile_y) const
{
    return impl->m_image->tile(tile_x, tile_y);
}

// Convert an RGBA tile from linear RGB to the color space of the frame.
void Frame::transform_tile_to_frame_color_space(Tile& tile) const
{
    assert(tile.get_channel_count() == 4);

    const size_t tile_width = tile.get_width();
    const size_t tile_height = tile.get_height();

    for (size_t y = 0; y < tile_height; ++y)
    {
        for (size_t x = 0; x < tile_width; ++x)
        {
            Color4f linear_rgb;
            tile.get_pixel(x, y, linear_rgb);
            tile.set_pixel(x, y, linear_rgb_to_frame(linear_rgb));
        }
    }
}

// Convert an RGBA image from linear RGB to the color space of the frame.
void Frame::transform_image_to_frame_color_space(Image& image) const
{
    const CanvasProperties& image_props = image.properties();

    assert(image_props.m_channel_count == 4);

    for (size_t ty = 0; ty < image_props.m_tile_count_y; ++ty)
    {
        for (size_t tx = 0; tx < image_props.m_tile_count_x; ++tx)
        {
            transform_tile_to_frame_color_space(image.tile(tx, ty));
        }
    }
}

namespace
{
    double write_image(
        const string&           filename,
        const Image&            image,
        const ImageAttributes&  image_attributes)
    {
        Stopwatch<DefaultWallclockTimer> stopwatch;
        stopwatch.start();

        try
        {
            GenericImageFileWriter writer;
            writer.write(filename, image, image_attributes);
        }
        catch (const GenericImageFileWriter::ExceptionUnknownFileTypeError&)
        {
            // Extract the extension of the image filename.
            const filesystem::path filepath(filename);
            const string extension = lower_case(filepath.extension());

            // Emit an error message.
            RENDERER_LOG_ERROR(
                "file format '%s' not supported, writing the image in OpenEXR format "
                "(but keeping the filename unmodified)",
                extension.c_str());

            // Write the image in OpenEXR format.
            EXRImageFileWriter writer;
            writer.write(filename, image, image_attributes);
        }

        stopwatch.measure();

        return stopwatch.get_seconds();
    }
}

// Write the frame to disk.
bool Frame::write(const char* filename) const
{
    assert(filename);

    try
    {
        Image final_image(*impl->m_image);
        transform_image_to_frame_color_space(final_image);

        const double seconds =
            write_image(
                filename,
                final_image,
                ImageAttributes::create_default_attributes());

        RENDERER_LOG_INFO(
            "wrote image file %s in %s", filename, pretty_time(seconds).c_str());
    }
    catch (const IImageFileWriter::ExceptionIOError&)
    {
        RENDERER_LOG_ERROR(
            "failed to write image file %s: i/o error",
            filename);

        return false;
    }
    catch (const Exception& e)
    {
        RENDERER_LOG_ERROR(
            "failed to write image file %s: %s",
            filename,
            e.what());

        return false;
    }

    return true;
}

// Archive the frame to disk, in a given directory.
bool Frame::archive(
    const char*     directory,
    char**          output_path) const
{
    assert(directory);

    // Construct the name of the image file.
    const string filename =
        "autosave." + get_time_stamp_string() + ".exr";

    // Construct the path to the image file.
    const filesystem::path image_path = filesystem::path(directory) / filename;

    // Return the path to the image file.
    if (output_path)
    {
        const string& path = image_path.file_string();
        *output_path = new char[path.size() + 1];
        strncpy(*output_path, path.c_str(), path.size() + 1);
    }

    try
    {
        Image final_image(*impl->m_image);
        transform_image_to_frame_color_space(final_image);

        const double seconds =
            write_image(
                image_path.file_string(),
                final_image,
                ImageAttributes::create_default_attributes());

        RENDERER_LOG_INFO(
            "frame successfully archived to %s in %s",
            image_path.file_string().c_str(),
            pretty_time(seconds).c_str());
    }
    catch (const IImageFileWriter::ExceptionIOError&)
    {
        RENDERER_LOG_WARNING(
            "automatic frame archiving to %s failed: i/o error",
            image_path.file_string().c_str());

        return false;
    }
    catch (const Exception& e)
    {
        RENDERER_LOG_WARNING(
            "automatic frame archiving to %s failed: %s",
            image_path.file_string().c_str(),
            e.what());

        return false;
    }

    return true;
}

namespace
{
    inline Color4f clamp_to_zero(const Color4f& c)
    {
        return Color4f(
            max(c[0], 0.0f),
            max(c[1], 0.0f),
            max(c[2], 0.0f),
            max(c[3], 0.0f));
    }

    double accumulate_luminance(const Tile& tile)
    {
        double accumulated_luminance = 0.0;

        const size_t tile_width = tile.get_width();
        const size_t tile_height = tile.get_height();

        for (size_t y = 0; y < tile_height; ++y)
        {
            for (size_t x = 0; x < tile_width; ++x)
            {
                Color4f linear_rgba;
                tile.get_pixel(x, y, linear_rgba);

                linear_rgba = clamp_to_zero(linear_rgba);

                const float lum = luminance(linear_rgba.rgb());
                assert(lum >= 0.0f);

                accumulated_luminance += static_cast<double>(lum);
            }
        }

        return accumulated_luminance;
    }

    double accumulate_luminance(const Image& image)
    {
        double accumulated_luminance = 0.0;

        const CanvasProperties& props = image.properties();

        for (size_t ty = 0; ty < props.m_tile_count_y; ++ty)
        {
            for (size_t tx = 0; tx < props.m_tile_count_x; ++tx)
            {
                const Tile& tile = image.tile(tx, ty);
                accumulated_luminance += accumulate_luminance(tile);
            }
        }

        return accumulated_luminance;
    }

    double compute_avg_luminance(const Image& image)
    {
        const double accumulated_luminance = accumulate_luminance(image);

        const CanvasProperties& props = image.properties();
        const double average_luminance = accumulated_luminance / props.m_pixel_count;

        return average_luminance;
    }

    FOUNDATION_TEST_SUITE(Renderer_Modeling_Frame_Details)
    {
        FOUNDATION_TEST_CASE(ClampToZero_GivenColorWithNegativeValues_ReplacesNegativeValuesWithZeroes)
        {
            const Color4f result = clamp_to_zero(Color4f(-1.0f, 0.0f, 1.0f, -0.0f));

            FOUNDATION_EXPECT_EQ(Color4f(0.0f, 0.0f, 1.0f, -0.0f), result);
        }

        FOUNDATION_TEST_CASE(AccumulateLuminance_Given2x2TileFilledWithZeroes_ReturnsZero)
        {
            Tile tile(2, 2, 4, PixelFormatFloat);
            tile.clear(Color4f(0.0f));

            const double accumulated_luminance = accumulate_luminance(tile);

            FOUNDATION_EXPECT_EQ(0.0, accumulated_luminance);
        }

        FOUNDATION_TEST_CASE(AccumulateLuminance_Given2x2TileFilledWithOnes_ReturnsFour)
        {
            Tile tile(2, 2, 4, PixelFormatFloat);
            tile.clear(Color4f(1.0f));

            const double accumulated_luminance = accumulate_luminance(tile);

            FOUNDATION_EXPECT_FEQ(4.0, accumulated_luminance);
        }

        FOUNDATION_TEST_CASE(AccumulateLuminance_Given2x2TileFilledWithMinusOnes_ReturnsZero)
        {
            Tile tile(2, 2, 4, PixelFormatFloat);
            tile.clear(Color4f(-1.0f));

            const double accumulated_luminance = accumulate_luminance(tile);

            FOUNDATION_EXPECT_EQ(0.0, accumulated_luminance);
        }

        void clear_image(Image& image, const Color4f& color)
        {
            const CanvasProperties& props = image.properties();

            for (size_t ty = 0; ty < props.m_tile_count_y; ++ty)
            {
                for (size_t tx = 0; tx < props.m_tile_count_x; ++tx)
                {
                    Tile& tile = image.tile(tx, ty);
                    tile.clear(color);
                }
            }
        }

        FOUNDATION_TEST_CASE(AccumulateLuminance_Given4x4ImageFilledWithZeroes_ReturnsZero)
        {
            Image image(4, 4, 2, 2, 4, PixelFormatFloat);
            clear_image(image, Color4f(0.0f));

            const double accumulated_luminance = accumulate_luminance(image);

            FOUNDATION_EXPECT_EQ(0.0, accumulated_luminance);
        }

        FOUNDATION_TEST_CASE(AccumulateLuminance_Given4x4ImageFilledWithOnes_ReturnsSixteen)
        {
            Image image(4, 4, 2, 2, 4, PixelFormatFloat);
            clear_image(image, Color4f(1.0f));

            const double accumulated_luminance = accumulate_luminance(image);

            FOUNDATION_EXPECT_FEQ(16.0, accumulated_luminance);
        }

        FOUNDATION_TEST_CASE(ComputeAvgLuminance_Given4x4ImageFilledWithZeroes_ReturnsZero)
        {
            Image image(4, 4, 2, 2, 4, PixelFormatFloat);
            clear_image(image, Color4f(0.0f));

            const double average_luminance = compute_avg_luminance(image);

            FOUNDATION_EXPECT_EQ(0.0, average_luminance);
        }

        FOUNDATION_TEST_CASE(ComputeAvgLuminance_Given4x4ImageFilledWithOnes_ReturnsOne)
        {
            Image image(4, 4, 2, 2, 4, PixelFormatFloat);
            clear_image(image, Color4f(1.0f));

            const double average_luminance = compute_avg_luminance(image);

            FOUNDATION_EXPECT_FEQ(1.0, average_luminance);
        }
    }
}

// Compute and return the average luminance of the frame.
double Frame::compute_average_luminance() const
{
    return compute_avg_luminance(*impl->m_image.get());
}

// Transform a linear RGB color to the color space of the frame.
Color4f Frame::linear_rgb_to_frame(const Color4f& linear_rgb) const
{
    Color4f result;

    // Transform the input color to the color space of the frame.
    switch (impl->m_params.m_color_space)
    {
      case ColorSpaceLinearRGB:
        result = linear_rgb;
        break;

      case ColorSpaceSRGB:
        result.rgb() = fast_linear_rgb_to_srgb(linear_rgb.rgb());
        result.a = linear_rgb.a;
        break;

      case ColorSpaceCIEXYZ:
        result.rgb() = linear_rgb_to_ciexyz(linear_rgb.rgb());
        result.a = linear_rgb.a;
        break;

      default:
        assert(!"Invalid target color space.");
        result = linear_rgb;
        break;
    }

    // Clamp all pixel color channels to [0,1].
    // todo: mark clamped pixels in the diagnostic map.
    result = saturate(result);

    // Gamma-correct the pixel color if gamma correction is enabled.
    if (impl->m_params.m_gamma_correct)
    {
        // todo: investigate the usage of fast_pow() for gamma correction.
        const float rcp_target_gamma = impl->m_params.m_rcp_target_gamma;
        result[0] = pow(result[0], rcp_target_gamma);
        result[1] = pow(result[1], rcp_target_gamma);
        result[2] = pow(result[2], rcp_target_gamma);
    }

    return result;
}

}   // namespace renderer
