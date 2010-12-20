
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

#ifndef APPLESEED_RENDERER_MODELING_FRAME_FRAME_H
#define APPLESEED_RENDERER_MODELING_FRAME_FRAME_H

// appleseed.renderer headers.
#include "renderer/global/global.h"
#include "renderer/modeling/entity/entity.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"

// Forward declarations.
namespace foundation    { class Image; }
namespace foundation    { class LightingConditions; }
namespace foundation    { class Tile; }

namespace renderer
{

//
// Frame class.
//

class RENDERERDLL Frame
  : public Entity
{
  public:
    // Constructor.
    Frame(
        const char*         name,
        const ParamArray&   params);

    // Destructor.
    ~Frame();

    // Delete this instance.
    virtual void release();

    // Access canvas properties.
    const foundation::CanvasProperties& properties() const;

    // Return the lighting conditions for spectral to RGB conversion.
    const foundation::LightingConditions& get_lighting_conditions() const;

    // Direct access to a given tile of the frame.
    // The tile is always in linear RGB color space.
    foundation::Tile& tile(
        const size_t        tile_x,
        const size_t        tile_y) const;

    // Return the coordinates of a given sample.
    // The returned point is expressed in normalized device coordinates [-0.5,0.5]^2.
    foundation::Vector2d get_sample_position(
        const double        sample_x,           // x coordinate of the sample in the image, in [0,1)
        const double        sample_y) const;    // y coordinate of the sample in the image, in [0,1)
    foundation::Vector2d get_sample_position(
        const size_t        pixel_x,            // x coordinate of the pixel in the image
        const size_t        pixel_y,            // y coordinate of the pixel in the image
        const double        sample_x,           // x coordinate of the sample in the pixel, in [0,1)
        const double        sample_y) const;    // y coordinate of the sample in the pixel, in [0,1)
    foundation::Vector2d get_sample_position(
        const size_t        tile_x,             // x coordinate of the tile in the image
        const size_t        tile_y,             // y coordinate of the tile in the image
        const size_t        pixel_x,            // x coordinate of the pixel in the tile
        const size_t        pixel_y,            // y coordinate of the pixel in the tile
        const double        sample_x,           // x coordinate of the sample in the pixel, in [0,1)
        const double        sample_y) const;    // y coordinate of the sample in the pixel, in [0,1)

    // Convert an RGBA tile or image from linear RGB to the color space of the frame.
    void transform_tile_to_frame_color_space(foundation::Tile& tile) const;
    void transform_image_to_frame_color_space(foundation::Image& image) const;

    // Write the frame to disk.
    // Return true if successful, false otherwise.
    bool write(const char* filename) const;

    // Archive the frame to a given directory on disk. If output_path is provided,
    // the full path to the output file will be returned. The returned string must
    // be freed using foundation::free_string().
    // Return true if successful, false otherwise.
    bool archive(
        const char*         directory,
        char**              output_path = 0) const;

    // Compute and return the average luminance of the frame.
    double compute_average_luminance() const;

  private:
    // Private implementation.
    struct Impl;
    Impl* impl;

    // Derogate to the private implementation rule, as we want get_sample_position()
    // to be inline, for performance reasons -- but it needs the canvas properties.
    foundation::CanvasProperties m_props;

    void extract_parameters();

    // Transform a linear RGB color to the color space of the frame.
    foundation::Color4f linear_rgb_to_frame(
        const foundation::Color4f&  linear_rgb) const;
};


//
// Frame class implementation.
//

inline foundation::Vector2d Frame::get_sample_position(
    const double    sample_x,
    const double    sample_y) const
{
    assert(sample_x >= 0.0 && sample_x < 1.0);
    assert(sample_y >= 0.0 && sample_y < 1.0);

    const foundation::Vector2d p(sample_x - 0.5, 0.5 - sample_y);

    assert(p.x >= -0.5 && p.x <= 0.5);
    assert(p.y >= -0.5 && p.y <= 0.5);

    return p;
}

inline foundation::Vector2d Frame::get_sample_position(
    const size_t    pixel_x,
    const size_t    pixel_y,
    const double    sample_x,
    const double    sample_y) const
{
    assert(pixel_x < m_props.m_canvas_width);
    assert(pixel_y < m_props.m_canvas_height);
    assert(sample_x >= 0.0 && sample_x < 1.0);
    assert(sample_y >= 0.0 && sample_y < 1.0);

    foundation::Vector2d p;

    // Compute sample coordinates in image space.
    p.x = pixel_x + sample_x;
    p.y = pixel_y + sample_y;

    // Remap to normalized device coordinates.
    p.x = p.x * m_props.m_rcp_canvas_width - 0.5;
    p.y = 0.5 - p.y * m_props.m_rcp_canvas_height;

    assert(p.x >= -0.5 && p.x <= 0.5);
    assert(p.y >= -0.5 && p.y <= 0.5);

    return p;
}

inline foundation::Vector2d Frame::get_sample_position(
    const size_t    tile_x,
    const size_t    tile_y,
    const size_t    pixel_x,
    const size_t    pixel_y,
    const double    sample_x,
    const double    sample_y) const
{
    assert(tile_x < m_props.m_tile_count_x);
    assert(tile_y < m_props.m_tile_count_y);
    assert(pixel_x < m_props.m_tile_width);
    assert(pixel_y < m_props.m_tile_height);
    assert(sample_x >= 0.0 && sample_x < 1.0);
    assert(sample_y >= 0.0 && sample_y < 1.0);

    return
        get_sample_position(
            tile_x * m_props.m_tile_width + pixel_x,
            tile_y * m_props.m_tile_height + pixel_y,
            sample_x,
            sample_y);
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_FRAME_FRAME_H
