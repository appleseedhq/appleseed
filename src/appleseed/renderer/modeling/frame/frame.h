
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

#pragma once

// appleseed.renderer headers.
#include "renderer/modeling/aov/aovcontainer.h"
#include "renderer/modeling/entity/entity.h"
#include "renderer/modeling/postprocessingstage/postprocessingstagecontainer.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/colorspace.h"
#include "foundation/math/aabb.h"
#include "foundation/math/filter.h"
#include "foundation/math/vector.h"
#include "foundation/platform/types.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/uid.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cassert>
#include <cstddef>

// Forward declarations.
namespace foundation    { class DictionaryArray; }
namespace foundation    { class IAbortSwitch; }
namespace foundation    { class Image; }
namespace foundation    { class ImageAttributes; }
namespace foundation    { class StringArray; }
namespace foundation    { class StringDictionary; }
namespace foundation    { class Tile; }
namespace renderer      { class BaseGroup; }
namespace renderer      { class DenoiserAOV; }
namespace renderer      { class ImageStack; }
namespace renderer      { class OnFrameBeginRecorder; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Project; }

namespace renderer
{

//
// Frame class.
//
// Pixels in a frame are expressed in linear RGB and use premultiplied alpha.
//

class APPLESEED_DLLSYMBOL Frame
  : public Entity
{
  public:
    // Return the unique ID of this class of entities.
    static foundation::UniqueID get_class_uid();

    // Delete this instance.
    void release() override;

    // Print this entity's parameters to the renderer's global logger.
    void print_settings();

    // Access the AOVs.
    AOVContainer& aovs() const;

    // Access the post-processing stages.
    PostProcessingStageContainer& post_processing_stages() const;

    // Return the name of the active camera.
    const char* get_active_camera_name() const;

    // Access the main underlying image.
    foundation::Image& image() const;

    // Clear the main and AOV images to transparent black.
    void clear_main_and_aov_images();

    // Access the AOV images.
    ImageStack& aov_images() const;

    // Return the reconstruction filter used by the main image and the AOV images.
    const foundation::Filter2f& get_filter() const;

    // Set/get the crop window. The crop window is inclusive on all sides.
    void reset_crop_window();
    bool has_crop_window() const;
    void set_crop_window(const foundation::AABB2u& crop_window);
    const foundation::AABB2u& get_crop_window() const;

    // Get the noise seed.
    foundation::uint32 get_noise_seed() const;

    // Expose asset file paths referenced by this entity to the outside.
    void collect_asset_paths(foundation::StringArray& paths) const override;
    void update_asset_paths(const foundation::StringDictionary& mappings) override;

    // This method is called once before rendering each frame.
    // Returns true on success, false otherwise.
    bool on_frame_begin(
        const Project&              project,
        const BaseGroup*            parent,
        OnFrameBeginRecorder&       recorder,
        foundation::IAbortSwitch*   abort_switch = nullptr) override;

    // Return the normalized device coordinates of a given sample.
    foundation::Vector2d get_sample_position(
        const double                sample_x,               // x coordinate of the sample in the image, in [0,width)
        const double                sample_y) const;        // y coordinate of the sample in the image, in [0,height)
    foundation::Vector2d get_sample_position(
        const size_t                pixel_x,                // x coordinate of the pixel in the image
        const size_t                pixel_y,                // y coordinate of the pixel in the image
        const double                sample_x,               // x coordinate of the sample in the pixel, in [0,1)
        const double                sample_y) const;        // y coordinate of the sample in the pixel, in [0,1)
    foundation::Vector2d get_sample_position(
        const size_t                tile_x,                 // x coordinate of the tile in the image
        const size_t                tile_y,                 // y coordinate of the tile in the image
        const size_t                pixel_x,                // x coordinate of the pixel in the tile
        const size_t                pixel_y,                // y coordinate of the pixel in the tile
        const double                sample_x,               // x coordinate of the sample in the pixel, in [0,1)
        const double                sample_y) const;        // y coordinate of the sample in the pixel, in [0,1)

    // Do any post-process needed by AOV images.
    void post_process_aov_images() const;

    // Access render info. Render info contain statistics and additional results
    // from the rendering process such as render time. They are used in particular
    // by the Render Stamp post-processing stage.
    ParamArray& render_info();

    enum class DenoisingMode
    {
        Off,
        WriteOutputs,
        Denoise
    };

    // Retrieve the selected denoising mode.
    DenoisingMode get_denoising_mode() const;

    // Run the denoiser on the frame.
    void denoise(
        const size_t                thread_count,
        foundation::IAbortSwitch*   abort_switch) const;

    // Write the main image to disk.
    // Return true if successful, false otherwise.
    bool write_main_image(const char* file_path) const;

    // Write the AOV images to disk.
    // Return true if successful, false otherwise.
    bool write_aov_images(const char* file_path) const;

    // Write the main image and the AOV images to disk.
    // Output file paths are taken from the frame's and AOVs' "output_filename" parameters.
    // Return true if successful, false otherwise.
    bool write_main_and_aov_images() const;

    // Write the main image and the AOV images to a multipart OpenEXR file.
    void write_main_and_aov_images_to_multipart_exr(const char* file_path) const;

    // Archive the frame to a given directory on disk. If output_path is provided,
    // the full path to the output file will be returned. The returned string must
    // be freed using foundation::free_string().
    // Return true if successful, false otherwise.
    bool archive(
        const char*                 directory,
        char**                      output_path = nullptr) const;

  private:
    friend class AOVAccumulatorContainer;
    friend class FrameFactory;

    struct Impl;
    Impl* impl;

    foundation::CanvasProperties m_props;

    // Constructor.
    Frame(
        const char*                 name,
        const ParamArray&           params,
        const AOVContainer&         aovs);

    // Destructor.
    ~Frame() override;

    void extract_parameters();

    // Access the internal AOVs.
    AOVContainer& internal_aovs() const;
};


//
// FrameFactory class implementation.
//

class APPLESEED_DLLSYMBOL FrameFactory
{
  public:
    // Return a set of input metadata for frames.
    static foundation::DictionaryArray get_input_metadata();

    // Create a new frame.
    static foundation::auto_release_ptr<Frame> create(
        const char*                 name,
        const ParamArray&           params);

    // Create a new frame.
    static foundation::auto_release_ptr<Frame> create(
        const char*                 name,
        const ParamArray&           params,
        const AOVContainer&         aovs);
};


//
// Frame class implementation.
//

inline foundation::Vector2d Frame::get_sample_position(
    const double    sample_x,
    const double    sample_y) const
{
    return
        foundation::Vector2d(
            sample_x * m_props.m_rcp_canvas_width,
            sample_y * m_props.m_rcp_canvas_height);
}

inline foundation::Vector2d Frame::get_sample_position(
    const size_t    pixel_x,
    const size_t    pixel_y,
    const double    sample_x,
    const double    sample_y) const
{
    return
        get_sample_position(
            pixel_x + sample_x,
            pixel_y + sample_y);
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

    return
        get_sample_position(
            tile_x * m_props.m_tile_width + pixel_x,
            tile_y * m_props.m_tile_height + pixel_y,
            sample_x,
            sample_y);
}

}   // namespace renderer
