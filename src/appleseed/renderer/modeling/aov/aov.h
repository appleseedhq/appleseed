
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/modeling/entity/entity.h"

// appleseed.foundation headers.
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/utility/uid.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class Image; }
namespace foundation    { class ImageAttributes; }
namespace renderer      { class AOVAccumulator; }
namespace renderer      { class AOVAccumulatorContainer; }
namespace renderer      { class Frame; }
namespace renderer      { class ImageStack; }
namespace renderer      { class ParamArray; }

namespace renderer
{

//
// AOV base class.
//

class APPLESEED_DLLSYMBOL AOV
  : public Entity
{
  public:
    // Return the unique ID of this class of entities.
    static foundation::UniqueID get_class_uid();

    // Constructor.
    AOV(const char* name, const ParamArray& params);

    // Delete this instance.
    void release() override;

    // Return a string identifying the model of this entity.
    virtual const char* get_model() const = 0;

    // Return the number of channels of this AOV.
    virtual size_t get_channel_count() const = 0;

    // Return the AOV channel names.
    virtual const char** get_channel_names() const = 0;

    // Return true if this AOV contains color data.
    virtual bool has_color_data() const = 0;

    // Return a reference to the AOV image.
    foundation::Image& get_image() const;

    // Clear the AOV image to default values.
    virtual void clear_image() = 0;

    // Apply any post-processing needed to the AOV image.
    virtual void post_process_image(const Frame& frame);

    // Write image to OpenEXR file.
    virtual bool write_images(
        const char*                         file_path,
        const foundation::ImageAttributes&  image_attributes) const;

    // Fill tile with specific color to debug.
    void debug_fill(float r, float g, float b);

  protected:
    friend class AOVAccumulatorContainer;
    friend class Frame;

    foundation::Image*  m_image;
    size_t              m_image_index;

    // Create an image to store the AOV result.
    virtual void create_image(
        const size_t    canvas_width,
        const size_t    canvas_height,
        const size_t    tile_width,
        const size_t    tile_height,
        ImageStack&     aov_images);

    // Create an accumulator for this AOV.
    virtual foundation::auto_release_ptr<AOVAccumulator> create_accumulator() const = 0;
};


//
// Color AOV base class.
//

class ColorAOV
  : public AOV
{
  public:
    // Constructor.
    ColorAOV(const char* name, const ParamArray& params);

    // Return the number of channels of this AOV.
    size_t get_channel_count() const override;

    // Return the AOV channel names.
    const char** get_channel_names() const override;

    // Return true if this AOV contains color data.
    bool has_color_data() const override;

    // Clear the AOV image to default values.
    void clear_image() override;
};


//
// Unfiltered AOV base class.
//

class UnfilteredAOV
  : public AOV
{
  public:
    // Constructor.
    UnfilteredAOV(const char* name, const ParamArray& params);

    // Destructor.
    ~UnfilteredAOV() override;

    // Return the number of channels of this AOV.
    size_t get_channel_count() const override;

    // Return the AOV channel names.
    const char** get_channel_names() const override;

    // Return true if this AOV contains color data.
    bool has_color_data() const override;

    // Clear the AOV image to default values.
    void clear_image() override;

  protected:
    void create_image(
        const size_t    canvas_width,
        const size_t    canvas_height,
        const size_t    tile_width,
        const size_t    tile_height,
        ImageStack&     aov_images) override;
};

}   // namespace renderer
