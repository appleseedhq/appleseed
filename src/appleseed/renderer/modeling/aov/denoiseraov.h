
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

#pragma once

// appleseed.renderer headers.
#include "renderer/modeling/aov/aov.h"

// appleseed.foundation headers.
#include "foundation/utility/autoreleaseptr.h"

// BCD headers.
#include "bcd/DeepImage.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace renderer      { class AOV; }
namespace renderer      { class ParamArray; }

namespace renderer
{

//
// Denoiser AOV.
//

class DenoiserAOV
  : public AOV
{
  public:
    ~DenoiserAOV() override;

    const char* get_model() const override;

    size_t get_channel_count() const override;

    const char** get_channel_names() const override;

    bool has_color_data() const override;

    void create_image(
        const size_t    canvas_width,
        const size_t    canvas_height,
        const size_t    tile_width,
        const size_t    tile_height,
        ImageStack&     aov_images) override;

    void clear_image() override;

    void fill_empty_samples() const;

    const bcd::Deepimf& histograms_image() const;
    bcd::Deepimf& histograms_image();

    void extract_num_samples_image(bcd::Deepimf& num_samples) const;
    void compute_covariances_image(bcd::Deepimf& covariances) const;

    bool write_images(const char* file_path) const override;

  protected:
    foundation::auto_release_ptr<AOVAccumulator> create_accumulator() const override;

  private:
    friend class DenoiserAOVFactory;

    struct Impl;
    Impl* impl;

    DenoiserAOV(
        const float  max_hist_value,
        const size_t num_bins);
};


//
// A factory for denoiser AOVs.
//

class DenoiserAOVFactory
{
  public:
    static foundation::auto_release_ptr<DenoiserAOV> create(
        const float  max_hist_value = 2.5f,
        const size_t num_bins = 20);
};

}   // namespace renderer
