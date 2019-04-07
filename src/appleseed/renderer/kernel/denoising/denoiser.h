
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

// BCD headers.
#include "bcd/DeepImage.h"

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace foundation    { class Image; }

namespace renderer
{

class DenoiserOptions
{
  public:
    float   m_histogram_patch_distance_threshold; //  histogram patch distance threshold
    size_t  m_patch_radius;                       //  patch has (1 + 2 x m_patchRadius)^2 pixels
    size_t  m_search_window_radius;               //  search windows (for neighbors) spreads across (1 + 2 x m_patchRadius)^2 pixels
    float   m_min_eigenvalue;                     //  minimum eigen value for matrix inversion
    bool    m_use_random_pixel_order;             //  true means the pixel will be processed in a random order; could be useful to remove some "grid" artifacts
    bool    m_prefilter_spikes;                   //  true means a spike removal prefiltering will be applied
    float   m_prefilter_threshold_stddev_factor;  //  see SpikeRemovalFilter::filter argument
    float   m_marked_pixels_skipping_probability; //  1 means the marked centers of the denoised patches will be skipped to accelerate a lot the computations
    size_t  m_num_scales;                         //  number of pyramid levels to use.
    size_t  m_num_cores;                          //  number of cores used to denoise. O means using all the cores available.
    bool    m_mark_invalid_pixels;

    DenoiserOptions()
      : m_histogram_patch_distance_threshold(1.0f)
      , m_patch_radius(1)
      , m_search_window_radius(6)
      , m_min_eigenvalue(1.e-8f)
      , m_use_random_pixel_order(false)
      , m_prefilter_spikes(false)
      , m_prefilter_threshold_stddev_factor(2.0f)
      , m_marked_pixels_skipping_probability(0.0f)
      , m_num_scales(3)
      , m_num_cores(0)
      , m_mark_invalid_pixels(false)
    {
    }
};

bool denoise_beauty_image(
    foundation::Image&          img,
    bcd::Deepimf&               num_samples,
    bcd::Deepimf&               histograms,
    bcd::Deepimf&               covariances,
    const DenoiserOptions&      options,
    foundation::IAbortSwitch*   abort_switch);

bool denoise_aov_image(
    foundation::Image&          img,
    const bcd::Deepimf&         num_samples,
    const bcd::Deepimf&         histograms,
    const bcd::Deepimf&         covariances,
    const DenoiserOptions&      options,
    foundation::IAbortSwitch*   abort_switch);

}   // namespace renderer
