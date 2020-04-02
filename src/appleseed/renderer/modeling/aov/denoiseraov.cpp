
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

// Interface header.
#include "denoiseraov.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/aov/aovaccumulator.h"
#include "renderer/kernel/rendering/pixelcontext.h"
#include "renderer/kernel/shading/shadingcomponents.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/aov/aov.h"
#include "renderer/modeling/color/colorspace.h"
#include "renderer/modeling/frame/frame.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/platform/defaulttimers.h"
#include "foundation/string/string.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/stopwatch.h"

// BCD headers.
#include "bcd/CovarianceMatrix.h"
#include "bcd/ImageIO.h"

// Boost headers.
#include "boost/filesystem.hpp"

// Standard headers.
#include <cmath>
#include <cstddef>

using namespace bcd;
using namespace foundation;
namespace bf = boost::filesystem;

namespace renderer
{

namespace
{
    //
    // Denoiser AOV accumulator.
    //

    class DenoiserAOVAccumulator
      : public AOVAccumulator
    {
      public:
        DenoiserAOVAccumulator(
            const size_t   num_bins,
            const float    gamma,
            const float    max_value,
            Deepimf&       sum_accum,
            Deepimf&       covariance_accum,
            Deepimf&       histograms)
          : m_num_bins(num_bins)
          , m_gamma(gamma)
          , m_rcp_gamma(1.0f / gamma)
          , m_max_value(max_value)
          , m_samples_channel_index(3 * num_bins)
          , m_sum_accum(sum_accum)
          , m_covariance_accum(covariance_accum)
          , m_histograms(histograms)
        {
        }

        void on_tile_begin(
            const Frame&                frame,
            const size_t                tile_x,
            const size_t                tile_y,
            const size_t                max_spp) override
        {
            // Fetch the destination tile.
            const CanvasProperties& props = frame.image().properties();
            const Tile& tile = frame.image().tile(tile_x, tile_y);

            // Fetch the tile bounds (inclusive).
            m_tile_origin_x = static_cast<int>(tile_x * props.m_tile_width);
            m_tile_origin_y = static_cast<int>(tile_y * props.m_tile_height);
            m_tile_end_x = static_cast<int>(m_tile_origin_x + tile.get_width() - 1);
            m_tile_end_y = static_cast<int>(m_tile_origin_y + tile.get_height() - 1);
        }

        void on_sample_begin(
            const PixelContext&         pixel_context) override
        {
            m_accum.set(0.0f);
            m_sample_count = 0;
        }

        void on_sample_end(
            const PixelContext&         pixel_context) override
        {
            // Ignore invalid samples.
            if (m_sample_count == 0)
                return;

            const Vector2i& pi = pixel_context.get_pixel_coords();

            // Ignore samples outside the tile.
            if (outside_tile(pi))
                return;

            // Accumulate unpremultiplied samples.
            m_accum.unpremultiply_in_place();

            // Update the num samples channel.
            m_histograms.get(pi.y, pi.x, static_cast<int>(m_samples_channel_index)) += 1.0f;

            // Update the sum and covariance accumulator.
            m_sum_accum.get(pi.y, pi.x, 0) += m_accum.r;
            m_sum_accum.get(pi.y, pi.x, 1) += m_accum.g;
            m_sum_accum.get(pi.y, pi.x, 2) += m_accum.b;

            const size_t c_xx = static_cast<size_t>(ESymmetricMatrix3x3Data::e_xx);
            const size_t c_yy = static_cast<size_t>(ESymmetricMatrix3x3Data::e_yy);
            const size_t c_zz = static_cast<size_t>(ESymmetricMatrix3x3Data::e_zz);
            const size_t c_yz = static_cast<size_t>(ESymmetricMatrix3x3Data::e_yz);
            const size_t c_xz = static_cast<size_t>(ESymmetricMatrix3x3Data::e_xz);
            const size_t c_xy = static_cast<size_t>(ESymmetricMatrix3x3Data::e_xy);

            m_covariance_accum.get(pi.y, pi.x, c_xx) += m_accum.r * m_accum.r;
            m_covariance_accum.get(pi.y, pi.x, c_yy) += m_accum.g * m_accum.g;
            m_covariance_accum.get(pi.y, pi.x, c_zz) += m_accum.b * m_accum.b;
            m_covariance_accum.get(pi.y, pi.x, c_yz) += m_accum.g * m_accum.b;
            m_covariance_accum.get(pi.y, pi.x, c_xz) += m_accum.r * m_accum.b;
            m_covariance_accum.get(pi.y, pi.x, c_xy) += m_accum.r * m_accum.g;

            // Fill histogram: code from BCD's SampleAccumulator class.
            for (size_t c = 0; c < 3; ++c)
            {
                const size_t start_bin = c * m_num_bins;
                float value = m_accum[c];

                // Clamp to 0.
                value = std::max(value, 0.0f);

                // Exponential scaling.
                if (m_gamma > 1.0f) value = std::pow(value, m_rcp_gamma);

                // Normalize to the maximum value.
                if (m_max_value > 0.0f)
                    value /= m_max_value;

                // Used for determining the weight to give to the sample
                // in the highest two bins, when the sample is saturated.
                const float sature_level_gamma = 2.0f;
                value = std::min(value, sature_level_gamma);

                const float bin_float_index = value * (m_num_bins - 2);

                size_t floor_bin_index = truncate<size_t>(bin_float_index);
                size_t ceil_bin_index;

                float floor_bin_weight;
                float ceil_bin_weight;

                if (floor_bin_index < m_num_bins - 2)
                {
                    // In bounds.
                    ceil_bin_index = floor_bin_index + 1;
                    ceil_bin_weight = bin_float_index - floor_bin_index;
                    floor_bin_weight = 1.0f - ceil_bin_weight;
                }
                else
                {
                    // Out of bounds... v >= 1.
                    floor_bin_index = m_num_bins - 2;
                    ceil_bin_index = floor_bin_index + 1;
                    ceil_bin_weight = (value - 1.0f) / (sature_level_gamma - 1.f);
                    floor_bin_weight = 1.0f - ceil_bin_weight;
                }

                m_histograms.get(
                    pi.y,
                    pi.x,
                    static_cast<int>(start_bin + floor_bin_index)) += floor_bin_weight;

                m_histograms.get(
                    pi.y,
                    pi.x,
                    static_cast<int>(start_bin + ceil_bin_index)) += ceil_bin_weight;
            }
        }

        void write(
            const PixelContext&         pixel_context,
            const ShadingPoint&         shading_point,
            const ShadingComponents&    shading_components,
            const AOVComponents&        aov_components,
            ShadingResult&              shading_result) override
        {
            if (shading_result.is_main_valid())
            {
                // Composite over the previous sample.
                Color4f main = shading_result.m_main;
                main.premultiply_in_place();
                m_accum += (1.0f - m_accum.a) * shading_result.m_main;
                ++m_sample_count;
            }
        }

      private:
        Color4f         m_accum;
        size_t          m_sample_count;

        const size_t    m_num_bins;
        const float     m_gamma;
        const float     m_rcp_gamma;
        const float     m_max_value;
        const size_t    m_samples_channel_index;

        int             m_tile_origin_x;
        int             m_tile_origin_y;
        int             m_tile_end_x;
        int             m_tile_end_y;

        Deepimf&        m_sum_accum;
        Deepimf&        m_covariance_accum;

        Deepimf&        m_histograms;

        bool outside_tile(const Vector2i& pi) const
        {
            return
                pi.x < m_tile_origin_x ||
                pi.y < m_tile_origin_y ||
                pi.x > m_tile_end_x ||
                pi.y > m_tile_end_y;
        }
    };

    const char* DenoiserAOVModel = "denoiser_aov";
}


//
// Denoiser AOV class implementation.
//

struct DenoiserAOV::Impl
{
    size_t  m_num_bins;
    float   m_max_value;
    float   m_gamma;

    Deepimf m_sum_accum;
    Deepimf m_covariance_accum;

    Deepimf m_histograms;
};

DenoiserAOV::DenoiserAOV(
    const float  max_hist_value,
    const size_t num_bins)
  : AOV("denoiser", ParamArray())
  , impl(new Impl())
{
    impl->m_num_bins = num_bins;
    impl->m_max_value = max_hist_value;
    impl->m_gamma = 2.2f;
}

DenoiserAOV::~DenoiserAOV()
{
    delete impl;
}

const char* DenoiserAOV::get_model() const
{
    return DenoiserAOVModel;
}

size_t DenoiserAOV::get_channel_count() const
{
    return 0;
}

const char** DenoiserAOV::get_channel_names() const
{
    return nullptr;
}

bool DenoiserAOV::has_color_data() const
{
    return false;
}

void DenoiserAOV::create_image(
    const size_t    canvas_width,
    const size_t    canvas_height,
    const size_t    tile_width,
    const size_t    tile_height,
    ImageStack&     aov_images)
{
    const int w = static_cast<int>(canvas_width);
    const int h = static_cast<int>(canvas_height);
    const int bins = static_cast<int>(impl->m_num_bins);

    impl->m_sum_accum.resize(w, h, 3);
    impl->m_covariance_accum.resize(w, h, 6);
    impl->m_histograms.resize(w, h, 3 * bins + 1);

    clear_image();
}

void DenoiserAOV::clear_image()
{
    impl->m_sum_accum.fill(0.0f);
    impl->m_covariance_accum.fill(0.0f);
    impl->m_histograms.fill(0.0f);
}

void DenoiserAOV::fill_empty_samples() const
{
    const int w = impl->m_histograms.getWidth();
    const int h = impl->m_histograms.getHeight();

    const int num_bins = static_cast<int>(impl->m_num_bins);
    const int samples_channel_index = num_bins * 3;

    for (int y = 0; y < h; ++y)
    {
        for (int x = 0; x < w; ++x)
        {
            const float num_samples =
                impl->m_histograms.get(y, x, samples_channel_index);

            if (num_samples == 0.0f)
            {
                impl->m_histograms.get(y, x, 0) = 1.0f;
                impl->m_histograms.get(y, x, num_bins) = 1.0f;
                impl->m_histograms.get(y, x, num_bins * 2) = 1.0f;
                impl->m_histograms.get(y, x, samples_channel_index) = 1.0f;
            }
        }
    }
}

const Deepimf& DenoiserAOV::histograms_image() const
{
    return impl->m_histograms;
}

Deepimf& DenoiserAOV::histograms_image()
{
    return impl->m_histograms;
}

const Deepimf& DenoiserAOV::covariance_image() const
{
    return impl->m_covariance_accum;
}

Deepimf& DenoiserAOV::covariance_image()
{
    return impl->m_covariance_accum;
}

const Deepimf& DenoiserAOV::sum_image() const
{
    return impl->m_sum_accum;
}

Deepimf& DenoiserAOV::sum_image()
{
    return impl->m_sum_accum;
}

void DenoiserAOV::extract_num_samples_image(bcd::Deepimf& num_samples_image) const
{
    const int w = impl->m_histograms.getWidth();
    const int h = impl->m_histograms.getHeight();
    const int samples_channel_index = static_cast<int>(impl->m_num_bins * 3);

    num_samples_image.resize(w, h, 1);

    for (int y = 0; y < h; ++y)
    {
        for (int x = 0; x < w; ++x)
            num_samples_image.get(y, x, 0) = impl->m_histograms.get(y, x, samples_channel_index);
    }
}

void DenoiserAOV::compute_covariances_image(Deepimf& covariances_image) const
{
    const int w = impl->m_covariance_accum.getWidth();
    const int h = impl->m_covariance_accum.getHeight();

    covariances_image.resize(w, h, 6);
    covariances_image.fill(0.0f);

    const int samples_channel_index = static_cast<int>(impl->m_num_bins * 3);

    const size_t c_xx = static_cast<size_t>(ESymmetricMatrix3x3Data::e_xx);
    const size_t c_yy = static_cast<size_t>(ESymmetricMatrix3x3Data::e_yy);
    const size_t c_zz = static_cast<size_t>(ESymmetricMatrix3x3Data::e_zz);
    const size_t c_yz = static_cast<size_t>(ESymmetricMatrix3x3Data::e_yz);
    const size_t c_xz = static_cast<size_t>(ESymmetricMatrix3x3Data::e_xz);
    const size_t c_xy = static_cast<size_t>(ESymmetricMatrix3x3Data::e_xy);

    for (int y = 0; y < h; ++y)
    {
        for (int x = 0; x < w; ++x)
        {
            const float sample_count = impl->m_histograms.get(y, x, samples_channel_index);

            if (sample_count != 0.0f)
            {
                const float rcp_sample_count = 1.0f / sample_count;
                const float bias_correction_factor =
                    sample_count == 1.0f
                        ? 1.0f
                        : 1.0f / (1.0f - rcp_sample_count);

                // Compute the mean.
                float mean[3];
                for (int k = 0; k < 3; ++k)
                    mean[k] = impl->m_sum_accum.get(y, x, k) * rcp_sample_count;

                // Compute the covariances.
                const float xx = impl->m_covariance_accum.get(y, x, c_xx);
                const float yy = impl->m_covariance_accum.get(y, x, c_yy);
                const float zz = impl->m_covariance_accum.get(y, x, c_zz);
                const float yz = impl->m_covariance_accum.get(y, x, c_yz);
                const float xz = impl->m_covariance_accum.get(y, x, c_xz);
                const float xy = impl->m_covariance_accum.get(y, x, c_xy);

                covariances_image.get(y, x, c_xx) = (xx * rcp_sample_count - mean[0] * mean[0]) * bias_correction_factor;
                covariances_image.get(y, x, c_yy) = (yy * rcp_sample_count - mean[1] * mean[1]) * bias_correction_factor;
                covariances_image.get(y, x, c_zz) = (zz * rcp_sample_count - mean[2] * mean[2]) * bias_correction_factor;
                covariances_image.get(y, x, c_yz) = (yz * rcp_sample_count - mean[1] * mean[2]) * bias_correction_factor;
                covariances_image.get(y, x, c_xz) = (xz * rcp_sample_count - mean[0] * mean[2]) * bias_correction_factor;
                covariances_image.get(y, x, c_xy) = (xy * rcp_sample_count - mean[0] * mean[1]) * bias_correction_factor;
            }
        }
    }
}

bool DenoiserAOV::write_images(
    const char*             file_path,
    const ImageAttributes&  image_attributes) const
{
    fill_empty_samples();

    const bf::path boost_file_path(file_path);
    const bf::path directory = boost_file_path.parent_path();
    const std::string base_file_name = boost_file_path.stem().string();
    const std::string extension = boost_file_path.extension().string();

    bool success = true;

    Stopwatch<DefaultWallclockTimer> stopwatch;

    // Write histograms.
    stopwatch.start();
    const std::string hist_file_name = base_file_name + ".hist" + extension;
    const std::string hist_file_path = (directory / hist_file_name).string();
    if (ImageIO::writeMultiChannelsEXR(histograms_image(), hist_file_path.c_str()))
    {
        stopwatch.measure();
        RENDERER_LOG_INFO(
            "wrote image file %s for aov \"%s\" in %s.",
            hist_file_path.c_str(),
            get_path().c_str(),
            pretty_time(stopwatch.get_seconds()).c_str());
    }
    else
    {
        RENDERER_LOG_ERROR(
            "failed to write image file %s for aov \"%s\".",
            hist_file_path.c_str(),
            get_path().c_str());
        success = false;
    }

    // Compute covariances image.
    Deepimf covariances_image;
    compute_covariances_image(covariances_image);

    // Write covariances image.
    stopwatch.start();
    const std::string cov_file_name = base_file_name + ".cov" + extension;
    const std::string cov_file_path = (directory / cov_file_name).string();
    if (ImageIO::writeMultiChannelsEXR(covariances_image, cov_file_path.c_str()))
    {
        stopwatch.measure();
        RENDERER_LOG_INFO(
            "wrote image file %s for aov \"%s\" in %s.",
            cov_file_path.c_str(),
            get_path().c_str(),
            pretty_time(stopwatch.get_seconds()).c_str());
    }
    else
    {
        RENDERER_LOG_ERROR(
            "failed to write image file %s for aov \"%s\".",
            cov_file_path.c_str(),
            get_path().c_str());
        success = false;
    }

    return success;
}

auto_release_ptr<AOVAccumulator> DenoiserAOV::create_accumulator() const
{
    return auto_release_ptr<AOVAccumulator>(
        new DenoiserAOVAccumulator(
            impl->m_num_bins,
            impl->m_gamma,
            impl->m_max_value,
            impl->m_sum_accum,
            impl->m_covariance_accum,
            impl->m_histograms));
}


//
// DenoiserAOVFactory class implementation.
//

auto_release_ptr<DenoiserAOV> DenoiserAOVFactory::create(
    const float  max_hist_value,
    const size_t num_bins)
{
    return
        auto_release_ptr<DenoiserAOV>(
            new DenoiserAOV(max_hist_value, num_bins));
}

}   // namespace renderer
