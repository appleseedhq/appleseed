
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
#include "denoiser.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/utility/job/iabortswitch.h"

// BCD headers.
#include "bcd/DeepImage.h"
#include "bcd/Denoiser.h"
#include "bcd/IDenoiser.h"
#include "bcd/MultiscaleDenoiser.h"
#include "bcd/SpikeRemovalFilter.h"
#include "bcd/Utils.h"

// Standard headers.
#include <cmath>
#include <memory>
#include <vector>

using namespace bcd;
using namespace foundation;

namespace renderer
{

namespace
{

    void image_to_deepimage(const Image& src, Deepimf& dst)
    {
        const CanvasProperties& src_props = src.properties();
        assert(src_props.m_channel_count == 4);

        dst.resize(
            static_cast<int>(src_props.m_canvas_width),
            static_cast<int>(src_props.m_canvas_height),
            3);

        for (size_t j = 0; j < src_props.m_canvas_height; ++j)
        {
            for (size_t i = 0; i < src_props.m_canvas_width; ++i)
            {
                Color4f c;
                src.get_pixel(i, j, c);
                c.unpremultiply_in_place();

                dst.set(static_cast<int>(j), static_cast<int>(i), 0, c[0]);
                dst.set(static_cast<int>(j), static_cast<int>(i), 1, c[1]);
                dst.set(static_cast<int>(j), static_cast<int>(i), 2, c[2]);
            }
        }
    }

    void deepimage_to_image(const Deepimf& src, Image& dst)
    {
        const CanvasProperties& dst_props = dst.properties();

        assert(src.getWidth() == dst_props.m_canvas_width);
        assert(src.getHeight() == dst_props.m_canvas_height);
        assert(src.getDepth() == 3);
        assert(dst_props.m_channel_count == 4);

        for (size_t j = 0; j < dst_props.m_canvas_height; ++j)
        {
            for (size_t i = 0; i < dst_props.m_canvas_width; ++i)
            {
                Color4f c;
                dst.get_pixel(i, j, c);

                c[0] = src.get(static_cast<int>(j), static_cast<int>(i), 0);
                c[1] = src.get(static_cast<int>(j), static_cast<int>(i), 1);
                c[2] = src.get(static_cast<int>(j), static_cast<int>(i), 2);

                c.premultiply_in_place();
                dst.set_pixel(i, j, c);
            }
        }
    }

    class DenoiserCallbacks
      : public ICallbacks
    {
      public:
        explicit DenoiserCallbacks(IAbortSwitch* abort_switch)
          : m_abort_switch(abort_switch)
        {
        }

        void progress(const float p) const override
        {
        }

        bool isAborted() const override
        {
            if (m_abort_switch)
                return m_abort_switch->is_aborted();

            return false;
        }

      private:
        void logInfo(const char* msg) const override
        {
            RENDERER_LOG_INFO("%s", msg);
        }

        void logWarning(const char* msg) const override
        {
            RENDERER_LOG_WARNING("%s", msg);
        }

        void logError(const char* msg) const override
        {
            RENDERER_LOG_ERROR("%s", msg);
        }

        void logDebug(const char* msg) const override
        {
            RENDERER_LOG_DEBUG("%s", msg);
        }

        IAbortSwitch* m_abort_switch;
    };

    bool do_denoise_image(
        Deepimf&                src,
        const Deepimf&          num_samples,
        const Deepimf&          histograms,
        const Deepimf&          covariances,
        const DenoiserOptions&  options,
        IAbortSwitch*           abort_switch,
        Deepimf&                dst)
    {
        DenoiserInputs inputs;
        inputs.m_pColors = &src;
        inputs.m_pNbOfSamples = &num_samples;
        inputs.m_pHistograms = &histograms;
        inputs.m_pSampleCovariances = &covariances;

        DenoiserParameters parameters;
        parameters.m_histogramDistanceThreshold = options.m_histogram_patch_distance_threshold;
        parameters.m_patchRadius = static_cast<int>(options.m_patch_radius);
        parameters.m_searchWindowRadius = static_cast<int>(options.m_search_window_radius);
        parameters.m_minEigenValue = options.m_min_eigenvalue;
        parameters.m_useRandomPixelOrder = options.m_use_random_pixel_order;
        parameters.m_markedPixelsSkippingProbability = options.m_marked_pixels_skipping_probability;
        parameters.m_nbOfCores = static_cast<int>(options.m_num_cores);
        parameters.m_markInvalidPixels = options.m_mark_invalid_pixels;

        DenoiserOutputs outputs;
        outputs.m_pDenoisedColors = &dst;

        std::unique_ptr<IDenoiser> denoiser;

        if (options.m_num_scales > 1)
            denoiser.reset(new MultiscaleDenoiser(static_cast<int>(options.m_num_scales)));
        else
            denoiser.reset(new Denoiser());

        DenoiserCallbacks callbacks(abort_switch);
        denoiser->setCallbacks(&callbacks);

        denoiser->setInputs(inputs);
        denoiser->setOutputs(outputs);
        denoiser->setParameters(parameters);

        if (!denoiser->inputsOutputsAreOk())
            return false;

        return denoiser->denoise();
    }

}

bool denoise_beauty_image(
    Image&                  img,
    Deepimf&                num_samples,
    Deepimf&                histograms,
    Deepimf&                covariances,
    const DenoiserOptions&  options,
    IAbortSwitch*           abort_switch)
{
    Deepimf src;
    image_to_deepimage(img, src);

    if (options.m_prefilter_spikes)
    {
        SpikeRemovalFilter::filter(
            src,
            num_samples,
            histograms,
            covariances,
            options.m_prefilter_threshold_stddev_factor);
    }

    Deepimf dst(src);

    const bool success =
        do_denoise_image(
            src,
            num_samples,
            histograms,
            covariances,
            options,
            abort_switch,
            dst);

    if (success)
        deepimage_to_image(dst, img);

    return success;
}

bool denoise_aov_image(
    Image&                  img,
    const Deepimf&          num_samples,
    const Deepimf&          histograms,
    const Deepimf&          covariances,
    const DenoiserOptions&  options,
    IAbortSwitch*           abort_switch)
{
    Deepimf src;
    image_to_deepimage(img, src);

    if (options.m_prefilter_spikes)
    {
        SpikeRemovalFilter::filter(
            src,
            options.m_prefilter_threshold_stddev_factor);
    }

    Deepimf dst(src);

    const bool success =
        do_denoise_image(
            src,
            num_samples,
            histograms,
            covariances,
            options,
            abort_switch,
            dst);

    if (success)
        deepimage_to_image(dst, img);

    return success;
}

}   // namespace renderer
