
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2018 Francois Beaune, The appleseedhq Organization
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
#include "settingsparsing.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/platform/system.h"
#include "foundation/string/string.h"
#include "foundation/utility/makevector.h"

// Standard headers.
#include <algorithm>

using namespace foundation;

namespace renderer
{

std::string get_render_device(const ParamArray& params)
{
    std::vector<std::string> devices;
    devices.emplace_back("cpu");

    const std::string device =
        params.get_optional<std::string>(
            "device",
            "cpu",
            devices);

    return device;
}

Spectrum::Mode get_spectrum_mode(const ParamArray& params)
{
    const std::string spectrum_mode =
        params.get_required<std::string>(
            "spectrum_mode",
            "rgb",
            make_vector("rgb", "spectral"));

#ifdef APPLESEED_WITH_SPECTRAL_SUPPORT
    return
        spectrum_mode == "rgb"
            ? Spectrum::RGB
            : Spectrum::Spectral;
#else
    if (spectrum_mode == "spectral")
    {
        RENDERER_LOG_WARNING(
            "color pipeline set the \"spectral\" but spectral color support "
            "was not enabled when building appleseed; rgb will be used instead");
    }

    return Spectrum::RGB;
#endif
}

std::string get_spectrum_mode_name(const Spectrum::Mode mode)
{
#ifdef APPLESEED_WITH_SPECTRAL_SUPPORT
    switch (mode)
    {
      case Spectrum::RGB: return "rgb";
      case Spectrum::Spectral: return "spectral";
      default: return "unknown";
    }
#else
    return "rgb";
#endif
}

SamplingContext::Mode get_sampling_context_mode(const ParamArray& params)
{
    const std::string sampling_mode =
        params.get_required<std::string>(
            "sampling_mode",
            "qmc",
            make_vector("rng", "qmc"));

    return
        sampling_mode == "rng"
            ? SamplingContext::RNGMode
            : SamplingContext::QMCMode;
}

std::string get_sampling_context_mode_name(const SamplingContext::Mode mode)
{
    switch (mode)
    {
      case SamplingContext::RNGMode: return "rng";
      case SamplingContext::QMCMode: return "qmc";
      default: return "unknown";
    }
}

size_t get_rendering_thread_count(const ParamArray& params)
{
    const size_t core_count = System::get_logical_cpu_core_count();

    static const char* ThreadCountParameterName = "rendering_threads";

    if (!params.strings().exist(ThreadCountParameterName))
        return core_count;

    const std::string thread_count_str = params.strings().get<std::string>(ThreadCountParameterName);

    if (thread_count_str == "auto")
        return core_count;

    bool conversion_failed = false;
    size_t thread_count = 0; // zero is assigned to suppress -Wunitialized

    try
    {
        const int num_threads = from_string<int>(thread_count_str);
        if (num_threads < 0)
        {
            // If num_threads is negative, use all cores except -num_threads.
            thread_count = std::max(static_cast<int>(core_count) + num_threads, 1);
        }
        else
            thread_count = num_threads;
    }
    catch (const ExceptionStringConversionError&)
    {
        conversion_failed = true;
    }

    if (conversion_failed || thread_count == 0)
    {
        RENDERER_LOG_ERROR(
            "invalid value \"%s\" for parameter \"%s\", using default value \"%s\".",
            thread_count_str.c_str(),
            ThreadCountParameterName,
            "auto");

        return core_count;
    }

    return thread_count;
}

}   // namespace renderer
