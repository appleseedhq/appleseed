
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
#include "environmentedf.h"

// appleseed.foundation headers.
#include "foundation/utility/uid.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/source.h"

using namespace foundation;

namespace renderer
{

//
// EnvironmentEDF class implementation.
//

namespace
{
    const UniqueID g_class_uid = new_guid();
}

EnvironmentEDF::EnvironmentEDF(
    const char*         name,
    const ParamArray&   params)
  : ConnectableEntity(g_class_uid, params)
{
    set_name(name);
}

bool EnvironmentEDF::on_frame_begin(const Project& project)
{
    return true;
}

void EnvironmentEDF::on_frame_end(const Project& project)
{
}

void EnvironmentEDF::check_uniform_input(const char* input_name) const
{
    if (!m_inputs.source(input_name)->is_uniform())
    {
        RENDERER_LOG_ERROR(
            "the \"%s\" input of a \"%s\" must be bound to a scalar or a color.",
            input_name,
            get_model());
    }
}

void EnvironmentEDF::check_exitance_input_non_null(
    const char*         exitance_input_name,
    const char*         multiplier_input_name) const
{
    if (is_exitance_input_null(exitance_input_name, multiplier_input_name))
        warn_exitance_input_null();
}

bool EnvironmentEDF::is_exitance_input_null(
    const char*         exitance_input_name,
    const char*         multiplier_input_name) const
{
    const Source* exitance_source = m_inputs.source(exitance_input_name);
    const Source* multiplier_source = m_inputs.source(multiplier_input_name);

    if (exitance_source->is_uniform())
    {
        Spectrum exitance;
        Alpha alpha;
        exitance_source->evaluate_uniform(exitance, alpha);

        if (exitance == Spectrum(0.0f))
            return true;
    }

    if (multiplier_source->is_uniform())
    {
        double multiplier;
        multiplier_source->evaluate_uniform(multiplier);

        if (multiplier == 0.0)
            return true;
    }

    return false;
}

void EnvironmentEDF::warn_exitance_input_null() const
{
    RENDERER_LOG_WARNING(
        "environment edf \"%s\" has a zero exitance and will slow down rendering "
        "without contributing to the lighting.",
        get_name());
}

}   // namespace renderer
