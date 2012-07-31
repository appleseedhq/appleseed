
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
#include "edf.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/source.h"

// appleseed.foundation headers.
#include "foundation/utility/uid.h"

using namespace foundation;

namespace renderer
{

//
// EDF class implementation.
//

namespace
{
    const UniqueID g_class_uid = new_guid();
}

EDF::EDF(
    const char*         name,
    const ParamArray&   params)
  : ConnectableEntity(g_class_uid, params)
{
    set_name(name);
}

void EDF::on_frame_begin(
    const Project&      project,
    const Assembly&     assembly)
{
}

void EDF::on_frame_end(
    const Project&      project,
    const Assembly&     assembly)
{
}

void EDF::check_non_null_exitance_input(
    const char*         exitance_input_name,
    const char*         multiplier_input_name) const
{
    bool zero_exitance = false;
    const Source* exitance_source = m_inputs.source(exitance_input_name);

    if (exitance_source->is_uniform())
    {
        Spectrum exitance;
        Alpha alpha;
        exitance_source->evaluate_uniform(exitance, alpha);
        zero_exitance = exitance == Spectrum(0.0f);
    }

    bool zero_multiplier = false;
    const Source* multiplier_source = m_inputs.source(multiplier_input_name);

    if (multiplier_source->is_uniform())
    {
        double multiplier;
        multiplier_source->evaluate_uniform(multiplier);
        zero_multiplier = multiplier == 0.0;
    }

    if (zero_exitance || zero_multiplier)
    {
        RENDERER_LOG_WARNING(
            "edf \"%s\" has a zero exitance and will slow down rendering "
            "without contributing to the lighting.",
            get_name());
    }
}

}   // namespace renderer
