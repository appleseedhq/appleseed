
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2017 Esteban Tovagliari, The appleseedhq Organization
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
#include "ilightingengine.h"

// appleseed.foundation headers.
#include "foundation/utility/containers/dictionary.h"

using namespace foundation;

namespace renderer
{

void ILightingEngineFactory::add_common_params_metadata(
    Dictionary& metadata,
    const bool  add_lighting_samples)
{
    metadata.dictionaries().insert(
        "enable_ibl",
        Dictionary()
            .insert("type", "bool")
            .insert("default", "on")
            .insert("label", "Enable IBL")
            .insert("help", "Enable image-based lighting"));

    if (add_lighting_samples)
    {
        metadata.dictionaries().insert(
            "dl_light_samples",
            Dictionary()
                .insert("type", "float")
                .insert("default", "1.0")
                .insert("label", "Light Samples")
                .insert("help", "Number of samples used to estimate direct lighting"));

        metadata.dictionaries().insert(
            "dl_low_light_threshold",
            Dictionary()
                .insert("type", "float")
                .insert("default", "0.0")
                .insert("label", "Low Light Threshold")
                .insert("help", "Light contribution threshold to disable shadow rays"));

        metadata.dictionaries().insert(
            "ibl_env_samples",
            Dictionary()
                .insert("type", "float")
                .insert("default", "1.0")
                .insert("label", "IBL Samples")
                .insert("help", "Number of samples used to estimate environment lighting"));
    }
}

}       // namespace renderer
