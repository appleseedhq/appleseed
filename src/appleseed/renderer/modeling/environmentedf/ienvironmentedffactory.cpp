
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
#include "ienvironmentedffactory.h"

// appleseed.foundation headers.
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"

using namespace foundation;

namespace renderer
{

void IEnvironmentEDFFactory::add_common_sky_input_metadata(DictionaryArray& metadata)
{
    metadata.push_back(
        Dictionary()
            .insert("name", "sun_theta")
            .insert("label", "Sun Theta Angle")
            .insert("type", "numeric")
            .insert("min_value", "0.0")
            .insert("max_value", "90.0")
            .insert("use", "required")
            .insert("default", "45.0")
            .insert("help", "Sun polar (vertical) angle in degrees"));

    metadata.push_back(
        Dictionary()
            .insert("name", "sun_phi")
            .insert("label", "Sun Phi Angle")
            .insert("type", "numeric")
            .insert("min_value", "-360.0")
            .insert("max_value", "360.0")
            .insert("use", "required")
            .insert("default", "0.0")
            .insert("help", "Sun azimuthal (horizontal) angle in degrees"));

    metadata.push_back(
        Dictionary()
            .insert("name", "turbidity")
            .insert("label", "Turbidity")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "1.0")
            .insert("help", "Atmospheric haziness"));

    metadata.push_back(
        Dictionary()
            .insert("name", "turbidity_multiplier")
            .insert("label", "Turbidity Multiplier")
            .insert("type", "numeric")
            .insert("min_value", "0.0")
            .insert("max_value", "8.0")
            .insert("use", "optional")
            .insert("default", "2.0")
            .insert("help", "Atmospheric haziness multiplier"));

    metadata.push_back(
        Dictionary()
            .insert("name", "luminance_multiplier")
            .insert("label", "Luminance Multiplier")
            .insert("type", "numeric")
            .insert("min_value", "0.0")
            .insert("max_value", "10.0")
            .insert("use", "optional")
            .insert("default", "1.0")
            .insert("help", "Sky luminance multiplier"));

    metadata.push_back(
        Dictionary()
            .insert("name", "luminance_gamma")
            .insert("label", "Luminance Gamma")
            .insert("type", "numeric")
            .insert("min_value", "0.0")
            .insert("max_value", "3.0")
            .insert("use", "optional")
            .insert("default", "1.0")
            .insert("help", "Sky luminance gamma"));

    metadata.push_back(
        Dictionary()
            .insert("name", "saturation_multiplier")
            .insert("label", "Saturation Multiplier")
            .insert("type", "numeric")
            .insert("min_value", "0.0")
            .insert("max_value", "10.0")
            .insert("use", "optional")
            .insert("default", "1.0")
            .insert("help", "Sky color saturation multiplier"));

    metadata.push_back(
        Dictionary()
            .insert("name", "horizon_shift")
            .insert("label", "Horizon Shift")
            .insert("type", "text")
            .insert("use", "optional")
            .insert("default", "0.0")
            .insert("help", "Rotate the sky horizontally by a given number of degrees"));
}

}   // namespace renderer
