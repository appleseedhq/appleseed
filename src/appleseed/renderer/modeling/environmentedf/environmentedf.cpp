
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "foundation/math/transform.h"
#include "foundation/utility/api/apistring.h"

// Standard headers.
#include <cstddef>

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

UniqueID EnvironmentEDF::get_class_uid()
{
    return g_class_uid;
}

EnvironmentEDF::EnvironmentEDF(
    const char*             name,
    const ParamArray&       params)
  : ConnectableEntity(g_class_uid, params)
  , m_flags(0)
{
    set_name(name);
}

bool EnvironmentEDF::on_frame_begin(
    const Project&          project,
    const BaseGroup*        parent,
    OnFrameBeginRecorder&   recorder,
    IAbortSwitch*           abort_switch)
{
    if (!ConnectableEntity::on_frame_begin(project, parent, recorder, abort_switch))
        return false;

    if (m_params.get_optional<bool>("cast_shadows", true))
        m_flags |= CastShadows;
    else
        m_flags &= ~CastShadows;

    // Make sure the environment EDF's transform is only a (sequence of) rotation.
    bool warned = false;
    for (size_t i = 0, e = m_transform_sequence.size(); i < e; ++i)
    {
        // Retrieve transform.
        float time;
        Transformd transform;
        m_transform_sequence.get_transform(i, time, transform);

        // Decompose transform.
        Vector3d scaling, translation;
        Quaterniond rotation;
        transform.get_local_to_parent().decompose(scaling, rotation, translation);

        // Check transform validity.
        const bool is_valid_scaling =
            (feq(scaling[0], +1.0) || feq(scaling[0], -1.0)) &&
            (feq(scaling[1], +1.0) || feq(scaling[1], -1.0)) &&
            (feq(scaling[2], +1.0) || feq(scaling[2], -1.0));
        const bool is_valid_translation = fz(translation);
        const bool is_valid_rotation = is_normalized(rotation);
        const bool is_valid_transform = is_valid_scaling && is_valid_translation && is_valid_rotation;

        // Warn once about invalid transforms.
        if (!is_valid_transform && !warned)
        {
            RENDERER_LOG_WARNING(
                "transforms of environment edf \"%s\" must be pure rotations and flips but have scaling and/or translation components; these will be ignored.",
                get_path().c_str());
            warned = true;
        }

        // Rebuild transform.
        scaling[0] = scaling[0] < 0.0 ? -1.0 : +1.0;
        scaling[1] = scaling[1] < 0.0 ? -1.0 : +1.0;
        scaling[2] = scaling[2] < 0.0 ? -1.0 : +1.0;
        rotation = normalize(rotation);
        transform =
            Transformd::from_local_to_parent(
                Matrix4d::make_rotation(rotation) *
                Matrix4d::make_scaling(scaling));

        // Store transform.
        m_transform_sequence.set_transform(time, transform);
    }

    return true;
}

}   // namespace renderer
