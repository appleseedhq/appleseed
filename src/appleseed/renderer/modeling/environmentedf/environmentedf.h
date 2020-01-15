
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

#pragma once

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/entity/connectableentity.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/uid.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class BaseGroup; }
namespace renderer      { class OnFrameBeginRecorder; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Project; }
namespace renderer      { class ShadingContext; }

namespace renderer
{

//
// Environment Emittance Distribution Function (EDF).
//
// Conventions:
//
//   * All direction vectors are expressed in world space.
//
//   * All direction vectors are unit-length and pointing toward the environment.
//
//   * All probability densities are measured with respect to solid angle.
//

class APPLESEED_DLLSYMBOL EnvironmentEDF
  : public ConnectableEntity
{
  public:
    // Return the unique ID of this class of entities.
    static foundation::UniqueID get_class_uid();

    // Constructor.
    EnvironmentEDF(
        const char*                 name,
        const ParamArray&           params);

    // Return a string identifying the model of this entity.
    virtual const char* get_model() const = 0;

    enum Flags
    {
        CastShadows = 1UL << 0      // does this environment cast shadows?
    };

    // Retrieve the flags.
    int get_flags() const;

    // Access the transform sequence of the environment EDF.
    TransformSequence& transform_sequence();
    const TransformSequence& transform_sequence() const;

    bool on_frame_begin(
        const Project&              project,
        const BaseGroup*            parent,
        OnFrameBeginRecorder&       recorder,
        foundation::IAbortSwitch*   abort_switch = nullptr) override;

    // Sample the EDF and compute the emission direction, its probability
    // density and the value of the EDF for this direction.
    virtual void sample(
        const ShadingContext&       shading_context,
        const foundation::Vector2f& s,                          // sample in [0,1)^2
        foundation::Vector3f&       outgoing,                   // world space emission direction, unit-length
        Spectrum&                   value,                      // EDF value for this direction
        float&                      probability) const = 0;     // PDF value

    // Evaluate the EDF for a given emission direction.
    virtual void evaluate(
        const ShadingContext&       shading_context,
        const foundation::Vector3f& outgoing,                   // world space emission direction, unit-length
        Spectrum&                   value) const = 0;           // EDF value for this direction
    virtual void evaluate(
        const ShadingContext&       shading_context,
        const foundation::Vector3f& outgoing,                   // world space emission direction, unit-length
        Spectrum&                   value,                      // EDF value for this direction
        float&                      probability) const = 0;     // PDF value

    // Evaluate the PDF for a given emission direction.
    virtual float evaluate_pdf(
        const foundation::Vector3f& outgoing) const = 0;        // world space emission direction, unit-length

  protected:
    int                 m_flags;
    TransformSequence   m_transform_sequence;
};


//
// EnvironmentEDF class implementation.
//

inline int EnvironmentEDF::get_flags() const
{
    return m_flags;
}

inline TransformSequence& EnvironmentEDF::transform_sequence()
{
    return m_transform_sequence;
}

inline const TransformSequence& EnvironmentEDF::transform_sequence() const
{
    return m_transform_sequence;
}

}   // namespace renderer
