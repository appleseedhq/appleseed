
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_MODELING_ENVIRONMENTEDF_ENVIRONMENTEDF_H
#define APPLESEED_RENDERER_MODELING_ENVIRONMENTEDF_ENVIRONMENTEDF_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/entity/connectableentity.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/math/vector.h"
#include "foundation/utility/uid.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class InputEvaluator; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Project; }
namespace renderer      { class ShadingContext; }

namespace renderer
{

//
// Environment Emittance Distribution Function (EDF).
//
// All direction vectors are unit-length and pointing toward the environment.
// All vectors are expressed in world space.
// All probability densities are measured with respect to solid angle.
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

    // Access the transform sequence of the environment EDF.
    TransformSequence& transform_sequence();
    const TransformSequence& transform_sequence() const;

    // This method is called once before rendering each frame.
    // Returns true on success, false otherwise.
    virtual bool on_frame_begin(
        const Project&              project,
        foundation::IAbortSwitch*   abort_switch = 0);

    // This method is called once after rendering each frame.
    virtual void on_frame_end(const Project& project);

    // Sample the EDF and compute the emission direction, its probability
    // density and the value of the EDF for this direction.
    virtual void sample(
        const ShadingContext&       shading_context,
        InputEvaluator&             input_evaluator,
        const foundation::Vector2d& s,                          // sample in [0,1)^2
        foundation::Vector3d&       outgoing,                   // world space emission direction, unit-length
        Spectrum&                   value,                      // EDF value for this direction
        double&                     probability) const = 0;     // PDF value

    // Evaluate the EDF for a given emission direction.
    virtual void evaluate(
        const ShadingContext&       shading_context,
        InputEvaluator&             input_evaluator,
        const foundation::Vector3d& outgoing,                   // world space emission direction, unit-length
        Spectrum&                   value) const = 0;           // EDF value for this direction
    virtual void evaluate(
        const ShadingContext&       shading_context,
        InputEvaluator&             input_evaluator,
        const foundation::Vector3d& outgoing,                   // world space emission direction, unit-length
        Spectrum&                   value,                      // EDF value for this direction
        double&                     probability) const = 0;     // PDF value

    // Evaluate the PDF for a given emission direction.
    virtual double evaluate_pdf(
        InputEvaluator&             input_evaluator,
        const foundation::Vector3d& outgoing) const = 0;        // world space emission direction, unit-length

  protected:
    TransformSequence m_transform_sequence;
};


//
// EnvironmentEDF class implementation.
//

inline TransformSequence& EnvironmentEDF::transform_sequence()
{
    return m_transform_sequence;
}

inline const TransformSequence& EnvironmentEDF::transform_sequence() const
{
    return m_transform_sequence;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_ENVIRONMENTEDF_ENVIRONMENTEDF_H
