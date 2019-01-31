
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Artem Bishev, The appleseedhq Organization
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

// appleseed.foundation headers.
#include "foundation/math/vector.h"
#include "foundation/utility/uid.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class Arena; }
namespace renderer      { class DistanceSample; }
namespace renderer      { class ParamArray; }
namespace renderer      { class ShadingContext; }
namespace renderer      { class ShadingRay; }

namespace renderer
{

class APPLESEED_DLLSYMBOL Volume
  : public ConnectableEntity
{
  public:
    // Return the unique ID of this class of entities.
    static foundation::UniqueID get_class_uid();

    // Constructor.
    Volume(
        const char*                 name,
        const ParamArray&           params);

    // Return a string identifying the model of this entity.
    virtual const char* get_model() const = 0;

    // Return true if this volume represents homogeneous media, else false.
    virtual bool is_homogeneous() const = 0;

    // Return the size in bytes to allocate for the input values of this volume
    // and its precomputed values, if any. By default, enough space is allocated
    // for the inputs alone, i.e. this returns get_inputs().compute_data_size().
    // If a volume stores additional data such as precomputed values in its input
    // block, it must override this method and return the correct size.
    // If evaluate_inputs() is overridden, then this method is irrelevant.
    virtual size_t compute_input_data_size() const;

    // Evaluate the inputs of this volume, if any.
    virtual void* evaluate_inputs(
        const ShadingContext&       shading_context,
        const ShadingRay&           volume_ray) const;

    // Precompute data based on already evaluated input values.
    virtual void prepare_inputs(
        foundation::Arena&          arena,
        const ShadingRay&           volume_ray,
        void*                       data) const;

    // Sample the distance before the next scattering event.
    virtual void sample(
        foundation::Arena&      arena,
        SamplingContext&        sampling_context,
        const void*             data,
        DistanceSample&         sample) const = 0;

    // Sample the distance before the next scattering event.
    virtual void evaluate(
        foundation::Arena&      arena,
        const void*             data,
        const double            distance,
        DistanceSample&         sample) const = 0;

    // Evaluate the transmission (spectrum) between the front end of the ray and a given point.
    virtual void evaluate_transmission(
        const void*                 data,                       // input values
        const ShadingRay&           volume_ray,                 // ray used for marching inside the volume
        const float                 distance,                   // distance to the point on this volume segment
        Spectrum&                   spectrum) const = 0;        // resulting spectrum

    // Evaluate the transmission (spectrum) of the entire ray.
    virtual void evaluate_transmission(
        const void*                 data,                       // input values
        const ShadingRay&           volume_ray,                 // ray used for marching inside the volume
        Spectrum&                   spectrum) const = 0;        // resulting spectrum
};

}   // namespace renderer
