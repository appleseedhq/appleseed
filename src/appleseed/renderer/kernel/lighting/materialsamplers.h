
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
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/modeling/bsdf/bsdf.h"

// appleseed.foundation headers.
#include "foundation/math/dual.h"
#include "foundation/math/vector.h"

// Forward declarations.
namespace renderer  { class DirectShadingComponents; }
namespace renderer  { class ShadingContext; }
namespace renderer  { class ShadingPoint; }

//
// This file contains wrappers over samplable entities, such as BSDFs and Volumes.
// These wrappers provide a uniform interface for sampling and evaluating these entities
// at a certain point, which can be used during lighting integration.
//

namespace renderer
{

class IMaterialSampler
{
  public:
    virtual ~IMaterialSampler() {}

    virtual const foundation::Vector3d& get_point() const = 0;

    virtual const ShadingPoint& get_shading_point() const = 0;

    virtual bool contributes_to_light_sampling() const = 0;

    virtual const ShadingPoint& trace_full(
        const ShadingContext&           shading_context,
        const foundation::Vector3f&     direction,
        Spectrum&                       transmission) const = 0;

    virtual void trace_simple(
        const ShadingContext&           shading_context,
        const foundation::Vector3f&     direction,
        Spectrum&                       transmission) const = 0;

    virtual void trace_between(
        const ShadingContext&           shading_context,
        const foundation::Vector3d&     target_position,
        Spectrum&                       transmission) const = 0;

    virtual bool sample(
        SamplingContext&                sampling_context,
        const foundation::Dual3d&       outgoing,
        foundation::Dual3f&             incoming,
        DirectShadingComponents&        value,
        float&                          pdf) const = 0;

    virtual float evaluate(
        const foundation::Vector3f&     outgoing,
        const foundation::Vector3f&     incoming,
        const int                       light_sampling_modes,
        DirectShadingComponents&        value) const = 0;
};

class BSDFSampler
  : public IMaterialSampler
{
  public:
    BSDFSampler(
        const BSDF&                     bsdf,
        const void*                     bsdf_data,
        const int                       bsdf_sampling_modes,
        const ShadingPoint&             shading_point);

    const foundation::Vector3d& get_point() const override;

    const ShadingPoint& get_shading_point() const override;

    bool contributes_to_light_sampling() const override;

    const ShadingPoint& trace_full(
        const ShadingContext&           shading_context,
        const foundation::Vector3f&     direction,
        Spectrum&                       transmission) const override;

    void trace_simple(
        const ShadingContext&           shading_context,
        const foundation::Vector3f&     direction,
        Spectrum&                       transmission) const override;

    void trace_between(
        const ShadingContext&           shading_context,
        const foundation::Vector3d&     target_position,
        Spectrum&                       transmission) const override;

    bool sample(
        SamplingContext&                sampling_context,
        const foundation::Dual3d&       outgoing,
        foundation::Dual3f&             incoming,
        DirectShadingComponents&        value,
        float&                          pdf) const override;

    float evaluate(
        const foundation::Vector3f&     outgoing,
        const foundation::Vector3f&     incoming,
        const int                       light_sampling_modes,
        DirectShadingComponents&        value) const override;

  private:
    const BSDF&                         m_bsdf;
    const void*                         m_bsdf_data;
    const int                           m_bsdf_sampling_modes;
    const ShadingPoint&                 m_shading_point;
    BSDF::LocalGeometry                 m_local_geometry;
};

class VolumeSampler
  : public IMaterialSampler
{
  public:
    VolumeSampler(
        const ShadingRay&               volume_ray,
        const Volume&                   volume,
        const void*                     volume_data,
        const float                     distance,
        const ShadingPoint&             shading_point);

    const foundation::Vector3d& get_point() const override;

    const ShadingPoint& get_shading_point() const override;

    bool contributes_to_light_sampling() const override;

    const ShadingPoint& trace_full(
        const ShadingContext&           shading_context,
        const foundation::Vector3f&     direction,
        Spectrum&                       transmission) const override;

    void trace_simple(
        const ShadingContext&           shading_context,
        const foundation::Vector3f&     direction,
        Spectrum&                       transmission) const override;

    void trace_between(
        const ShadingContext&           shading_context,
        const foundation::Vector3d&     target_position,
        Spectrum&                       transmission) const override;

    bool sample(
        SamplingContext&                sampling_context,
        const foundation::Dual3d&       outgoing,
        foundation::Dual3f&             incoming,
        DirectShadingComponents&        value,
        float&                          pdf) const override;

    float evaluate(
        const foundation::Vector3f&     outgoing,
        const foundation::Vector3f&     incoming,
        const int                       light_sampling_modes,
        DirectShadingComponents&        value) const override;

  private:
    const ShadingRay&                   m_volume_ray;
    const Volume&                       m_volume;
    const void*                         m_volume_data;
    const float                         m_distance;
    const ShadingPoint&                 m_shading_point;
    const foundation::Vector3d          m_point;
};

}   // namespace renderer
