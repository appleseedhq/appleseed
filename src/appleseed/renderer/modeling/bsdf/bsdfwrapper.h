
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
#include "renderer/kernel/shading/directshadingcomponents.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/bsdfsample.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/utility/shadowterminator.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/dual.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cmath>

// Forward declarations.
namespace renderer  { class ParamArray; }

namespace renderer
{

//
// The BSDFWrapper class wraps a BRDF or BTDF implementation with validity checks
// and takes care of correcting for the use of shading normals in the adjoint case.
//

template <typename BSDFImpl, bool Cull = true>
class BSDFWrapper
  : public BSDFImpl
{
  public:
    using LocalGeometry = typename BSDFImpl::LocalGeometry;

    BSDFWrapper(
        const char*                     name,
        const ParamArray&               params);

    void sample(
        SamplingContext&                sampling_context,
        const void*                     data,
        const bool                      adjoint,
        const bool                      cosine_mult,
        const LocalGeometry&            local_geometry,
        const foundation::Dual3f&       outgoing,
        const int                       modes,
        BSDFSample&                     sample) const override;

    float evaluate(
        const void*                     data,
        const bool                      adjoint,
        const bool                      cosine_mult,
        const LocalGeometry&            local_geometry,
        const foundation::Vector3f&     outgoing,
        const foundation::Vector3f&     incoming,
        const int                       modes,
        DirectShadingComponents&        value) const override;

    float evaluate_pdf(
        const void*                     data,
        const bool                      adjoint,
        const LocalGeometry&            local_geometry,
        const foundation::Vector3f&     outgoing,
        const foundation::Vector3f&     incoming,
        const int                       modes) const override;

  private:
    bool is_culled(
        const bool                      adjoint,
        const foundation::Basis3f&      shading_basis,
        const foundation::Vector3f&     outgoing,
        const foundation::Vector3f&     incoming) const;
};


//
// BSDFWrapper class implementation.
//

template <typename BSDFImpl, bool Cull>
BSDFWrapper<BSDFImpl, Cull>::BSDFWrapper(
    const char*                         name,
    const ParamArray&                   params)
  : BSDFImpl(name, params)
{
}

template <typename BSDFImpl, bool Cull>
void BSDFWrapper<BSDFImpl, Cull>::sample(
    SamplingContext&                    sampling_context,
    const void*                         data,
    const bool                          adjoint,
    const bool                          cosine_mult,
    const LocalGeometry&                local_geometry,
    const foundation::Dual3f&           outgoing,
    const int                           modes,
    BSDFSample&                         sample) const
{
    assert(foundation::is_normalized(local_geometry.m_geometric_normal));
    assert(foundation::is_normalized(outgoing.get_value()));

#ifndef NDEBUG
    // Save the sampling context at the beginning of the iteration.
    const SamplingContext backup_sampling_context(sampling_context);

    // Resume execution here to reliably reproduce problems downstream.
    sampling_context = backup_sampling_context;
#endif

    BSDFImpl::sample(
        sampling_context,
        data,
        adjoint,
        false,
        local_geometry,
        outgoing,
        modes,
        sample);

    if (sample.get_mode() != ScatteringMode::None)
    {
        assert(foundation::is_normalized(sample.m_incoming.get_value(), 1.0e-5f));
        assert(sample.m_value.is_valid());
        assert(sample.m_aov_components.is_valid());

        // Disabled until BSDF are evaluated in local space, because the numerous
        // conversions between local space and world space kill precision.
        //
        // #ifndef NDEBUG
        //         const float ref_probability =
        //             evaluate_pdf(
        //                 data,
        //                 adjoint,
        //                 local_geometry.m_geometric_normal,
        //                 local_geometry.m_shading_basis,
        //                 outgoing.get_value(),
        //                 sample.m_incoming.get_value(),
        //                 modes);
        //
        //         assert(
        //             (sample.m_probability == BSDFImpl::DiracDelta && ref_probability == 0.0f) ||
        //             (sample.m_probability > 0.0f && feq(sample.m_probability, ref_probability, 1.0e-2f)) ||
        //             (sample.m_probability > 0.0f && ref_probability == 0.0f));  // todo: this case is worrisome!
        // #endif

        if (cosine_mult)
        {
            if (adjoint)
            {
                const float cos_on = std::abs(foundation::dot(outgoing.get_value(), local_geometry.m_shading_basis.get_normal()));
                const float cos_ig = std::abs(foundation::dot(sample.m_incoming.get_value(), local_geometry.m_geometric_normal));
                const float cos_og = std::abs(foundation::dot(outgoing.get_value(), local_geometry.m_geometric_normal));
                sample.m_value *= cos_on * cos_ig / cos_og;
            }
            else
            {
                const float shadow_terminator_freq_mult = local_geometry.m_shading_point->get_object_instance().get_render_data().m_shadow_terminator_freq_mult;
                const float cos_in = std::min(std::abs(foundation::dot(sample.m_incoming.get_value(), local_geometry.m_shading_basis.get_normal())), 1.0f);
                sample.m_value *= shift_cos_in_fast(cos_in, shadow_terminator_freq_mult);
            }
        }
    }
}

template <typename BSDFImpl, bool Cull>
float BSDFWrapper<BSDFImpl, Cull>::evaluate(
    const void*                         data,
    const bool                          adjoint,
    const bool                          cosine_mult,
    const LocalGeometry&                local_geometry,
    const foundation::Vector3f&         outgoing,
    const foundation::Vector3f&         incoming,
    const int                           modes,
    DirectShadingComponents&            value) const
{
    assert(foundation::is_normalized(local_geometry.m_geometric_normal));
    assert(foundation::is_normalized(outgoing));
    assert(foundation::is_normalized(incoming));

    if (Cull && is_culled(adjoint, local_geometry.m_shading_basis, outgoing, incoming))
        return 0.0f;

    const float probability =
        BSDFImpl::evaluate(
            data,
            adjoint,
            false,
            local_geometry,
            outgoing,
            incoming,
            modes,
            value);
    assert(probability >= 0.0f);

    if (probability > 0.0f)
    {
        assert(value.is_valid());

        if (cosine_mult)
        {
            if (adjoint)
            {
                const float cos_on = std::abs(foundation::dot(outgoing, local_geometry.m_shading_basis.get_normal()));
                const float cos_ig = std::abs(foundation::dot(incoming, local_geometry.m_geometric_normal));
                const float cos_og = std::abs(foundation::dot(outgoing, local_geometry.m_geometric_normal));
                value *= cos_on * cos_ig / cos_og;
            }
            else
            {
                const float shadow_terminator_freq_mult = local_geometry.m_shading_point->get_object_instance().get_render_data().m_shadow_terminator_freq_mult;
                const float cos_in = std::min(std::abs(foundation::dot(incoming, local_geometry.m_shading_basis.get_normal())), 1.0f);
                value *= shift_cos_in_fast(cos_in, shadow_terminator_freq_mult);
            }
        }
    }

    return probability;
}

template <typename BSDFImpl, bool Cull>
float BSDFWrapper<BSDFImpl, Cull>::evaluate_pdf(
    const void*                         data,
    const bool                          adjoint,
    const LocalGeometry&                local_geometry,
    const foundation::Vector3f&         outgoing,
    const foundation::Vector3f&         incoming,
    const int                           modes) const
{
    assert(foundation::is_normalized(local_geometry.m_geometric_normal));
    assert(foundation::is_normalized(outgoing));
    assert(foundation::is_normalized(incoming));

    if (Cull && is_culled(adjoint, local_geometry.m_shading_basis, outgoing, incoming))
        return 0.0f;

    const float probability =
        BSDFImpl::evaluate_pdf(
            data,
            adjoint,
            local_geometry,
            outgoing,
            incoming,
            modes);
    assert(probability >= 0.0f);

    return probability;
}

template <typename BSDFImpl, bool Cull>
bool BSDFWrapper<BSDFImpl, Cull>::is_culled(
    const bool                          adjoint,
    const foundation::Basis3f&          shading_basis,
    const foundation::Vector3f&         outgoing,
    const foundation::Vector3f&         incoming) const
{
    const foundation::Vector3f& n = shading_basis.get_normal();
    const float cos_n = foundation::dot(adjoint ? outgoing : incoming, n);
    return BSDFImpl::get_type() == BSDF::Reflective ? cos_n < 0.0f : cos_n > 0.0f;
}

}   // namespace renderer
