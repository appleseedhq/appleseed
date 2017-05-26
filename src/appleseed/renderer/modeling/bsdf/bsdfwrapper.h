
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_MODELING_BSDF_BSDFWRAPPER_H
#define APPLESEED_RENDERER_MODELING_BSDF_BSDFWRAPPER_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/bsdf/bsdfsample.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"

// Standard headers.
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

template <typename BSDFImpl>
class BSDFWrapper
  : public BSDFImpl
{
  public:
    BSDFWrapper(
        const char*                     name,
        const ParamArray&               params);

    virtual void sample(
        SamplingContext&                sampling_context,
        const void*                     data,
        const bool                      adjoint,
        const bool                      cosine_mult,
        const int                       modes,
        BSDFSample&                     sample) const APPLESEED_OVERRIDE;

    virtual float evaluate(
        const void*                     data,
        const bool                      adjoint,
        const bool                      cosine_mult,
        const foundation::Vector3f&     geometric_normal,
        const foundation::Basis3f&      shading_basis,
        const foundation::Vector3f&     outgoing,
        const foundation::Vector3f&     incoming,
        const int                       modes,
        Spectrum&                       value) const APPLESEED_OVERRIDE;

    virtual float evaluate_pdf(
        const void*                     data,
        const foundation::Vector3f&     geometric_normal,
        const foundation::Basis3f&      shading_basis,
        const foundation::Vector3f&     outgoing,
        const foundation::Vector3f&     incoming,
        const int                       modes) const APPLESEED_OVERRIDE;
};


//
// BSDFWrapper class implementation.
//

template <typename BSDFImpl>
BSDFWrapper<BSDFImpl>::BSDFWrapper(
    const char*                         name,
    const ParamArray&                   params)
  : BSDFImpl(name, params)
{
}

template <typename BSDFImpl>
void BSDFWrapper<BSDFImpl>::sample(
    SamplingContext&                    sampling_context,
    const void*                         data,
    const bool                          adjoint,
    const bool                          cosine_mult,
    const int                           modes,
    BSDFSample&                         sample) const
{
    assert(foundation::is_normalized(sample.m_geometric_normal));
    assert(foundation::is_normalized(sample.m_outgoing.get_value()));

    BSDFImpl::sample(
        sampling_context,
        data,
        adjoint,
        false,
        modes,
        sample);

    if (sample.m_mode != ScatteringMode::None)
    {
        assert(foundation::is_normalized(sample.m_incoming.get_value(), 1.0e-5f));
        assert(sample.m_probability == BSDFImpl::DiracDelta || sample.m_probability > 0.0f);

        if (cosine_mult)
        {
            if (adjoint)
            {
                const float cos_on = std::abs(foundation::dot(sample.m_outgoing.get_value(), sample.m_shading_basis.get_normal()));
                const float cos_ig = std::abs(foundation::dot(sample.m_incoming.get_value(), sample.m_geometric_normal));
                const float cos_og = std::abs(foundation::dot(sample.m_outgoing.get_value(), sample.m_geometric_normal));
                sample.m_value *= cos_on * cos_ig / cos_og;
            }
            else
            {
                const float cos_in = std::abs(foundation::dot(sample.m_incoming.get_value(), sample.m_shading_basis.get_normal()));
                sample.m_value *= cos_in;
            }
        }
    }
}

template <typename BSDFImpl>
float BSDFWrapper<BSDFImpl>::evaluate(
    const void*                         data,
    const bool                          adjoint,
    const bool                          cosine_mult,
    const foundation::Vector3f&         geometric_normal,
    const foundation::Basis3f&          shading_basis,
    const foundation::Vector3f&         outgoing,
    const foundation::Vector3f&         incoming,
    const int                           modes,
    Spectrum&                           value) const
{
    assert(foundation::is_normalized(geometric_normal));
    assert(foundation::is_normalized(outgoing));
    assert(foundation::is_normalized(incoming));

    const float probability =
        BSDFImpl::evaluate(
            data,
            adjoint,
            false,
            geometric_normal,
            shading_basis,
            outgoing,
            incoming,
            modes,
            value);

    assert(probability >= 0.0f);

    if (probability > 0.0f && cosine_mult)
    {
        if (adjoint)
        {
            const float cos_on = std::abs(foundation::dot(outgoing, shading_basis.get_normal()));
            const float cos_ig = std::abs(foundation::dot(incoming, geometric_normal));
            const float cos_og = std::abs(foundation::dot(outgoing, geometric_normal));
            value *= cos_on * cos_ig / cos_og;
        }
        else
        {
            const float cos_in = std::abs(foundation::dot(incoming, shading_basis.get_normal()));
            value *= cos_in;
        }
    }

    return probability;
}

template <typename BSDFImpl>
float BSDFWrapper<BSDFImpl>::evaluate_pdf(
    const void*                         data,
    const foundation::Vector3f&         geometric_normal,
    const foundation::Basis3f&          shading_basis,
    const foundation::Vector3f&         outgoing,
    const foundation::Vector3f&         incoming,
    const int                           modes) const
{
    assert(foundation::is_normalized(geometric_normal));
    assert(foundation::is_normalized(outgoing));
    assert(foundation::is_normalized(incoming));

    const float probability =
        BSDFImpl::evaluate_pdf(
            data,
            geometric_normal,
            shading_basis,
            outgoing,
            incoming,
            modes);

    assert(probability >= 0.0f);

    return probability;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_BSDF_BSDFWRAPPER_H
