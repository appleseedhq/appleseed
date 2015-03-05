
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
        const void*                     data,
        const bool                      adjoint,
        const bool                      cosine_mult,
        BSDFSample&                     sample) const APPLESEED_OVERRIDE;

    virtual double evaluate(
        const void*                     data,
        const bool                      adjoint,
        const bool                      cosine_mult,
        const foundation::Vector3d&     geometric_normal,
        const foundation::Basis3d&      shading_basis,
        const foundation::Vector3d&     outgoing,
        const foundation::Vector3d&     incoming,
        const int                       modes,
        Spectrum&                       value) const APPLESEED_OVERRIDE;

    virtual double evaluate_pdf(
        const void*                     data,
        const foundation::Vector3d&     geometric_normal,
        const foundation::Basis3d&      shading_basis,
        const foundation::Vector3d&     outgoing,
        const foundation::Vector3d&     incoming,
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
    const void*                         data,
    const bool                          adjoint,
    const bool                          cosine_mult,
    BSDFSample&                         sample) const
{
    assert(foundation::is_normalized(sample.get_geometric_normal()));
    assert(foundation::is_normalized(sample.get_outgoing_vector()));

    BSDFImpl::sample(
        data,
        adjoint,
        false,
        sample);

    if (!sample.is_absorption())
    {
        assert(foundation::is_normalized(sample.get_incoming_vector()));
        assert(sample.get_probability() == BSDFImpl::DiracDelta || sample.get_probability() > 0.0);

        if (cosine_mult)
        {
            if (adjoint)
            {
                const double cos_on = std::abs(foundation::dot(sample.get_outgoing_vector(), sample.get_shading_normal()));
                const double cos_ig = std::abs(foundation::dot(sample.get_incoming_vector(), sample.get_geometric_normal()));
                const double cos_og = std::abs(foundation::dot(sample.get_outgoing_vector(), sample.get_geometric_normal()));
                sample.value() *= static_cast<float>(cos_on * cos_ig / cos_og);
            }
            else
            {
                const double cos_in = std::abs(foundation::dot(sample.get_incoming_vector(), sample.get_shading_normal()));
                sample.value() *= static_cast<float>(cos_in);
            }
        }
    }
}

template <typename BSDFImpl>
double BSDFWrapper<BSDFImpl>::evaluate(
    const void*                         data,
    const bool                          adjoint,
    const bool                          cosine_mult,
    const foundation::Vector3d&         geometric_normal,
    const foundation::Basis3d&          shading_basis,
    const foundation::Vector3d&         outgoing,
    const foundation::Vector3d&         incoming,
    const int                           modes,
    Spectrum&                           value) const
{
    assert(foundation::is_normalized(geometric_normal));
    assert(foundation::is_normalized(outgoing));
    assert(foundation::is_normalized(incoming));

    const double probability =
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

    assert(probability >= 0.0);

    if (probability > 0.0 && cosine_mult)
    {
        if (adjoint)
        {
            const double cos_on = std::abs(foundation::dot(outgoing, shading_basis.get_normal()));
            const double cos_ig = std::abs(foundation::dot(incoming, geometric_normal));
            const double cos_og = std::abs(foundation::dot(outgoing, geometric_normal));
            value *= static_cast<float>(cos_on * cos_ig / cos_og);
        }
        else
        {
            const double cos_in = std::abs(foundation::dot(incoming, shading_basis.get_normal()));
            value *= static_cast<float>(cos_in);
        }
    }

    return probability;
}

template <typename BSDFImpl>
double BSDFWrapper<BSDFImpl>::evaluate_pdf(
    const void*                         data,
    const foundation::Vector3d&         geometric_normal,
    const foundation::Basis3d&          shading_basis,
    const foundation::Vector3d&         outgoing,
    const foundation::Vector3d&         incoming,
    const int                           modes) const
{
    assert(foundation::is_normalized(geometric_normal));
    assert(foundation::is_normalized(outgoing));
    assert(foundation::is_normalized(incoming));

    const double probability =
        BSDFImpl::evaluate_pdf(
            data,
            geometric_normal,
            shading_basis,
            outgoing,
            incoming,
            modes);

    assert(probability >= 0.0);

    return probability;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_BSDF_BSDFWRAPPER_H
