
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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

#ifndef APPLESEED_RENDERER_MODELING_BSDF_BRDFWRAPPER_H
#define APPLESEED_RENDERER_MODELING_BSDF_BRDFWRAPPER_H

// appleseed.renderer headers.
#include "renderer/global/global.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"

namespace renderer
{

//
// The BRDFWrapper class wraps a BRDF implementation with domain validity checks
// and takes care of correcting for the use of shading normals in the adjoint case.
//

template <typename BRDFImpl>
class BRDFWrapper
  : public BRDFImpl
{
  public:
    typedef typename BRDFImpl::Mode Mode;

    BRDFWrapper(
        const char*                     name,
        const ParamArray&               params);

    virtual void sample(
        SamplingContext&                sampling_context,
        const void*                     data,
        const bool                      adjoint,
        const bool                      cosine_mult,
        const foundation::Vector3d&     geometric_normal,
        const foundation::Basis3d&      shading_basis,
        const foundation::Vector3d&     outgoing,
        foundation::Vector3d&           incoming,
        Spectrum&                       value,
        double&                         probability,
        Mode&                           mode) const override;

    virtual double evaluate(
        const void*                     data,
        const bool                      adjoint,
        const bool                      cosine_mult,
        const foundation::Vector3d&     geometric_normal,
        const foundation::Basis3d&      shading_basis,
        const foundation::Vector3d&     outgoing,
        const foundation::Vector3d&     incoming,
        Spectrum&                       value) const override;

    virtual double evaluate_pdf(
        const void*                     data,
        const foundation::Vector3d&     geometric_normal,
        const foundation::Basis3d&      shading_basis,
        const foundation::Vector3d&     outgoing,
        const foundation::Vector3d&     incoming) const override;
};


//
// BRDFWrapper class implementation.
//

template <typename BRDFImpl>
BRDFWrapper<BRDFImpl>::BRDFWrapper(
    const char*                         name,
    const ParamArray&                   params)
  : BRDFImpl(name, params)
{
}

template <typename BRDFImpl>
void BRDFWrapper<BRDFImpl>::sample(
    SamplingContext&                    sampling_context,
    const void*                         data,
    const bool                          adjoint,
    const bool                          cosine_mult,
    const foundation::Vector3d&         geometric_normal,
    const foundation::Basis3d&          shading_basis,
    const foundation::Vector3d&         outgoing,
    foundation::Vector3d&               incoming,
    Spectrum&                           value,
    double&                             probability,
    Mode&                               mode) const
{
    assert(foundation::is_normalized(geometric_normal));
    assert(foundation::is_normalized(outgoing));
    assert(foundation::dot(outgoing, geometric_normal) >= 0.0);

    BRDFImpl::sample(
        sampling_context,
        data,
        adjoint,
        false,
        geometric_normal,
        shading_basis,
        outgoing,
        incoming,
        value,
        probability,
        mode);

    if (mode == BRDFImpl::Absorption)
        return;

    assert(foundation::is_normalized(incoming));
    assert(foundation::dot(incoming, geometric_normal) >= 0.0);
    assert(probability == DiracDelta || probability > 0.0);

    if (cosine_mult)
    {
        if (adjoint)
        {
            const double cos_on = std::abs(foundation::dot(outgoing, shading_basis.get_normal()));
            const double cos_ig = foundation::dot(incoming, geometric_normal);
            const double cos_og = foundation::dot(outgoing, geometric_normal);
            value *= static_cast<float>(cos_on * cos_ig / cos_og);
        }
        else
        {
            const double cos_in = std::abs(foundation::dot(incoming, shading_basis.get_normal()));
            value *= static_cast<float>(cos_in);
        }
    }
}

template <typename BRDFImpl>
double BRDFWrapper<BRDFImpl>::evaluate(
    const void*                         data,
    const bool                          adjoint,
    const bool                          cosine_mult,
    const foundation::Vector3d&         geometric_normal,
    const foundation::Basis3d&          shading_basis,
    const foundation::Vector3d&         outgoing,
    const foundation::Vector3d&         incoming,
    Spectrum&                           value) const
{
    assert(foundation::is_normalized(geometric_normal));
    assert(foundation::is_normalized(outgoing));
    assert(foundation::is_normalized(incoming));

    const double cos_ig = foundation::dot(incoming, geometric_normal);
    const double cos_og = foundation::dot(outgoing, geometric_normal);

    // No reflection in or below the geometric surface.
    if (cos_ig <= 0.0 || cos_og <= 0.0)
        return 0.0;

    const double probability =
        BRDFImpl::evaluate(
            data,
            adjoint,
            false,
            geometric_normal,
            shading_basis,
            outgoing,
            incoming,
            value);

    assert(probability >= 0.0);

    if (probability > 0.0 && cosine_mult)
    {
        if (adjoint)
        {
            const double cos_on = std::abs(foundation::dot(outgoing, shading_basis.get_normal()));
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

template <typename BRDFImpl>
double BRDFWrapper<BRDFImpl>::evaluate_pdf(
    const void*                         data,
    const foundation::Vector3d&         geometric_normal,
    const foundation::Basis3d&          shading_basis,
    const foundation::Vector3d&         outgoing,
    const foundation::Vector3d&         incoming) const
{
    assert(foundation::is_normalized(geometric_normal));
    assert(foundation::is_normalized(outgoing));
    assert(foundation::is_normalized(incoming));

    const double cos_ig = foundation::dot(incoming, geometric_normal);
    const double cos_og = foundation::dot(outgoing, geometric_normal);

    // No reflection in or below the geometric surface.
    if (cos_ig <= 0.0 || cos_og <= 0.0)
        return 0.0;

    const double probability =
        BRDFImpl::evaluate_pdf(
            data,
            geometric_normal,
            shading_basis,
            outgoing,
            incoming);

    assert(probability >= 0.0);

    return probability;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_BSDF_BRDFWRAPPER_H
