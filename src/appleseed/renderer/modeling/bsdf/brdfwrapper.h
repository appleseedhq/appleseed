
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
        const foundation::Vector3d&     geometric_normal,
        const foundation::Basis3d&      shading_basis,
        const foundation::Vector3d&     outgoing,
        foundation::Vector3d&           incoming,
        Spectrum&                       value,
        double&                         probability,
        Mode&                           mode) const;

    virtual bool evaluate(
        const void*                     data,
        const bool                      adjoint,
        const foundation::Vector3d&     geometric_normal,
        const foundation::Basis3d&      shading_basis,
        const foundation::Vector3d&     outgoing,
        const foundation::Vector3d&     incoming,
        Spectrum&                       value,
        double*                         probability = 0) const;

    virtual double evaluate_pdf(
        const void*                     data,
        const foundation::Vector3d&     geometric_normal,
        const foundation::Basis3d&      shading_basis,
        const foundation::Vector3d&     outgoing,
        const foundation::Vector3d&     incoming) const;
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

    // No reflection for back faces.
    const foundation::Vector3d& shading_normal = shading_basis.get_normal();
    const double cos_ng = foundation::dot(shading_normal, geometric_normal);
    if (cos_ng <= 0.0)
    {
        mode = BRDFImpl::None;
        return;
    }

    // No reflection in or below the geometric surface.
    const double cos_og = foundation::dot(outgoing, geometric_normal);
    if (cos_og <= 0.0)
    {
        mode = BRDFImpl::None;
        return;
    }

    BRDFImpl::sample(
        sampling_context,
        data,
        adjoint,
        geometric_normal,
        shading_basis,
        outgoing,
        incoming,
        value,
        probability,
        mode);

    if (adjoint)
    {
        const double cos_on = std::abs(foundation::dot(outgoing, shading_normal));
        const double cos_ig = foundation::dot(incoming, geometric_normal);
        value *= static_cast<float>(cos_on * cos_ig / cos_og);
    }
    else
    {
        const double cos_in = std::abs(foundation::dot(incoming, shading_normal));
        value *= static_cast<float>(cos_in);
    }
}

template <typename BRDFImpl>
bool BRDFWrapper<BRDFImpl>::evaluate(
    const void*                         data,
    const bool                          adjoint,
    const foundation::Vector3d&         geometric_normal,
    const foundation::Basis3d&          shading_basis,
    const foundation::Vector3d&         outgoing,
    const foundation::Vector3d&         incoming,
    Spectrum&                           value,
    double*                             probability) const
{
    assert(foundation::is_normalized(geometric_normal));
    assert(foundation::is_normalized(outgoing));
    assert(foundation::is_normalized(incoming));

    // No reflection for back faces.
    const foundation::Vector3d& shading_normal = shading_basis.get_normal();
    const double cos_ng = foundation::dot(shading_normal, geometric_normal);
    if (cos_ng <= 0.0)
        return false;

    // No reflection in or below the geometric surface.
    const double cos_ig = foundation::dot(incoming, geometric_normal);
    const double cos_og = foundation::dot(outgoing, geometric_normal);
    if (cos_ig <= 0.0 || cos_og <= 0.0)
        return false;

    const bool defined =
        BRDFImpl::evaluate(
            data,
            adjoint,
            geometric_normal,
            shading_basis,
            outgoing,
            incoming,
            value,
            probability);

    if (!defined)
        return false;

    assert(probability == 0 || *probability >= 0.0);

    if (adjoint)
    {
        const double cos_on = std::abs(foundation::dot(outgoing, shading_normal));
        value *= static_cast<float>(cos_on * cos_ig / cos_og);
    }
    else
    {
        const double cos_in = std::abs(foundation::dot(incoming, shading_normal));
        value *= static_cast<float>(cos_in);
    }

    return true;
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

    // No reflection for back faces.
    const foundation::Vector3d& shading_normal = shading_basis.get_normal();
    const double cos_ng = foundation::dot(shading_normal, geometric_normal);
    if (cos_ng <= 0.0)
        return 0.0;

    // No reflection in or below the geometric surface.
    const double cos_ig = foundation::dot(incoming, geometric_normal);
    const double cos_og = foundation::dot(outgoing, geometric_normal);
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
