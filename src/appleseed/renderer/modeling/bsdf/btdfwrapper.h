
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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

#ifndef APPLESEED_RENDERER_MODELING_BSDF_BTDFWRAPPER_H
#define APPLESEED_RENDERER_MODELING_BSDF_BTDFWRAPPER_H

// appleseed.renderer headers.
#include "renderer/global/global.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"

namespace renderer
{

//
// The BTDFWrapper class wraps a BTDF with domain validity checks.
//

template <typename Base>
class BTDFWrapper
  : public Base
{
  public:
    typedef typename Base::Mode Mode;

    BTDFWrapper(
        const char*                     name,
        const ParamArray&               params);

    virtual void sample(
        const void*                     data,
        const foundation::Vector3d&     geometric_normal,
        const foundation::Basis3d&      shading_basis,
        const foundation::Vector3d&     s,
        const foundation::Vector3d&     outgoing,
        foundation::Vector3d&           incoming,
        Spectrum&                       value,
        double&                         probability,
        Mode&                           mode) const;

    virtual void evaluate(
        const void*                     data,
        const foundation::Vector3d&     geometric_normal,
        const foundation::Basis3d&      shading_basis,
        const foundation::Vector3d&     outgoing,
        const foundation::Vector3d&     incoming,
        Spectrum&                       value) const;

    virtual double evaluate_pdf(
        const void*                     data,
        const foundation::Vector3d&     geometric_normal,
        const foundation::Basis3d&      shading_basis,
        const foundation::Vector3d&     outgoing,
        const foundation::Vector3d&     incoming) const;
};


//
// BTDFWrapper class implementation.
//

template <typename Base>
BTDFWrapper<Base>::BTDFWrapper(
    const char*                         name,
    const ParamArray&                   params)
  : Base(name, params)
{
}

template <typename Base>
void BTDFWrapper<Base>::sample(
    const void*                         data,
    const foundation::Vector3d&         geometric_normal,
    const foundation::Basis3d&          shading_basis,
    const foundation::Vector3d&         s,
    const foundation::Vector3d&         outgoing,
    foundation::Vector3d&               incoming,
    Spectrum&                           value,
    double&                             probability,
    Mode&                               mode) const
{
    assert(foundation::is_normalized(geometric_normal));
    assert(foundation::is_normalized(outgoing));

    Base::sample(
        data,
        geometric_normal,
        shading_basis,
        s,
        outgoing,
        incoming,
        value,
        probability,
        mode);
}

template <typename Base>
void BTDFWrapper<Base>::evaluate(
    const void*                         data,
    const foundation::Vector3d&         geometric_normal,
    const foundation::Basis3d&          shading_basis,
    const foundation::Vector3d&         outgoing,
    const foundation::Vector3d&         incoming,
    Spectrum&                           value) const
{
    assert(foundation::is_normalized(geometric_normal));
    assert(foundation::is_normalized(outgoing));
    assert(foundation::is_normalized(incoming));

    Base::evaluate(
        data,
        geometric_normal,
        shading_basis,
        outgoing,
        incoming,
        value);
}

template <typename Base>
double BTDFWrapper<Base>::evaluate_pdf(
    const void*                         data,
    const foundation::Vector3d&         geometric_normal,
    const foundation::Basis3d&          shading_basis,
    const foundation::Vector3d&         outgoing,
    const foundation::Vector3d&         incoming) const
{
    assert(foundation::is_normalized(geometric_normal));
    assert(foundation::is_normalized(outgoing));
    assert(foundation::is_normalized(incoming));

    const double probability =
        Base::evaluate_pdf(
            data,
            geometric_normal,
            shading_basis,
            outgoing,
            incoming);

    assert(probability >= 0.0);

    return probability;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_BSDF_BTDFWRAPPER_H
