
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014 Esteban Tovagliari, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_KERNEL_SHADING_CLOSURES_H
#define APPLESEED_RENDERER_KERNEL_SHADING_CLOSURES_H

// appleseed.renderer headers.
#include "renderer/modeling/bsdf/ashikhminbrdf.h"
#include "renderer/modeling/bsdf/diffusebtdf.h"
#include "renderer/modeling/bsdf/lambertianbrdf.h"
#include "renderer/modeling/bsdf/microfacetbrdf.h"
#include "renderer/modeling/bsdf/specularbrdf.h"
#include "renderer/modeling/bsdf/specularbtdf.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"

// OSL headers.
#include "OSL/dual.h"
#include "OSL/oslexec.h"

// boost headers.
#include <boost/mpl/assert.hpp>
#include <boost/mpl/back_inserter.hpp>
#include <boost/mpl/copy.hpp>
#include <boost/mpl/deref.hpp>
#include <boost/mpl/equal.hpp>
#include <boost/mpl/max_element.hpp>
#include <boost/mpl/size.hpp>
#include <boost/mpl/sizeof.hpp>
#include <boost/mpl/transform_view.hpp>
#include <boost/mpl/vector.hpp>

namespace renderer
{

//
// appleseed's closures IDs.
//

enum ClosureID
{
    // bsdf closures.
    AshikhminShirleyID = 1,
    LambertID,
    MicrofacetBeckmannID,
    MicrofacetBlinnID,
    MicrofacetGGXID,
    MicrofacetWardID,
    ReflectionID,
    RefractionID,
    TranslucentID,

    // special closures.
    EmissionID,
    HoldoutID,
    TransparentID
};


//
// Composite OSL closure.
//

class FOUNDATION_ALIGN(16) CompositeClosure 
  : public foundation::NonCopyable
{
  private:
    typedef boost::mpl::vector< 
        AshikminBRDFInputValues,
        DiffuseBTDFInputValues,
        LambertianBRDFInputValues,
        MicrofacetBRDFInputValues,
        SpecularBRDFInputValues,
        SpecularBTDFInputValues> InputValuesTypeList;

    // Find the biggest InputValues type.
    typedef boost::mpl::max_element<
        boost::mpl::transform_view<
            InputValuesTypeList,
            boost::mpl::sizeof_<boost::mpl::_1> > >::type BiggestInputValueType;

    enum { MaxClosureEntries = 8 };
    enum { MaxPoolSize = MaxClosureEntries * sizeof(boost::mpl::deref<BiggestInputValueType::base>::type) };

    char m_pool[MaxPoolSize];
};

// Register appleseed's closures.
void register_closures(OSL::ShadingSystem& shading_system);

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_SHADING_CLOSURES_H
