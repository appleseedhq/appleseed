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
#include "renderer/modeling/bsdf/microfacet2brdf.h"
#include "renderer/modeling/bsdf/orennayarbrdf.h"
#include "renderer/modeling/bsdf/specularbrdf.h"
#include "renderer/modeling/bsdf/specularbtdf.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/image/color.h"
#include "foundation/math/cdf.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"

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

// Standard headers.
#include <cassert>
#include <cstddef>

namespace renderer
{

//
// appleseed's closures IDs.
//

enum ClosureID
{
    // BSDF closures.
    AshikhminShirleyID,
    LambertID,
    OrenNayarID,
    ReflectionID,
    RefractionID,
    TranslucentID,

    // Special closures.
    BackgroundID,
    DebugID,
    EmissionID,
    HoldoutID,
    TransparentID,

    // Microfacets are special.    
    MicrofacetID,
    MicrofacetBeckmannReflectionID = MicrofacetID,
    MicrofacetBlinnReflectionID,
    MicrofacetGGXReflectionID,

    NumClosuresIDs
};

//
// Composite OSL closure.
//

class APPLESEED_ALIGN(16) CompositeClosure
  : public foundation::NonCopyable
{
  public:
    explicit CompositeClosure(const OSL::ClosureColor* ci);

    size_t get_num_closures() const;
    ClosureID get_closure_type(const size_t index) const;
    double get_closure_cdf_weight(const size_t index) const;
    const Spectrum& get_closure_spectrum_multiplier(const size_t index) const;
    const foundation::Vector3d& get_closure_normal(const size_t index) const;
    bool closure_has_tangent(const size_t index) const;
    const foundation::Vector3d& get_closure_tangent(const size_t index) const;
    void* get_closure_input_values(const size_t index) const;

    size_t choose_closure(const double w) const;

  private:
    typedef boost::mpl::vector<
        AshikminBRDFInputValues,
        DiffuseBTDFInputValues,
        LambertianBRDFInputValues,
        Microfacet2BRDFInputValues,
        OrenNayarBRDFInputValues,
        SpecularBRDFInputValues,
        SpecularBTDFInputValues> InputValuesTypeList;

    // Find the biggest InputValues type.
    typedef boost::mpl::max_element<
        boost::mpl::transform_view<
            InputValuesTypeList,
            boost::mpl::sizeof_<boost::mpl::_1> > >::type BiggestInputValueType;

    enum { InputValuesAlignment = 16 };
    enum { MaxClosureEntries = 8 };
    enum { MaxPoolSize = MaxClosureEntries * (sizeof(boost::mpl::deref<BiggestInputValueType::base>::type) + 16) };

    // m_pool has to be first, because it has to be aligned.
    char                            m_pool[MaxPoolSize];
    void*                           m_input_values[MaxClosureEntries];
    ClosureID                       m_closure_types[MaxClosureEntries];
    foundation::Vector3d            m_normals[MaxClosureEntries];
    bool                            m_has_tangent[MaxClosureEntries];
    foundation::Vector3d            m_tangents[MaxClosureEntries];
    int                             m_num_closures;
    int                             m_num_bytes;
    foundation::CDF<size_t, double> m_cdf;
    Spectrum                        m_spectrum_multipliers[MaxClosureEntries];

    void process_closure_tree(
        const OSL::ClosureColor*    closure,
        const foundation::Color3f&  weight);

    template <typename InputValues>
    void add_closure(
        const ClosureID             closure_type,
        const foundation::Color3f&  weight,
        const foundation::Vector3d& normal,
        const InputValues&          values);

    template <typename InputValues>
    void add_closure(
        const ClosureID             closure_type,
        const foundation::Color3f&  weight,
        const foundation::Vector3d& normal,
        const foundation::Vector3d& tangent,
        const InputValues&          values);

    template <typename InputValues>
    void do_add_closure(
        const ClosureID             closure_type,
        const foundation::Color3f&  weight,
        const foundation::Vector3d& normal,
        bool                        has_tangent,
        const foundation::Vector3d& tangent,
        const InputValues&          values);
};

// Register appleseed's closures.
void register_closures(OSL::ShadingSystem& shading_system);


//
// CompositeClosure class implementation.
//

inline size_t CompositeClosure::get_num_closures() const
{
    return m_num_closures;
}

inline ClosureID CompositeClosure::get_closure_type(const size_t index) const
{
    assert(index < get_num_closures());
    return m_closure_types[index];
}

inline double CompositeClosure::get_closure_cdf_weight(const size_t index) const
{
    assert(index < get_num_closures());
    return m_cdf[index].second;
}

inline const Spectrum& CompositeClosure::get_closure_spectrum_multiplier(const size_t index) const
{
    assert(index < get_num_closures());
    return m_spectrum_multipliers[index];
}

inline const foundation::Vector3d& CompositeClosure::get_closure_normal(const size_t index) const
{
    assert(index < get_num_closures());
    return m_normals[index];
}

inline bool CompositeClosure::closure_has_tangent(const size_t index) const
{
    assert(index < get_num_closures());
    return m_has_tangent[index];
}

inline const foundation::Vector3d& CompositeClosure::get_closure_tangent(const size_t index) const
{
    assert(index < get_num_closures());
    assert(closure_has_tangent(index));
    return m_tangents[index];
}

inline void* CompositeClosure::get_closure_input_values(const size_t index) const
{
    assert(index < get_num_closures());
    return m_input_values[index];
}

void process_transparency_tree(const OSL::ClosureColor* ci, Alpha& alpha);
float process_holdout_tree(const OSL::ClosureColor* ci);

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_SHADING_CLOSURES_H
