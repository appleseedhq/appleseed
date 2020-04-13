
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2018 Esteban Tovagliari, The appleseedhq Organization
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

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/core/exceptions/exception.h"
#include "foundation/image/color.h"
#include "foundation/math/basis.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"

// OSL headers.
#include "foundation/platform/_beginoslheaders.h"
#include "OSL/dual.h"
#include "OSL/oslexec.h"
#include "foundation/platform/_endoslheaders.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstdint>

// Forward declarations.
namespace foundation    { class Arena; }
namespace renderer      { class BSDF; }
namespace renderer      { class OSLShadingSystem; }

namespace renderer
{

//
// appleseed's closure IDs.
//

enum ClosureID
{
    // BSDF closures.
    AshikhminShirleyID,
    BlinnID,
    DiffuseID,
    DisneyID,
    HairID,
    OrenNayarID,
    PhongID,
    ReflectionID,
    SheenID,
    TranslucentID,

    // Microfacet closures.
    GlassID,
    GlossyID,
    MetalID,
    PlasticID,

    // BSSRDF closures.
    SubsurfaceID,
    SubsurfaceBetterDipoleID,
    SubsurfaceStandardDipoleID,
    SubsurfaceDirectionalDipoleID,
    SubsurfaceNormalizedDiffusionID,
    SubsurfaceGaussianID,
    SubsurfaceRandomwalkID,

    RandomwalkGlassID,

    // Emission closures.
    EmissionID,

    // Special closures.
    BackgroundID,
    DebugID,
    HoldoutID,
    TransparentID,
    MatteID,

    // NPR closures.
    NPRShadingID,
    NPRContourID,

    // Layered BSDF closures (must be last).
    FirstLayeredClosure,
    GlossyLayerID = FirstLayeredClosure,

    NumClosuresIDs
};


//
// Exception thrown in case of an OSL runtime error.
//

struct ExceptionOSLRuntimeError
  : public foundation::Exception
{
    explicit ExceptionOSLRuntimeError(const char* what);
};


//
// Small closure layer ID stack.
//

template <std::size_t N>
class ClosureLayerIDStack
{
  public:
    ClosureLayerIDStack()
      : m_size(0)
    {
    }

    std::size_t size() const
    {
        return m_size;
    }

    bool empty() const
    {
        return m_size == 0;
    }

    std::int8_t operator[](const std::size_t i) const
    {
        assert(i < m_size);
        return m_ids[i];
    }

    void push(const std::int8_t id)
    {
        assert(m_size < N);
        m_ids[m_size++] = id;
    }

    void pop()
    {
        assert(m_size > 0);
        --m_size;
    }

  private:
    std::int8_t   m_ids[N];
    std::size_t   m_size;
};


//
// Composite OSL closure base class.
//

class APPLESEED_ALIGN(16) CompositeClosure
  : public foundation::NonCopyable
{
  public:
    enum { MaxClosureEntries = 16 };
    enum { MaxClosureLayers = 4 };

    using SmallClosureLayerIDStack = ClosureLayerIDStack<MaxClosureLayers>;

    std::size_t get_closure_count() const;
    std::int8_t get_last_closure_index() const;

    ClosureID get_closure_type(const std::size_t index) const;
    void* get_closure_input_values(const std::size_t index) const;

    const Spectrum& get_closure_weight(const std::size_t index) const;
    float get_closure_scalar_weight(const std::size_t index) const;

    const foundation::Basis3f& get_closure_shading_basis(const std::size_t index) const;

    const std::int8_t* get_closure_layers(const std::size_t index) const;

    void override_closure_scalar_weight(const float weight);

    void compute_closure_shading_basis(
        const foundation::Vector3f& normal,
        const foundation::Basis3f&  original_shading_basis);

    void compute_closure_shading_basis(
        const foundation::Vector3f& normal,
        const foundation::Vector3f& tangent,
        const foundation::Basis3f&  original_shading_basis);

    template <typename InputValues>
    InputValues* add_closure(
        const ClosureID             closure_type,
        const foundation::Basis3f&  original_shading_basis,
        const foundation::Color3f&  weight,
        const foundation::Vector3f& normal,
        foundation::Arena&          arena);

    template <typename InputValues>
    InputValues* add_closure(
        const ClosureID             closure_type,
        const foundation::Basis3f&  original_shading_basis,
        const foundation::Color3f&  weight,
        const foundation::Vector3f& normal,
        const foundation::Vector3f& tangent,
        foundation::Arena&          arena);

    void add_ior(
        const foundation::Color3f&  weight,
        const float                 ior);

    float choose_ior(const float w) const;

    void copy_layer_ids(const SmallClosureLayerIDStack& layer_stack);

  protected:
    std::size_t                     m_closure_count;
    void*                           m_input_values[MaxClosureEntries];
    ClosureID                       m_closure_types[MaxClosureEntries];
    Spectrum                        m_weights[MaxClosureEntries];
    float                           m_scalar_weights[MaxClosureEntries];
    foundation::Basis3f             m_bases[MaxClosureEntries];
    std::size_t                     m_ior_count;
    float                           m_iors[MaxClosureEntries];
    float                           m_ior_cdf[MaxClosureEntries];
    std::int8_t                     m_layers[MaxClosureEntries * MaxClosureLayers];

    CompositeClosure();

    template <typename InputValues, bool HasTangent>
    InputValues* do_add_closure(
        const ClosureID             closure_type,
        const foundation::Basis3f&  original_shading_basis,
        const foundation::Color3f&  weight,
        const foundation::Vector3f& normal,
        const foundation::Vector3f& tangent,
        foundation::Arena&          arena);

    void compute_pdfs(float pdfs[MaxClosureEntries]);
};


//
// Composite OSL surface closure.
//

class APPLESEED_ALIGN(16) CompositeSurfaceClosure
  : public CompositeClosure
{
  public:
    CompositeSurfaceClosure(
        const foundation::Basis3f&  original_shading_basis,
        const OSL::ClosureColor*    ci,
        foundation::Arena&          arena);

    int compute_pdfs(
        const int                   modes,
        float                       pdf[MaxClosureEntries]) const;

    std::size_t choose_closure(
        const float                 w,
        const std::size_t           num_closures,
        float                       pdfs[MaxClosureEntries]) const;

    void process_closure_tree(
        const OSL::ClosureColor*    closure,
        const foundation::Basis3f&  original_shading_basis,
        const foundation::Color3f&  weight,
        SmallClosureLayerIDStack&   layer_stack,
        foundation::Arena&          arena);
};


//
// Composite OSL subsurface closure.
//

class APPLESEED_ALIGN(16) CompositeSubsurfaceClosure
  : public CompositeClosure
{
  public:
    CompositeSubsurfaceClosure(
        const foundation::Basis3f&  original_shading_basis,
        const OSL::ClosureColor*    ci,
        foundation::Arena&          arena);

    float get_closure_pdf(const std::size_t index) const;

    std::size_t choose_closure(const float w) const;

  private:
    float                           m_pdfs[MaxClosureEntries];

    void process_closure_tree(
        const OSL::ClosureColor*    closure,
        const foundation::Basis3f&  original_shading_basis,
        const foundation::Color3f&  weight,
        SmallClosureLayerIDStack&   layer_stack,
        foundation::Arena&          arena);
};


//
// Composite OSL emission closure.
//

class APPLESEED_ALIGN(16) CompositeEmissionClosure
  : public CompositeClosure
{
  public:
    CompositeEmissionClosure(
        const foundation::Basis3f&  original_shading_basis,
        const OSL::ClosureColor*    ci,
        foundation::Arena&          arena);

    template <typename InputValues>
    InputValues* add_closure(
        const ClosureID             closure_type,
        const foundation::Color3f&  weight,
        const float                 max_weight_component,
        foundation::Arena&          arena);

    float get_closure_pdf(const std::size_t index) const;

    std::size_t choose_closure(const float w) const;

  private:
    float                           m_pdfs[MaxClosureEntries];

    void process_closure_tree(
        const OSL::ClosureColor*    closure,
        const foundation::Basis3f&  original_shading_basis,
        const foundation::Color3f&  weight,
        SmallClosureLayerIDStack&   layer_stack,
        foundation::Arena&          arena);
};


//
// Composite OSL NPR closure.
//

class APPLESEED_ALIGN(16) CompositeNPRClosure
  : public CompositeClosure
{
  public:
    CompositeNPRClosure(
        const OSL::ClosureColor*    ci,
        foundation::Arena&          arena);

    template <typename InputValues>
    InputValues* add_closure(
        const ClosureID             closure_type,
        const foundation::Color3f&  weight,
        foundation::Arena&          arena);

    std::size_t get_nth_contour_closure_index(const std::size_t i) const;

  private:
    void process_closure_tree(
        const OSL::ClosureColor*    closure,
        const foundation::Color3f&  weight,
        foundation::Arena&          arena);
};


//
// Utility functions.
//

void process_transparency_tree(const OSL::ClosureColor* ci, Alpha& alpha);

bool process_matte_tree(
    const OSL::ClosureColor*    ci,
    foundation::Color3f&        matte_color,
    float&                      matte_alpha);

foundation::Color3f process_background_tree(const OSL::ClosureColor* ci);

void register_closures(OSLShadingSystem& shading_system);


//
// CompositeClosure class implementation.
//

inline std::size_t CompositeClosure::get_closure_count() const
{
    return m_closure_count;
}

inline std::int8_t CompositeClosure::get_last_closure_index() const
{
    assert(m_closure_count > 0);
    assert(m_closure_count < 128);
    return static_cast<std::int8_t>(m_closure_count - 1);
}

inline ClosureID CompositeClosure::get_closure_type(const std::size_t index) const
{
    assert(index < get_closure_count());
    return m_closure_types[index];
}

inline void* CompositeClosure::get_closure_input_values(const std::size_t index) const
{
    assert(index < get_closure_count());
    return m_input_values[index];
}

inline const Spectrum& CompositeClosure::get_closure_weight(const std::size_t index) const
{
    assert(index < get_closure_count());
    return m_weights[index];
}

inline float CompositeClosure::get_closure_scalar_weight(const std::size_t index) const
{
    assert(index < get_closure_count());
    return m_scalar_weights[index];
}

inline const foundation::Basis3f& CompositeClosure::get_closure_shading_basis(const std::size_t index) const
{
    assert(index < get_closure_count());
    return m_bases[index];
}

inline const std::int8_t* CompositeClosure::get_closure_layers(const std::size_t index) const
{
    assert(index < get_closure_count());
    return m_layers + index * MaxClosureLayers;
}

inline void CompositeClosure::override_closure_scalar_weight(const float weight)
{
    assert(m_closure_count > 0);
    m_scalar_weights[m_closure_count - 1] = weight;
}


//
// CompositeSubsurfaceClosure class implementation.
//

inline float CompositeSubsurfaceClosure::get_closure_pdf(const std::size_t index) const
{
    assert(index < get_closure_count());
    return m_pdfs[index];
}


//
// CompositeEmissionClosure class implementation.
//

inline float CompositeEmissionClosure::get_closure_pdf(const std::size_t index) const
{
    assert(index < get_closure_count());
    return m_pdfs[index];
}

}   // namespace renderer
