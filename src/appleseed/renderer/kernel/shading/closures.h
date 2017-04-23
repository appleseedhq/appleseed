
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2017 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/global/globaltypes.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/core/exceptions/exception.h"
#include "foundation/image/color.h"
#include "foundation/math/basis.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"

// OSL headers.
#include "foundation/platform/oslheaderguards.h"
BEGIN_OSL_INCLUDES
#include "OSL/dual.h"
#include "OSL/oslexec.h"
END_OSL_INCLUDES

// Standard headers.
#include <cassert>
#include <cstddef>

// Forward declarations.
namespace foundation    { class Arena; }
namespace renderer      { class BSDF; }

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
    OrenNayarID,
    PhongID,
    ReflectionID,
    SheenID,
    TranslucentID,

    // Microfacet closures.
    GlassID,
    GlassBeckmannID,
    GlassGGXID,
    GlassSTDID,

    GlossyID,
    GlossyBeckmannID,
    GlossyGGXID,
    GlossySTDID,

    MetalID,
    MetalBeckmannID,
    MetalGGXID,
    MetalSTDID,

    // BSSRDF closures.
    SubsurfaceID,
    SubsurfaceBetterDipoleID,
    SubsurfaceStandardDipoleID,
    SubsurfaceDirectionalDipoleID,
    SubsurfaceNormalizedDiffusionID,
    SubsurfaceGaussianID,

    // Emission closures.
    EmissionID,

    // Special closures.
    BackgroundID,
    DebugID,
    HoldoutID,
    TransparentID,

    // Layered BSDF closures. (Must be last.)
    FirstLayeredClosure,
    AlSurfaceLayerID = FirstLayeredClosure,

    NumClosuresIDs
};


//
// Exception thrown in case of an OSL runtime error.
//

struct ExceptionOSLRuntimeError
  : public foundation::Exception
{
    explicit ExceptionOSLRuntimeError(const char* what)
      : foundation::Exception(what)
    {
    }
};


//
// Composite OSL closure.
//

class APPLESEED_ALIGN(16) CompositeClosure
  : public foundation::NonCopyable
{
  public:
    size_t get_closure_count() const;
    ClosureID get_closure_type(const size_t index) const;
    const Spectrum& get_closure_weight(const size_t index) const;
    float get_closure_pdf_weight(const size_t index) const;
    void* get_closure_input_values(const size_t index) const;

    size_t choose_closure(SamplingContext& sampling_context) const;
    size_t choose_closure(const float w) const;

    const foundation::Basis3f& get_closure_shading_basis(const size_t index) const;

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

  protected:
    enum { MaxClosureEntries = 16 };

    size_t                          m_closure_count;
    void*                           m_input_values[MaxClosureEntries];
    ClosureID                       m_closure_types[MaxClosureEntries];
    Spectrum                        m_weights[MaxClosureEntries];
    float                           m_cdf[MaxClosureEntries];
    float                           m_pdf_weights[MaxClosureEntries];
    foundation::Basis3f             m_bases[MaxClosureEntries];

    CompositeClosure();

    void compute_cdf();

    template <typename InputValues>
    InputValues* do_add_closure(
        const ClosureID             closure_type,
        const foundation::Basis3f&  original_shading_basis,
        const foundation::Color3f&  weight,
        const foundation::Vector3f& normal,
        const bool                  has_tangent,
        const foundation::Vector3f& tangent,
        foundation::Arena&          arena);
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

    void add_ior(
        const foundation::Color3f&  weight,
        const float                 ior);

    float choose_ior(const float w) const;

  private:
    size_t                          m_ior_count;
    float                           m_iors[MaxClosureEntries];
    float                           m_ior_cdf[MaxClosureEntries];

    void process_closure_tree(
        const OSL::ClosureColor*    closure,
        const foundation::Basis3f&  original_shading_basis,
        const foundation::Color3f&  weight,
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

  private:
    void process_closure_tree(
        const OSL::ClosureColor*    closure,
        const foundation::Basis3f&  original_shading_basis,
        const foundation::Color3f&  weight,
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
        const OSL::ClosureColor*    ci,
        foundation::Arena&          arena);

    template <typename InputValues>
    InputValues* add_closure(
        const ClosureID             closure_type,
        const foundation::Color3f&  weight,
        const float                 max_weight_component,
        foundation::Arena&          arena);

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
float process_holdout_tree(const OSL::ClosureColor* ci);
foundation::Color3f process_background_tree(const OSL::ClosureColor* ci);

void inject_layered_closure_values(
    const size_t    closure_id,
    const BSDF*     osl_bsdf,
    void*           data);

void register_closures(OSL::ShadingSystem& shading_system);


//
// CompositeClosure class implementation.
//

inline size_t CompositeClosure::get_closure_count() const
{
    return m_closure_count;
}

inline ClosureID CompositeClosure::get_closure_type(const size_t index) const
{
    assert(index < get_closure_count());
    return m_closure_types[index];
}

inline const Spectrum& CompositeClosure::get_closure_weight(const size_t index) const
{
    assert(index < get_closure_count());
    return m_weights[index];
}

inline float CompositeClosure::get_closure_pdf_weight(const size_t index) const
{
    assert(index < get_closure_count());
    return m_pdf_weights[index];
}

inline void* CompositeClosure::get_closure_input_values(const size_t index) const
{
    assert(index < get_closure_count());
    return m_input_values[index];
}

inline const foundation::Basis3f& CompositeClosure::get_closure_shading_basis(const size_t index) const
{
    assert(index < get_closure_count());
    return m_bases[index];
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_SHADING_CLOSURES_H
