
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Petra Gospodnetic, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_KERNEL_LIGHTING_LIGHTSAMPLERBASE_H
#define APPLESEED_RENDERER_KERNEL_LIGHTING_LIGHTSAMPLERBASE_H

// appleseed.renderer headers.
#include "renderer/kernel/lighting/lighttypes.h"
#include "renderer/modeling/scene/containers.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/cdf.h"

// Forward declarations.
namespace renderer  { class Assembly; }
namespace renderer  { class AssemblyInstance; }
namespace renderer  { class MaterialArray; }

namespace renderer
{

//
// LightSamplerBase class implementation.
//
// The LightSamplerBase contains function used by both BackwardLightSampler and
// ForwardLightSampler classes.
//

class LightSamplerBase
  : public foundation::NonCopyable
{
  public:
    // Constructor.
    LightSamplerBase(const ParamArray& params = ParamArray());

    // Return the number of non-physical lights in the scene.
    size_t get_non_physical_light_count() const;
  
  protected:
    struct Parameters
    {
        const bool m_importance_sampling;

        explicit Parameters(const ParamArray& params);
    };

    const Parameters            m_params;

    typedef std::vector<NonPhysicalLightInfo>   NonPhysicalLightVector;
    typedef std::vector<EmittingTriangle>       EmittingTriangleVector;
    typedef foundation::CDF<size_t, float>      EmitterCDF;

    NonPhysicalLightVector      m_light_tree_lights;
    NonPhysicalLightVector      m_non_physical_lights;
    EmittingTriangleVector      m_emitting_triangles;
    
    size_t                      m_non_physical_light_count;
    
    EmitterCDF                  m_non_physical_lights_cdf;
    EmitterCDF                  m_emitting_triangles_cdf;

    bool                        m_use_light_tree;

    // Recursively collect emitting triangles from a given set of assembly instances.
    void collect_emitting_triangles(
        const AssemblyInstanceContainer&    assembly_instances,
        const TransformSequence&            parent_transform_seq);

    // Collect emitting triangles from a given assembly.
    void collect_emitting_triangles(
        const Assembly&                     assembly,
        const AssemblyInstance&             assembly_instance,
        const TransformSequence&            transform_sequence);

    // Recursively collect non-physical lights from a given set of assembly instances.
    void collect_non_physical_lights(
        const AssemblyInstanceContainer&    assembly_instances,
        const TransformSequence&            parent_transform_seq);

    // Collect non-physical lights from a given assembly.
    void collect_non_physical_lights(
        const Assembly&                     assembly,
        const TransformSequence&            transform_sequence);

    void store_object_area_in_shadergroups(
        const AssemblyInstance*             assembly_instance,
        const ObjectInstance*               object_instance,
        const float                         object_area,
        const MaterialArray&                materials);
};


//
// LightSamplerBase class implementation.
//

inline size_t LightSamplerBase::get_non_physical_light_count() const
{
    return m_non_physical_light_count;
}


}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_LIGHTING_LIGHTSAMPLERBASE_H

