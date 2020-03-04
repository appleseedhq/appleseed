
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Petra Gospodnetic, The appleseedhq Organization
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
#include "renderer/kernel/lighting/lighttree_node.h"
#include "renderer/kernel/lighting/lighttypes.h"

// appleseed.foundation headers.
#include "foundation/containers/alignedvector.h"
#include "foundation/math/aabb.h"
#include "foundation/math/bvh.h"
#include "foundation/utility/statistics.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace renderer  { class ShadingPoint; }

namespace renderer
{

//
// Light tree.
//

class LightTree
  : public foundation::bvh::Tree<
               foundation::AlignedVector<
                   LightTreeNode<foundation::AABB3d>
               >
            >
{
  public:
    // Constructor.
    // Build the tree based on the lights collected by the BackwardLightSampler.
    LightTree(
        const std::vector<NonPhysicalLightInfo>&      non_physical_lights,
        const std::vector<EmittingShape>&             emitting_shapes);

    std::vector<size_t> build();

    bool is_built() const;

    void sample(
        const ShadingPoint&             shading_point,
        const float                     s,
        LightType&                      light_type,
        size_t&                         light_index,
        float&                          light_probability) const;

    // Compute the light probability of a particular tree node. Start from the
    // node and go backwards towards the root node.
    float evaluate_node_pdf(
        const ShadingPoint&             surface_point,
        const size_t                    node_index) const;

  private:
    struct Item
    {
        foundation::AABB3d      m_bbox;
        size_t                  m_light_index;
        LightType               m_light_type;

        Item() {}

        // Item contains bbox and source index of each light source.
        // source_index represents the light index in m_light_sources vector.
        // external_source_index represents the light index in light_tree_lights
        // and emitting_shapes vectors within the BackwardLightSampler.
        Item(
            const foundation::AABB3d&       bbox,
            const size_t                    light_index,
            const LightType                 light_type)
            : m_bbox(bbox)
            , m_light_index(light_index)
            , m_light_type(light_type)
        {
        }
    };

    typedef std::vector<NonPhysicalLightInfo>       NonPhysicalLightVector;
    typedef std::vector<EmittingShape>              EmittingShapeVector;
    typedef std::vector<Item>                       ItemVector;
    typedef std::vector<size_t>                     IndexLUT;

    const NonPhysicalLightVector&                   m_non_physical_lights;
    const EmittingShapeVector&                      m_emitting_shapes;
    ItemVector                                      m_items;
    size_t                                          m_tree_depth;
    bool                                            m_is_built;

    // Calculate the tree depth.
    // Assign total importance to each node of the tree, where total importance
    // represents the sum of all its child nodes importances.
    float recursive_node_update(
        const size_t                                parent_index,
        const size_t                                node_index,
        const size_t                                node_level,
        IndexLUT&                                   tri_index_to_node_index);

    float compute_node_probability(
        const LightTreeNode<foundation::AABB3d>&    node,
        const foundation::AABB3d&                   bbox,
        const ShadingPoint&                         shading_point) const;

    void child_node_probabilites(
        const LightTreeNode<foundation::AABB3d>&    node,
        const ShadingPoint&                         shading_point,
        float&                                      p1,
        float&                                      p2) const;

    // Dump the tree bounding boxes to a VPython file on disk.
    void draw_tree_structure(
        const std::string&                          filename_base,
        const foundation::AABB3d&                   root_bbox,
        const bool                                  separate_by_levels = false) const;
};

}   // namespace renderer
