
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

#ifndef APPLESEED_RENDERER_KERNEL_LIGHTING_LIGHTTREE_H
#define APPLESEED_RENDERER_KERNEL_LIGHTING_LIGHTTREE_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/lighting/lighttree_node.h"
#include "renderer/kernel/lighting/lighttypes.h"

// appleseed.foundation headers.
#include "foundation/math/aabb.h"
#include "foundation/math/bvh.h"
#include "foundation/math/cdf.h"
#include "foundation/utility/alignedvector.h"
#include "foundation/utility/statistics.h"
#include "foundation/utility/uid.h"

// Standard headers.
#include <cstddef>

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
    LightTree();

    // Destructor.
    ~LightTree();

    bool is_built() const;

    // Build the tree based on the lights collected by the BackwardLightSampler.
    size_t build(
        std::vector<NonPhysicalLightInfo>&  non_physical_lights,
        std::vector<EmittingTriangle>&      emitting_triangles);
    
    void sample(
        const foundation::Vector3d&         surface_point,
        const float                         s,
        int&                                light_type,
        size_t&                             light_index,
        float&                              light_probability) const;

    // Compute the light probability of a particular tree node. Start from the
    // node and go backwards towards the root node.
    float evaluate_node_pdf(
        const foundation::Vector3d&         surface_point,
        const size_t                        node_index) const;

  private:
    struct Item
    {
        foundation::AABB3d      m_bbox;
        size_t                  m_light_source_index;
        size_t                  m_external_source_index;

        Item() {}

        // Item contains bbox and source index of each light source.
        // source_index represents the light index in m_light_sources vector.
        // external_source_index represents the light index in light_tree_lights
        // and emitting_triangles vectors within the BackwardLightSampler.
        Item(
            const foundation::AABB3d&       bbox,
            const size_t                    source_index,
            const size_t                    external_source_index) 
            : m_bbox(bbox)
            , m_light_source_index(source_index)
            , m_external_source_index(external_source_index)
        {
        }
    };

    typedef std::vector<NonPhysicalLightInfo>       NonPhysicalLightVector;
    typedef std::vector<LightSource*>               LightSourcePointerVector;
    typedef std::vector<Item>                       ItemVector;

    LightSourcePointerVector   m_light_sources;
    ItemVector                 m_items;
    size_t                     m_tree_depth;
    bool                       m_built; // Was the tree built?

    // Dump the tree bounding boxes to a VPython file on disk.
    void draw_tree_structure(
        const std::string&                  filename_base,
        const foundation::AABB3d&           root_bbox,
        const bool                          separate_by_levels = false) const;

    // Calculate the tree depth.
    // Assign total luminance to each node of the tree, where total luminance
    // represents the sum of all its child nodes luminances.
    float recursive_node_update(
        const size_t                        parent_index,
        const size_t                        node_index, 
        const size_t                        node_level);

    void child_node_probabilites(
        const LightTreeNode<foundation::AABB3d>&    node,
        const foundation::Vector3d&                 surface_point,
        float&                                      p1,
        float&                                      p2) const;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_LIGHTING_LIGHTTREE_H
