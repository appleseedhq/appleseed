
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

// Interface header.
#include "lighttree.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"
#include "foundation/math/distance.h"
#include "foundation/math/permutation.h"
#include "foundation/platform/timers.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/vpythonfile.h"

namespace renderer
{

//
// LightTree class implementation.
//

LightTree::LightTree()
  : m_tree_depth(0)
  , m_built(false)
{
}

LightTree::~LightTree()
{
    for (foundation::const_each<LightSourcePointerVector> i = m_light_sources; i; ++i)
        delete *i;
}

bool LightTree::is_built() const
{
    return m_built;
}

size_t LightTree::build(
    std::vector<NonPhysicalLightInfo>&    non_physical_lights,
    std::vector<EmittingTriangle>&        emitting_triangles)
{
    foundation::Statistics statistics;
    AABBVector light_bboxes;

    // Collect all possible light sources into one vector.
    size_t light_index = 0;
    for (size_t i = 0; i < non_physical_lights.size(); ++i)
    {
        LightSource* light_source = new NonPhysicalLightSource(&non_physical_lights[i]);
        const foundation::AABB3d bbox = light_source->get_bbox();
        m_light_sources.push_back(light_source);
        light_bboxes.push_back(bbox);
        m_items.push_back(Item(bbox, light_index++, i));
    }

    // Collect all possible light sources into one vector.
    for (size_t i = 0; i < emitting_triangles.size(); ++i)
    {
        LightSource* light_source = new EmittingTriangleLightSource(&emitting_triangles[i]);
        foundation::AABB3d bbox = light_source->get_bbox();
        m_light_sources.push_back(light_source);
        light_bboxes.push_back(bbox);
        m_items.push_back(Item(bbox, light_index++, i));
    }

    // Create the partitioner.
    typedef foundation::bvh::MiddlePartitioner<AABBVector> Partitioner;
    Partitioner partitioner(light_bboxes);

    // Build the light tree.
    typedef foundation::bvh::Builder<LightTree, Partitioner> Builder;
    Builder builder;
    builder.build<foundation::DefaultWallclockTimer>(*this, partitioner, m_items.size(), 1);
    statistics.insert("light sources", m_light_sources.size());
    statistics.insert("nodes", m_nodes.size());
    statistics.insert("max tree depth", m_tree_depth);
    statistics.insert_time("total build time", builder.get_build_time());

    // Reorder m_items vector to match the ordering in the LightTree.
    if (!m_items.empty())
    {
        m_built = true;

        const std::vector<size_t>& ordering = partitioner.get_item_ordering();
        assert(m_items.size() == ordering.size());

        // Reorder the items according to the tree ordering.
        ItemVector temp_lights(ordering.size());
        foundation::small_item_reorder(
            &m_items[0],
            &temp_lights[0],
            &ordering[0],
            ordering.size());

        // Set total node importance and level for each node of the LightTree.
        recursive_node_update(0, 0, 0);
    }

    // Print light tree statistics.
    RENDERER_LOG_INFO("%s",
        foundation::StatisticsVector::make(
            "light tree statistics",
            statistics).to_string().c_str());

    return m_light_sources.size();
}

void LightTree::draw_tree_structure(
    const std::string&          filename_base,
    const foundation::AABB3d&   root_bbox,
    const bool                  separate_by_levels) const
{
    // TODO: Add a possibility to shift each level of bboxes along the z-axis.

    const double Width = 0.1;

    if (separate_by_levels)
    {
        const char* color = "color.green";

        // Find nodes on each level of the tree and draw their child bboxes.
        for (size_t parent_level = 0; parent_level < m_tree_depth; parent_level++)
        {
            const auto filename = foundation::format("{0}_{1}.py", filename_base, parent_level + 1);
            foundation::VPythonFile file(filename.c_str());
            file.draw_axes(Width);

            // Draw the initial bbox.
            file.draw_aabb(root_bbox, color, Width);

            // Find every node at the parent level and draw its child bboxes.
            for (size_t i = 0; i < m_nodes.size(); ++i)
            {
                if (m_nodes[i].is_leaf())
                    continue;

                if (m_nodes[i].get_level() == parent_level)
                {
                    const auto& bbox_left = m_nodes[i].get_left_bbox();
                    const auto& bbox_right = m_nodes[i].get_right_bbox();

                    file.draw_aabb(bbox_left, color, Width);
                    file.draw_aabb(bbox_right, color, Width);
                }
            }
        }
    }
    else
    {
        const auto filename = foundation::format("{0}.py", filename_base);
        foundation::VPythonFile file(filename.c_str());
        file.draw_axes(Width);

        // Draw the initial bbox.
        file.draw_aabb(root_bbox, "color.yellow", Width);

        // Find nodes on each level of the tree and draw their child bboxes.
        for (size_t i = 0; i < m_nodes.size(); ++i)
        {
            if (m_nodes[i].is_leaf())
                continue;

            // Make even levels red and odd green.
            const char* color =
                m_nodes[i].get_level() % 2 != 0
                    ? "color.red"
                    : "color.green";

            const auto& bbox_left = m_nodes[i].get_left_bbox();
            const auto& bbox_right = m_nodes[i].get_right_bbox();

            file.draw_aabb(bbox_left, color, Width);
            file.draw_aabb(bbox_right, color, Width);
        }
    }
}

float LightTree::recursive_node_update(
    const size_t parent_index,
    const size_t node_index, 
    const size_t node_level)
{
    float importance = 0.0f;

    if (!m_nodes[node_index].is_leaf())
    {
        const auto& child1 = m_nodes[node_index].get_child_node_index();
        const auto& child2 = m_nodes[node_index].get_child_node_index() + 1;

        const float importance1 = recursive_node_update(node_index, child1, node_level + 1);
        const float importance2 = recursive_node_update(node_index, child2, node_level + 1);

        importance = importance1 + importance2;
    }
    else
    {
        // Access the light importance value.
        const size_t item_index = m_nodes[node_index].get_item_index();
        const size_t light_source_index = m_items[item_index].m_light_source_index;
        importance = m_light_sources[light_source_index]->get_importance();

        // Modify the tree depth if the branch is deeper than the other branches
        // visited so far.
        if (m_tree_depth < node_level)
            m_tree_depth = node_level;

        // If the light is an emitting triangle, store the info about its
        // position in the light tree.
        if (m_light_sources[light_source_index]->get_type() == LightSource::EmittingTriangleType)
            m_light_sources[light_source_index]->set_tree_index(node_index);
    }

    m_nodes[node_index].set_importance(importance);
    m_nodes[node_index].set_level(node_level);
    if (node_index != 0)
        m_nodes[node_index].set_parent(parent_index);
    else
        m_nodes[node_index].set_root();

    return importance;
}

void LightTree::sample(
    const foundation::Vector3d&     surface_point,
    float                           s,
    int&                            light_type,
    size_t&                         light_index,
    float&                          light_probability) const
{
    assert(is_built());

    light_probability = 1.0f;
    size_t node_index = 0;

    while (!m_nodes[node_index].is_leaf())
    {
        const auto& node = m_nodes[node_index];

        float p1, p2;
        child_node_probabilites(node, surface_point, p1, p2);

        if (s <= p1)
        {
            light_probability *= p1;
            s /= p1;
            node_index = node.get_child_node_index();
        }
        else
        {
            light_probability *= p2;
            s = (s - p1) / p2;
            node_index = node.get_child_node_index() + 1;
        }
    }

    const size_t item_index = m_nodes[node_index].get_item_index();
    const size_t source_index = m_items[item_index].m_light_source_index;
    light_type = m_light_sources[source_index]->get_type();
    
    switch (light_type)
    {
      // m_light_source_index corresponds to the m_light_tree_lights
      // index of the BackwardLightSampler in case of NPL, whereas the EMT index
      // needs to be stored separately for now as it is not in the same
      // vector with other light tree compatible lights.
      case LightSource::NonPhysicalLightType:
        light_index = m_items[item_index].m_light_source_index;
        break;

      case LightSource::EmittingTriangleType:
        light_index = m_items[item_index].m_external_source_index;
        break;

      default:
        assert(!"Unexpected light type.");
    }
}

float LightTree::evaluate_node_pdf(
    const foundation::Vector3d&     surface_point,
    size_t                          node_index) const
{
    size_t parent_index = m_nodes[node_index].get_parent();
    float pdf = 1.0f;

    while (!m_nodes[parent_index].is_root())
    {
        const LightTreeNode<foundation::AABB3d>& node = m_nodes[parent_index];

        float p1, p2;
        child_node_probabilites(node, surface_point, p1, p2);

        pdf *= node.get_child_node_index() == node_index ? p1 : p2;

        // Save the child index to be sure which probability should be taken
        // into consideration.
        node_index = parent_index;
        parent_index = m_nodes[node_index].get_parent();
    }

    return pdf;
}

namespace
{
    float compute_node_probability(
        const LightTreeNode<foundation::AABB3d>&    node,
        const foundation::AABB3d&                   bbox,
        const foundation::Vector3d&                 surface_point)
    {
        // Calculate probability a single node based on its distance
        // to the surface point being evaluated.
        const float squared_distance =
            static_cast<float>(foundation::square_distance(surface_point, bbox.center()));
        return node.get_importance() / squared_distance;
    }
}

void LightTree::child_node_probabilites(
    const LightTreeNode<foundation::AABB3d>&    node,
    const foundation::Vector3d&                 surface_point,
    float&                                      p1,
    float&                                      p2) const
{
    const auto& child1 = m_nodes[node.get_child_node_index()];
    const auto& child2 = m_nodes[node.get_child_node_index() + 1];

    // Node has currently no info about its own bbox characteristics.
    // Hence we have to extract it before from its parent.
    // TODO: make LightTreeNode aware of its bbox!
    const auto& bbox_left  = node.get_left_bbox();
    const auto& bbox_right = node.get_right_bbox();

    p1 = compute_node_probability(child1, bbox_left, surface_point);
    p2 = compute_node_probability(child2, bbox_right, surface_point);

    // Normalize probabilities.
    const float total = p1 + p2;
    if (total <= 0.0f)
    {
        p1 = 0.5f;
        p2 = 0.5f;
    }
    else
    {
        p1 /= total;
        p2 /= total;
    }
}

}   // namespace renderer
