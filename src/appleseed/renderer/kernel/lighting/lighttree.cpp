
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

void LightTree::build(
    const std::vector<NonPhysicalLightInfo>&    non_physical_lights)
{
    foundation::Statistics statistics;
    AABBVector light_bboxes;

    // Collect all possible light sources into one vector.
    for (size_t i = 0; i < non_physical_lights.size(); ++i)
    {
        LightSource* light_source = new NonPhysicalLightSource(&non_physical_lights[i]);
        foundation::AABB3d bbox = light_source->get_bbox();
        m_light_sources.push_back(light_source);
        light_bboxes.push_back(bbox);
        m_items.push_back(
            Item(
                bbox,
                i));
    }

    // Create the partitioner.
    typedef foundation::bvh::MiddlePartitioner<AABBVector> Partitioner;
    Partitioner partitioner(light_bboxes);

    // Build the light-tree.
    typedef foundation::bvh::Builder<LightTree, Partitioner> Builder;
    Builder builder;
    builder.build<foundation::DefaultWallclockTimer>(*this, partitioner, m_items.size(), 1);
    statistics.insert_time("build time", builder.get_build_time());

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

        // Set total luminance and level for each node of the LightTree.
        recursive_node_update(0, 0);
    }
    
    // Print light-tree statistics.
    RENDERER_LOG_INFO("%s",
        foundation::StatisticsVector::make(
            "light-tree statistics",
            statistics).to_string().c_str());

    RENDERER_LOG_INFO("number of light sources: %zu", m_light_sources.size());
    RENDERER_LOG_INFO("number of nodes: %zu", m_nodes.size());
    RENDERER_LOG_INFO("tree depth: %zu", m_tree_depth);
}

//
// Control output of the built tree bboxes into the VPython file.
// The py file is written to the cwd.
// TODO: Add a possibility to shift each level of bboxes along the z-axis.
//

void LightTree::draw_tree_structure(
    const std::string &         filename_base,
    const foundation::AABB3d&   root_bbox,
    const bool                  separate_by_levels) const
{
    // Vpython tree output
    const double Width = 0.1;

    if (separate_by_levels)
    {
        const char* color = "color.green";
        for (size_t parent_level = 0; parent_level < m_tree_depth; parent_level++)
        {
            const std::string filename = filename_base
                                       + "_"
                                       + std::to_string(parent_level + 1)
                                       + ".py";
            foundation::VPythonFile file(filename.c_str());
            file.draw_axes(Width);

            // Draw the initial bbox.
            file.draw_aabb(root_bbox, color, Width);

            // Find the parent node to draw child bboxes from.
            for (size_t i = 0; i < m_nodes.size(); i++)
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
        const std::string filename = filename_base + ".py";
        foundation::VPythonFile file(filename.c_str());
        file.draw_axes(Width);
        const char* color = "color.yellow";

        // Draw the initial bbox.
        file.draw_aabb(root_bbox, color, Width);

        // Find the parent node to draw child bboxes from.
        for (size_t i = 0; i < m_nodes.size(); i++)
        {
            if (m_nodes[i].is_leaf())
                continue;

            // Make even levels red and odd green
            color = m_nodes[i].get_level() % 2 ? "color.red"
                                               : "color.green";

            const auto& bbox_left = m_nodes[i].get_left_bbox();
            const auto& bbox_right = m_nodes[i].get_right_bbox();

            file.draw_aabb(bbox_left, color, Width);
            file.draw_aabb(bbox_right, color, Width);
        }
    }
}

//
// Calculate the tree depth and assign total luminance to each node of the tree.
// Total luminance represents the sum of all its child nodes luminances.
//

float LightTree::recursive_node_update(size_t node_index, size_t node_level)
{
    float luminance = 0.0f;

    if (!m_nodes[node_index].is_leaf())
    {
        const auto& child1 = m_nodes[node_index].get_child_node_index();
        const auto& child2 = m_nodes[node_index].get_child_node_index() + 1;

        float luminance1 = recursive_node_update(child1, node_level + 1);
        float luminance2 = recursive_node_update(child2, node_level + 1);

        luminance = luminance1 + luminance2;
    }
    else
    {
        // Access the light intensity value.
        size_t item_index = m_nodes[node_index].get_item_index();
        size_t light_source_index = m_items[item_index].m_light_index;
        Spectrum spectrum = m_light_sources[light_source_index]->get_intensity();

        // Luminance is the average accross all the spectrum channels.
        for (size_t i = 0; i < spectrum.size(); i++)
        {
            luminance += spectrum[i];
        }
        luminance /= spectrum.size();

        // Modify the tree depth if the branch is deeper than the other branches
        // visited so far.
        if (m_tree_depth < node_level)
            m_tree_depth = node_level;
    }

    m_nodes[node_index].set_luminance(luminance);
    m_nodes[node_index].set_level(node_level);

    return luminance;
}

std::pair<size_t, float> LightTree::sample(
    const foundation::Vector3d&     surface_point,
    float                           s) const
{
    float light_probability = 1.0;
    size_t node_index = 0;

    while (!m_nodes[node_index].is_leaf())
    {
        // LightTreeNode.
        const auto& node = m_nodes[node_index];

        std::pair<float, float> result = child_node_probabilites(node, surface_point);
        const float p1 = result.first;
        const float p2 = result.second;

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
    size_t item_index = m_nodes[node_index].get_item_index();
    size_t light_index = m_items[item_index].m_light_index;

    return std::pair<size_t, float>(light_index, light_probability);
}

namespace
{
    float node_probability(
            const LightTreeNode<foundation::AABB3d>&    node,
            const foundation::AABB3d&                   bbox,
            const foundation::Vector3d&                 surface_point)
    {
        // Calculate probabiliy a single node based on its distance
        // to the surface point being evaluated.
        const float squared_distance = foundation::square_distance(surface_point, bbox.center());
        const float inverse_distance_falloff = 1.0f / squared_distance;

        return node.get_luminance() * inverse_distance_falloff;
    }    
}

std::pair<float, float> LightTree::child_node_probabilites(
    const LightTreeNode<foundation::AABB3d>&    node,
    const foundation::Vector3d&                 surface_point) const
{
    const auto& child1 = m_nodes[node.get_child_node_index()];
    const auto& child2 = m_nodes[node.get_child_node_index() + 1];

    // Node has currently no info about its own bbox characteristics.
    // Hence we have to extract it before from its parent.
    // TODO: make LightTreeNode aware of its bbox!
    const auto& bbox_left  = node.get_left_bbox();
    const auto& bbox_right = node.get_right_bbox();

    float p1 = node_probability(child1, bbox_left, surface_point);
    float p2 = node_probability(child2, bbox_right, surface_point);

    // Normalize probabilities.
    const float total = p1 + p2;
    if (total <= 0.0f)
    {
        p1 = 0.5;
        p2 = 0.5;
    }
    else
    {
        p1 = p1 / total;
        p2 = p2 / total;
    }

    return std::pair<float, float>(p1, p2);
}

}   // namespace renderer
