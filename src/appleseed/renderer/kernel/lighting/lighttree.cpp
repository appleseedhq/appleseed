
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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

LightTree::LightTree(const Scene& scene)
{

}

LightTree::~LightTree()
{
    RENDERER_LOG_INFO("Deleting the light tree...");
    for(foundation::const_each<LightSourcePointerVector> i = m_light_sources; i; ++i)
    {
        delete *i;
    }
}

void LightTree::build(
        const std::vector<NonPhysicalLightInfo>     non_physical_lights,
        const std::vector<EmittingTriangle>         emitting_triangles)
{

    RENDERER_LOG_INFO("Building a tree");
    foundation::Statistics statistics;
    AABBVector light_bboxes;

    RENDERER_LOG_INFO("Collecting light sources...");
    size_t light_index = 0;

    // Collect all possible light sources into one vector
    for (foundation::const_each<NonPhysicalLightVector> i = non_physical_lights; i; ++i)
    {
        LightSource* light_source = new NonPhysicalLightSource(&*i);
        foundation::AABB3d bbox = light_source->get_bbox();
        m_light_sources.push_back(light_source);
        light_bboxes.push_back(bbox);
        m_items.push_back(
            Item(
                bbox,
                light_index++));
    }

    for (foundation::const_each<EmittingTriangleVector> i = emitting_triangles; i; ++i)
    {
        LightSource* light_source = new EmittingTriangleLightSource(&*i);
        foundation::AABB3d bbox = light_source->get_bbox();
        m_light_sources.push_back(light_source);
        light_bboxes.push_back(bbox);
        m_items.push_back(
            Item(
                bbox,
                light_index++));
    }

    RENDERER_LOG_INFO("Number of light sources: %zu", m_light_sources.size());

    // Create the partitioner.
    typedef foundation::bvh::MiddlePartitioner<AABBVector> Partitioner;
    Partitioner partitioner(light_bboxes);
    // Build the light tree.
    typedef foundation::bvh::Builder<LightTree, Partitioner> Builder;
    Builder builder;
    builder.build<foundation::DefaultWallclockTimer>(*this, partitioner, m_items.size(), 1);
    statistics.insert_time("build time", builder.get_build_time());

    // Reorder m_items vector to match the ordering in the LightTree.
    size_t tree_depth = 0;
    if (!m_items.empty())
    {
        const std::vector<size_t>& ordering = partitioner.get_item_ordering();
        assert(m_items.size() == ordering.size());

        // Reorder the items according to the tree ordering.
        ItemVector temp_lights(ordering.size());
        foundation::small_item_reorder(
            &m_items[0],
            &temp_lights[0],
            &ordering[0],
            ordering.size());

        // Set total luminance for each node of the LightTree.
        update_luminance(0);
        tree_depth = update_level(0, 0);
    }
    
    // Print light tree statistics.
    RENDERER_LOG_INFO("%s",
        foundation::StatisticsVector::make(
            "light tree statistics",
            statistics).to_string().c_str());

    RENDERER_LOG_INFO("Number of nodes: %zu", m_nodes.size());
    RENDERER_LOG_INFO("Tree depth: %zu", tree_depth);
    

    // Vpython tree output
    const double Width = 0.1;
    const char* root_color = "color.yellow";

    // Calculate steps of a color heat map.
    const float color_map_step = 1.0 / m_nodes[0].get_node_luminance();
    for(size_t parent_level = 0; parent_level < tree_depth; parent_level++)
    {
        const std::string filename = "light_tree_level_" + std::to_string(parent_level + 1) + ".py";
        foundation::VPythonFile file(filename.c_str());
        file.draw_axes(Width);

        // Draw the initial bbox.
        const auto& root_bbox = partitioner.compute_bbox(0, m_items.size());
        file.draw_aabb(root_bbox, root_color, Width);

        // Find the parent node to draw child bboxes from.
        for(size_t i = 0; i < m_nodes.size(); i++)
        {
            if (m_nodes[i].is_leaf())
                continue;

            if (m_nodes[i].get_level() == parent_level)
            {
                const size_t node_luminance = m_nodes[i].get_node_luminance();
                const float luminance = color_map_step * node_luminance;
                // Calculate color.
                foundation::Color3f node_color(luminance, 1.0 - luminance, 0.0);
                std::string vpy_color = "("
                                        + std::to_string(node_color[0])
                                        +","
                                        + std::to_string(node_color[1])
                                        + ","
                                        + std::to_string(node_color[2])
                                        + ")";

                const auto& bbox_left = m_nodes[i].get_left_bbox();
                const auto& bbox_right = m_nodes[i].get_right_bbox();

                file.draw_aabb(bbox_left, vpy_color.c_str(), Width);
                file.draw_aabb(bbox_right, vpy_color.c_str(), Width);
            }
        }
    }
}

float LightTree::update_luminance(size_t node_index)
{
    float luminance = 0.0f;

    if (!m_nodes[node_index].is_leaf())
    {
        luminance = update_luminance(m_nodes[node_index].get_child_node_index()) // left child
                  + update_luminance(m_nodes[node_index].get_child_node_index() + 1);  // right child    
    }
    else
    {
        size_t item_index = m_nodes[node_index].get_item_index();
        size_t light_source_index = m_items[item_index].m_light_sources_index;
        Spectrum spectrum = m_light_sources[light_source_index]->get_intensity();
        for(size_t i = 0; i < spectrum.size(); i++)
        {
            luminance += spectrum[i];
        }
        luminance /= spectrum.size();
    }
   
    m_nodes[node_index].set_node_luminance(luminance);

    return luminance;
}

size_t LightTree::update_level(size_t node_index, size_t node_level)
{
    size_t tree_depth = 0;
    if (!m_nodes[node_index].is_leaf())
    {
        size_t depth1 = update_level(m_nodes[node_index].get_child_node_index(), node_level + 1); // left child
        size_t depth2 = update_level(m_nodes[node_index].get_child_node_index() + 1, node_level + 1);  // right child    

        tree_depth = depth2 < depth1 ? depth1
                                     : depth2;
    }
    else
        tree_depth = node_level;
   
    m_nodes[node_index].set_level(node_level);
    return tree_depth;
}

void LightTree::output_every_light_probability(
        size_t                        node_index,
        const foundation::Vector3d&   surface_point,
        float                         light_probability,
        float                         s) const
{
    printf("m_nodes index: %zu\n", node_index);

    if(!m_nodes[node_index].is_leaf())
    {
        printf("NOT LEAF\n");
        // LightTreeNodes
        const auto& node   = m_nodes[node_index];
        const auto& child1 = m_nodes[node.get_child_node_index()];
        const auto& child2 = m_nodes[node.get_child_node_index() + 1];

        const auto& bbox_left  = node.get_left_bbox();
        const auto& bbox_right = node.get_right_bbox();

        const float square_distance_left  = foundation::square_distance(surface_point, bbox_left.center());
        const float square_distance_right = foundation::square_distance(surface_point, bbox_right.center());
        printf("square_distance_left: %f\n",square_distance_left);
        printf("square_distance_right: %f\n",square_distance_right);

        float p1 = child1.get_probability(square_distance_left, bbox_left.radius());
        float p2 = child2.get_probability(square_distance_right, bbox_right.radius());

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
        printf("p1: %f\n",p1);
        printf("p2: %f\n",p2);

        output_every_light_probability(
            node.get_child_node_index(),
            surface_point,
            light_probability * p1,
            s / p1);

        output_every_light_probability(
            node.get_child_node_index() + 1,
            surface_point,
            light_probability * p2,
            (s - p1) / p2);
    }
    else
    {
        printf("IS LEAF\n");
        size_t item_index = m_nodes[node_index].get_item_index();
        // NOTE: this will work only for pure NPL scene as the lights in
        // m_light_sources will be mixed. Rewrite this!
        size_t light_index = m_items[item_index].m_light_sources_index;
    
        printf("light index: %zu\n", light_index);
        printf("light_probability: %f\n", light_probability);
    }
}

std::pair<size_t, float> LightTree::sample(
        const foundation::Vector3d    surface_point,
        float                         s) const
{
    float light_probability = 1.0;
    size_t node_index = 0;

    // printf("surface_point: [%f %f %f]\n", surface_point[0], surface_point[1], surface_point[2]);
    // printf("s: %f\n", s);
    // output_every_light_probability(0, surface_point, 1.0, s);

    std::pair<size_t, float> nearest_light;
    while (!m_nodes[node_index].is_leaf())
    {
        // LightTreeNodes
        const auto& node   = m_nodes[node_index];
        const auto& child1 = m_nodes[node.get_child_node_index()];
        const auto& child2 = m_nodes[node.get_child_node_index() + 1];

        const auto& bbox_left  = node.get_left_bbox();
        const auto& bbox_right = node.get_right_bbox();
        float square_distance_left  = foundation::square_distance(surface_point, bbox_left.center());
        float square_distance_right = foundation::square_distance(surface_point, bbox_right.center());

        float p1 = child1.get_probability(square_distance_left, bbox_left.radius());
        float p2 = child2.get_probability(square_distance_right, bbox_right.radius());

        float total = p1 + p2;

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
    // NOTE: this will work only for pure NPL scene as the lights in
    // m_light_sources will be mixed. Rewrite this!
    size_t light_index = m_items[item_index].m_light_sources_index;
    // printf("chosen light_probability: %f\n\n\n", light_probability);

    return std::pair<size_t, float>(light_index, light_probability);
}

}   // namespace renderer