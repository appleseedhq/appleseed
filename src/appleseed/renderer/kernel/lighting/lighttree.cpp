
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
#include "foundation/math/distance.h"
#include "foundation/math/permutation.h"
#include "foundation/platform/timers.h"
#include "foundation/utility/foreach.h"

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

        // Test output
        RENDERER_LOG_INFO("Non physical light index: %zu", light_index-1);
        Spectrum spectrum = light_source->get_intensity();
        RENDERER_LOG_INFO("Non physical light intensity: %f", spectrum[0]);
        RENDERER_LOG_INFO("Non physical light bbox center [%f %f %f]",
                            bbox.center()[0],
                            bbox.center()[1],
                            bbox.center()[2]);
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

    FILE* f = fopen("sorting_test.txt", "wt"); // in /sandbox/bin/Release/test.txt
    fclose(f);

    // Create the partitioner.
    typedef foundation::bvh::MiddlePartitioner<AABBVector> Partitioner;
    Partitioner partitioner(light_bboxes);
    // Build the light tree.
    typedef foundation::bvh::Builder<LightTree, Partitioner> Builder;
    Builder builder;
    builder.build<foundation::DefaultWallclockTimer>(*this, partitioner, m_items.size(), 1);
    statistics.insert_time("build time", builder.get_build_time());

    // Reorder m_items vector to match the ordering in the LightTree
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

        // Store the items in the tree leaves whenever possible.
        store_items_in_leaves(statistics);
    
        // Set total energy for each node of the LightTree
        update_nodes_energy();
    }
    
    // Print light tree statistics.
    RENDERER_LOG_INFO("%s",
        foundation::StatisticsVector::make(
            "light tree statistics",
            statistics).to_string().c_str());

    RENDERER_LOG_INFO("Number of nodes: %zu", m_nodes.size());
}

void LightTree::store_items_in_leaves(foundation::Statistics& statistics)
{
    size_t leaf_count = 0;
    size_t fat_leaf_count = 0;

    const size_t node_count = m_nodes.size();

    for (size_t i = 0; i < node_count; ++i)
    {
        NodeType& node = m_nodes[i];

        if (node.is_leaf())
        {
            ++leaf_count;

            const size_t item_count = node.get_item_count();

            if (item_count <= NodeType::MaxUserDataSize / sizeof(Item))
            {
                ++fat_leaf_count;

                const size_t item_begin = node.get_item_index();
                Item* user_data = &node.get_user_data<Item>();

                for (size_t j = 0; j < item_count; ++j)
                    user_data[j] = m_items[item_begin + j];
            }
        }
    }
}

void LightTree::update_nodes_energy()
{
    // Make sure the tree was built.
    assert(!tree.m_nodes.empty());

    // Check if the scene has only one light
    if(m_nodes[0].is_leaf())
    {   
        size_t item_index = m_nodes[0].get_item_index();
        size_t light_source_index = m_items[item_index].m_light_sources_index;
        float energy = m_light_sources[light_source_index]->get_intensity()[0]; 
        m_nodes[0].set_node_energy(energy);
        return;
    }

    // Start energy update with the root node
    float energy = update_energy(m_nodes[0].get_child_node_index()) // left child
                 + update_energy(m_nodes[0].get_child_node_index() + 1);  // right child

    m_nodes[0].set_node_energy(energy);
}

float LightTree::update_energy(size_t node_index)
{
    float energy = 0;

    if (!m_nodes[node_index].is_leaf())
    {
        energy = update_energy(m_nodes[node_index].get_child_node_index()) // left child
               + update_energy(m_nodes[node_index].get_child_node_index() + 1);  // right child    
        m_nodes[node_index].set_node_energy(energy);        
    }
    else
    {
        size_t item_index = m_nodes[node_index].get_item_index();
        size_t light_source_index = m_items[item_index].m_light_sources_index;
        energy = m_light_sources[light_source_index]->get_intensity()[0];
        m_nodes[node_index].set_node_energy(energy);
    }
    return energy;
}

// Return the nearest light and it's probability 
std::pair<size_t, float> LightTree::sample(
        const foundation::Vector3d    surface_point,
        const foundation::Vector3f&   s) const
{
    float light_probability = 1.0;
    std::pair<size_t, float> nearest_light = find_nearest_light(
                                                surface_point,
                                                0,
                                                s[0],
                                                light_probability);

    size_t light_index = nearest_light.first;
    light_probability = nearest_light.second;

    return std::pair<size_t, float>(light_index, light_probability);
}

std::pair<size_t, float> LightTree::find_nearest_light(
        const foundation::Vector3d      surface_point,
        const size_t                    node_index,
        float                           random_criteria,
        float                           total_probability) const
{
    std::pair<size_t, float> nearest_light;

    if (!m_nodes[node_index].is_leaf())
    {
        const LightTreeNode<foundation::AABB3d>& node = m_nodes[node_index];
        const LightTreeNode<foundation::AABB3d>& child1 = m_nodes[node.get_child_node_index()];
        const LightTreeNode<foundation::AABB3d>& child2 = m_nodes[node.get_child_node_index() + 1];

        const foundation::AABB3d& bbox_left  = node.get_left_bbox();
        const foundation::AABB3d& bbox_right = node.get_right_bbox();

        const float distance_left  = foundation::square_distance(surface_point, bbox_left.center());
        const float distance_right = foundation::square_distance(surface_point, bbox_right.center());

        float p1 = child1.get_probability(distance_left, bbox_left.radius());
        float p2 = child2.get_probability(distance_right, bbox_right.radius());
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

        if (random_criteria <= p1)
        {
            total_probability *= p1;
            random_criteria /= p1;
            nearest_light = find_nearest_light(surface_point,
                                               node.get_child_node_index(),
                                               random_criteria,
                                               total_probability);
        }
        else
        {
            total_probability *= p2;
            random_criteria /= p2;
            nearest_light = find_nearest_light(surface_point,
                                               node.get_child_node_index() + 1,
                                               random_criteria,
                                               total_probability);
        }
    }
    else
    {
        size_t item_index = m_nodes[node_index].get_item_index();
        // NOTE: this will work only for pure NPL scene as the lights in
        // m_light_sources will be mixed. Rewrite this!
        nearest_light.first = m_items[item_index].m_light_sources_index;
        nearest_light.second = total_probability;
    }

    return nearest_light;
}

}   // namespace renderer