
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

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"
#include "foundation/math/permutation.h"
#include "foundation/platform/timers.h"

// Standard headers.

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
    // Collect all possible light sources into one vector
    for (foundation::const_each<NonPhysicalLightVector> i = non_physical_lights; i; ++i)
    {
        LightSource* light_source = new NonPhysicalLightSource(&*i);
        foundation::Vector3d position = light_source->get_position();
        foundation::AABB3d bbox = light_source->get_bbox();
        m_light_sources.push_back(light_source);
        light_bboxes.push_back(bbox);
        m_items.push_back(
            Item(
                bbox,
                position));

        RENDERER_LOG_INFO("Non physical light bbox center [%f %f %f]",
                            bbox.center()[0],
                            bbox.center()[1],
                            bbox.center()[2]);
    }

    for (foundation::const_each<EmittingTriangleVector> i = emitting_triangles; i; ++i)
    {
        LightSource* light_source = new EmittingTriangleLightSource(&*i);
        foundation::Vector3d position = light_source->get_position();
        foundation::AABB3d bbox = light_source->get_bbox();
        m_light_sources.push_back(light_source);
        light_bboxes.push_back(bbox);
        m_items.push_back(
            Item(
                bbox,
                position));

        RENDERER_LOG_INFO("Emitting triangle centroid at coordinates [%f %f %f]",
                            position[0], position[1], position[2]);
        RENDERER_LOG_INFO("Emitting triangle bbox center [%f %f %f]",
                            bbox.center()[0], bbox.center()[1], bbox.center()[2]);
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
    }

    // Print light tree statistics.
    RENDERER_LOG_INFO("%s",
        foundation::StatisticsVector::make(
            "light tree statistics",
            statistics).to_string().c_str());
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

    RENDERER_LOG_INFO("leaf_count... %zu", leaf_count);
    RENDERER_LOG_INFO("fat_leaf_count... %zu", fat_leaf_count);
    statistics.insert_percent("fat leaves", fat_leaf_count, leaf_count);
}

}   // namespace renderer
