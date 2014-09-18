
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014 Srinath Ravichandran, The appleseedhq Organization
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
#include "curvetree.h"

// appleseed.renderer headers.
#include "renderer/modeling/object/curveobject.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/utility/messagecontext.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionnotimplemented.h"
#include "foundation/math/transform.h"
#include "foundation/platform/defaulttimers.h"
#include "foundation/platform/system.h"
#include "foundation/utility/makevector.h"
#include "foundation/utility/stopwatch.h"

// Standard headers.
#include <cassert>
#include <cstring>
#include <string>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// CurveTree class implementation.
//

CurveTree::Arguments::Arguments(
    const Scene&            scene,
    const UniqueID          curve_tree_uid,
    const GAABB3&           bbox,
    const Assembly&         assembly)
  : m_scene(scene)
  , m_curve_tree_uid(curve_tree_uid)
  , m_bbox(bbox)
  , m_assembly(assembly)
{
}

CurveTree::CurveTree(const Arguments& arguments)
  : TreeType(AlignedAllocator<void>(System::get_l1_data_cache_line_size()))
  , m_arguments(arguments)
{
    // Retrieve construction parameters.
    const MessageContext message_context(
        string("while building curve tree for assembly \"") + m_arguments.m_assembly.get_name() + "\"");
    const ParamArray& params = m_arguments.m_assembly.get_parameters().child("acceleration_structure");
    const string algorithm = params.get_optional<string>("algorithm", "bvh", make_vector("bvh", "sbvh"), message_context);
    const double time = params.get_optional<double>("time", 0.5);

    // Start stopwatch.
    Stopwatch<DefaultWallclockTimer> stopwatch;
    stopwatch.start();

    // Build the tree.
    Statistics statistics;
    if (algorithm == "bvh")
        build_bvh(params, time, statistics);
    else throw ExceptionNotImplemented();

    // Print curve tree statistics.
    statistics.insert_size("nodes alignment", alignment(&m_nodes[0]));
    statistics.insert_time("total time", stopwatch.measure().get_seconds());
    RENDERER_LOG_DEBUG("%s",
        StatisticsVector::make(
            "curve tree #" + to_string(m_arguments.m_curve_tree_uid) + " statistics",
            statistics).to_string().c_str());
}

void CurveTree::build_bvh(
    const ParamArray&       params,
    const double            time,
    Statistics&             statistics)
{
    // Collect curves for this tree.
    RENDERER_LOG_INFO(
        "collecting geometry for curve tree #" FMT_UNIQUE_ID " from assembly \"%s\"...",
        m_arguments.m_curve_tree_uid,
        m_arguments.m_assembly.get_name());
    vector<GAABB3> curve_bboxes;
    collect_curves(curve_bboxes);

    // Print statistics about the input geometry.
    RENDERER_LOG_INFO(
        "building curve tree #" FMT_UNIQUE_ID " (bvh, %s %s)...",
        m_arguments.m_curve_tree_uid,
        pretty_uint(m_curve_keys.size()).c_str(),
        plural(m_curve_keys.size(), "curve").c_str());

    // Create the partitioner.
    typedef bvh::SAHPartitioner<vector<GAABB3> > Partitioner;
    Partitioner partitioner(
        curve_bboxes,
        CurveTreeDefaultMaxLeafSize,
        CurveTreeDefaultInteriorNodeTraversalCost,
        CurveTreeDefaultCurveIntersectionCost);

    // Build the tree.
    typedef bvh::Builder<CurveTree, Partitioner> Builder;
    Builder builder;
    builder.build<DefaultWallclockTimer>(
        *this,
        partitioner,
        m_curves1.size() + m_curves3.size(),
        CurveTreeDefaultMaxLeafSize);
    statistics.merge(
        bvh::TreeStatistics<CurveTree>(*this, m_arguments.m_bbox));

    // Reorder the curve keys based on the nodes ordering.
    // The curves are then reordered within each leaf node.
    if (!m_curves1.empty() || !m_curves3.empty())
    {
        const vector<size_t>& order = partitioner.get_item_ordering();

        // Reorder they keys first.
        vector<CurveKey> temp_keys(m_curve_keys.size());
        small_item_reorder(&m_curve_keys[0], &temp_keys[0], &order[0], order.size());

        vector<CurveType1> temp_curves1(m_curves1.size());
        vector<CurveType3> temp_curves3(m_curves3.size());
        vector<CurveKey>   new_keys(temp_keys.size());

        size_t curve1_index = 0;
        size_t curve3_index = 0;

        // Reorder the curves in the original list based on type.
        for (size_t i = 0; i < order.size(); i++)
        {
            const CurveKey& key = m_curve_keys[i];
            if (key.get_curve_degree() == 1)
            {
                temp_curves1[curve1_index] = m_curves1[key.get_curve_index_tree()];
                new_keys[i] = CurveKey(key.get_object_instance_index(),
                                       key.get_curve_index(),
                                       curve1_index,
                                       key.get_curve_pa(),
                                       1);
                curve1_index++;
            }
            else if (key.get_curve_degree() == 3)
            {
                temp_curves3[curve3_index] = m_curves3[key.get_curve_index_tree()];
                new_keys[i] = CurveKey(key.get_object_instance_index(),
                                       key.get_curve_index(),
                                       curve3_index,
                                       key.get_curve_pa(),
                                       3);
                curve3_index++;
            }
        }

        // Sanity check to see if we have correctly reordered the keys.
        assert(curve1_index == m_curves1.size());
        assert(curve3_index == m_curves3.size());

        // Write curves and curve keys back to the original list.
        for (size_t i = 0; i < m_curves1.size(); i++)
            m_curves1[i] = temp_curves1[i];
        for (size_t i = 0; i < m_curves3.size(); i++)
            m_curves3[i] = temp_curves3[i];
        for (size_t i = 0; i < new_keys.size(); i++)
            m_curve_keys[i] = new_keys[i];

        // We now add extra data to the nodes so that elements in a leaf can be sequentially accessed.
        size_t degree1_curves_offset = 0;
        size_t degree3_curves_offset = 0;

        for (size_t i = 0; i < m_nodes.size(); i++)
        {
            if (m_nodes[i].is_leaf())
            {
                size_t num_elements = m_nodes[i].get_item_count();
                size_t key_index = m_nodes[i].get_item_index();

                std::vector<CurveKey> m_temp_curve1_keys;
                std::vector<CurveKey> m_temp_curve3_keys;
                
                // Get the number of degree 1 and degree 3 curves.
                // user_data is of the form [degree1_cnt, degree1_curves_offset, degree3_cnt, degree3_curves_offset].
                // Also reorder the keys within the node so that degree 1 curves keys come first and degree 3 curves come last.
                Vector4u user_data(0, 0, 0, 0);

                for (size_t j = 0; j < num_elements; j++)
                {
                    const CurveKey& key = m_curve_keys[key_index + j];

                    if (key.get_curve_degree() == 1)
                    {
                        (user_data[0])++;
                        m_temp_curve1_keys.push_back(key);
                    }
                    else
                    {
                        (user_data[2])++;
                        m_temp_curve3_keys.push_back(key);
                    }
                }

                // We have the count. We can decide the locations of the elements within the node.
                user_data[1] += degree1_curves_offset;
                user_data[3] += degree3_curves_offset;

                Vector4u& data = m_nodes[i].get_user_data<Vector4u>();
                data = user_data;

                // Update the offset.
                degree1_curves_offset += user_data[0];
                degree3_curves_offset += user_data[2];

                // Reorder the curve keys in the original list.
                size_t offset = 0;
                for (size_t curve1 = 0; curve1 < m_temp_curve1_keys.size(); curve1++)
                {
                    m_curve_keys[key_index + offset] = m_temp_curve1_keys[curve1];
                    offset++;
                }

                for (size_t curve3 = 0; curve3 < m_temp_curve3_keys.size(); curve3++)
                {
                    m_curve_keys[key_index + offset] = m_temp_curve3_keys[curve3];
                    offset++;
                }
            }
        }
    }
}

void CurveTree::collect_curves(vector<GAABB3>& curve_bboxes)
{
    const ObjectInstanceContainer& object_instances = m_arguments.m_assembly.object_instances();

    size_t index = 0;
    for (size_t i = 0; i < object_instances.size(); ++i)
    {
        // Retrieve the object instance.
        const ObjectInstance* object_instance = object_instances.get_by_index(i);
        assert(object_instance);

        // Retrieve the object.
        const Object& object = object_instance->get_object();

        // Process only curve objects.
        if (strcmp(object.get_model(), CurveObjectFactory::get_model()))
            continue;

        // Retrieve the object instance transform.
        const Transformd::MatrixType& transform =
            object_instance->get_transform().get_local_to_parent();

        // Store the curves, curve keys and curve bounding boxes.
        const CurveObject& curve_object = static_cast<const CurveObject&>(object);

        // Store degree 1 curves.
        const size_t curve1_count = curve_object.get_curve1_count();
        for (size_t j = 0; j < curve1_count; ++j)
        {
            const CurveType1 curve(curve_object.get_curve1(j), transform);
            const CurveKey curve_key(i, j, m_curves1.size(), 0, 1);

            GAABB3 curve_bbox = curve.compute_bbox();
            curve_bbox.grow(GVector3(GScalar(0.5) * curve.compute_max_width()));

            m_curves1.push_back(curve);
            m_curve_keys.push_back(curve_key);
            curve_bboxes.push_back(curve_bbox);
        }

        // Store degree 3 curves.
        const size_t curve3_count = curve_object.get_curve3_count();
        for (size_t j = 0; j < curve3_count; ++j)
        {
            const CurveType3 curve(curve_object.get_curve3(j), transform);
            const CurveKey curve_key(i, j, m_curves3.size(), 0, 3);  // for now we assume all the curves have the same material

            GAABB3 curve_bbox = curve.compute_bbox();
            curve_bbox.grow(GVector3(GScalar(0.5) * curve.compute_max_width()));

            m_curves3.push_back(curve);
            m_curve_keys.push_back(curve_key);
            curve_bboxes.push_back(curve_bbox);
        }
    }
}


//
// CurveTreeFactory class implementation.
//

CurveTreeFactory::CurveTreeFactory(const CurveTree::Arguments& arguments)
  : m_arguments(arguments)
{
}

auto_ptr<CurveTree> CurveTreeFactory::create()
{
    return auto_ptr<CurveTree>(new CurveTree(m_arguments));
}

}   // namespace renderer
