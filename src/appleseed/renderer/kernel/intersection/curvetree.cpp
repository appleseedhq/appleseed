
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

// appleseed.foundation headers.
#include "foundation/platform/system.h"
#include "foundation/platform/timer.h"
#include "foundation/utility/makevector.h"

// Standard headers.
#include <cstring>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// CurveTree class implementation.
//

namespace
{
    void collect_curves(
        const CurveTree::Arguments& arguments,
        vector<BezierCurve3d>*      curves,
        vector<CurveKey>*           curve_keys)
    {
        for (size_t i = 0; i < arguments.m_assembly.object_instances().size(); ++i)
        {
            // Retrieve the object instance.
            const ObjectInstance* object_instance = arguments.m_assembly.object_instances().get_by_index(i);
            assert(object_instance);

            // Retrieve the object.
            const Object& object = object_instance->get_object();

            // Process only curve objects.
            if (strcmp(object.get_model(), "curve_object"))
                continue;

            // Cast the object as curve object.
            const CurveObject* curve_object = static_cast<const CurveObject*>(&object);

            // Get the curves and store them.
            const size_t curve_count = curve_object->get_curve_count();

            // Create the curve keys and store them with the curves.
            for (size_t j = 0; j < curve_count; ++j)
            {
                curves->push_back(curve_object->get_curve(j));
                curve_keys->push_back(
                    CurveKey(
                        i,      // object instance index
                        j,      // curve index
                        0));    // for now we assume all the curves have the same material
            }
        }
    }
};

CurveTree::Arguments::Arguments(
    const Scene&            scene,
    const UniqueID          curve_tree_uid,
    const GAABB3&           bbox,
    const Assembly&         assembly,
    const RegionInfoVector& regions)
  : m_scene(scene)
  , m_curve_tree_uid(curve_tree_uid)
  , m_bbox(bbox)
  , m_assembly(assembly)
  , m_regions(regions)
{
}

CurveTree::CurveTree(const Arguments& arguments)
  : TreeType(AlignedAllocator<void>(System::get_l1_data_cache_line_size()))
  , m_arguments(arguments)
{
    // Retrieve construction parameters.
    const MessageContext message_context(
        string("while building acceleration structure for assembly \"") + m_arguments.m_assembly.get_name() + "\"");
    const ParamArray& params = m_arguments.m_assembly.get_parameters().child("acceleration_structure");
    const string algorithm = params.get_optional<string>("algorithm", "bvh", make_vector("bvh", "sbvh"), message_context);
    const double time = params.get_optional<double>("time", 0.5);
    const bool save_memory = params.get_optional<bool>("save_temporary_memory", false);

    // Start stopwatch.
    Stopwatch<DefaultWallclockTimer> stopwatch;
    stopwatch.start();    

    // Build the tree.
    Statistics statistics;
    if (algorithm == "bvh")
        build_bvh(params, time, save_memory, statistics);   

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
    const bool              save_memory,
    Statistics&             statistics)
{
    vector<GAABB3> curve_bboxes;

    // Obtain the curves and curve keys from the curve object.
    collect_curves(m_arguments, &m_curves3, &m_curve_keys);

    // Create the keys and curve bboxes required.
    curve_bboxes.resize(m_curves3.size());

    for (size_t i = 0; i < m_curves3.size(); ++i)
        curve_bboxes[i] = m_curves3[i].get_bounds();

    const size_t max_leaf_size = 1;
    const GScalar interior_node_travesal_cost = 1.0f;
    const GScalar triangle_intersection_cost = 1.0f;

    // Create the partitioner.
    typedef bvh::SAHPartitioner<vector<GAABB3> > Partitioner;
    Partitioner partitioner(
        curve_bboxes,
        max_leaf_size,
        interior_node_travesal_cost,
        triangle_intersection_cost);

    // Build the tree.
    typedef bvh::Builder<CurveTree, Partitioner> Builder;
    Builder builder;
    builder.build<DefaultWallclockTimer>(*this, partitioner, m_curves3.size(), max_leaf_size);

    // Bounding boxes are no longer needed.
    clear_release_memory(curve_bboxes);

    // Reorder the curves based upon the node indices.
    vector<BezierCurve3d> m_temp(m_curves3.size());
    vector<CurveKey> m_temp_keys(m_curve_keys.size());
    const vector<size_t>& order = partitioner.get_item_ordering();

    // Try to perform a reorder only if there is some data available.
    if (!m_curves3.empty())
    {
        small_item_reorder<BezierCurve3d, size_t>(&m_curves3[0], &m_temp[0], &order[0], order.size());
        small_item_reorder<CurveKey, size_t>(&m_curve_keys[0], &m_temp_keys[0], &order[0], order.size());
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
