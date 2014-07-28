
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
    vector<AABB3d> curve_bboxes;
    collect_curves(curve_bboxes);

    // Print statistics about the input geometry.
    RENDERER_LOG_INFO(
        "building curve tree #" FMT_UNIQUE_ID " (bvh, %s %s)...",
        m_arguments.m_curve_tree_uid,
        pretty_uint(m_curve_keys.size()).c_str(),
        plural(m_curve_keys.size(), "curve").c_str());

    // Create the partitioner.
    typedef bvh::SAHPartitioner<vector<AABB3d> > Partitioner;
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
        m_curves3.size(),
        CurveTreeDefaultMaxLeafSize);
    statistics.merge(
        bvh::TreeStatistics<CurveTree>(*this, AABB3d(m_arguments.m_bbox)));

    // Reorder the curves based on the nodes ordering.
    if (!m_curves3.empty())
    {
        const vector<size_t>& order = partitioner.get_item_ordering();

        vector<BezierCurve3d> temp_curves(m_curves3.size());
        small_item_reorder(&m_curves3[0], &temp_curves[0], &order[0], order.size());

        vector<CurveKey> temp_keys(m_curve_keys.size());
        small_item_reorder(&m_curve_keys[0], &temp_keys[0], &order[0], order.size());
    }
}

void CurveTree::collect_curves(vector<AABB3d>& curve_bboxes)
{
    const ObjectInstanceContainer& object_instances = m_arguments.m_assembly.object_instances();

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
        const size_t curve_count = curve_object.get_curve_count();
        for (size_t j = 0; j < curve_count; ++j)
        {
            const BezierCurve3d curve(curve_object.get_curve(j), transform);
            const CurveKey curve_key(i, j, 0);  // for now we assume all the curves have the same material

            AABB3d curve_bbox = curve.get_bbox();
            curve_bbox.grow(Vector3d(0.5 * curve.get_max_width()));

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
