
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2018 Srinath Ravichandran, The appleseedhq Organization
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
#include "renderer/global/globallogger.h"
#include "renderer/modeling/object/curveobject.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/utility/messagecontext.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionnotimplemented.h"
#include "foundation/math/beziercurve.h"
#include "foundation/math/permutation.h"
#include "foundation/math/transform.h"
#include "foundation/memory/alignedallocator.h"
#include "foundation/memory/memory.h"
#include "foundation/platform/defaulttimers.h"
#include "foundation/platform/system.h"
#include "foundation/string/string.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/makevector.h"
#include "foundation/utility/statistics.h"
#include "foundation/utility/stopwatch.h"

// Standard headers.
#include <cassert>
#include <cstring>
#include <string>

using namespace foundation;

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
        format("while building curve tree for assembly \"{0}\"", m_arguments.m_assembly.get_path()));
    const ParamArray& params = m_arguments.m_assembly.get_parameters().child("acceleration_structure");
    const std::string algorithm = params.get_optional<std::string>("algorithm", "bvh", make_vector("bvh", "sbvh"), message_context);
    const double time = params.get_optional<double>("time", 0.5);

    // Start stopwatch.
    Stopwatch<DefaultWallclockTimer> stopwatch;
    stopwatch.start();

    // Build the tree.
    Statistics statistics;
    if (algorithm == "bvh")
        build_bvh(params, time, statistics);
    else throw ExceptionNotImplemented();
    statistics.insert_time("total build time", stopwatch.measure().get_seconds());
    statistics.insert_size("nodes alignment", alignment(&m_nodes[0]));

    // Print curve tree statistics.
    RENDERER_LOG_DEBUG("%s",
        StatisticsVector::make(
            "curve tree #" + to_string(m_arguments.m_curve_tree_uid) + " statistics",
            statistics).to_string().c_str());
}

void CurveTree::collect_curves(std::vector<GAABB3>& curve_bboxes)
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
        if (strcmp(object.get_model(), CurveObjectFactory().get_model()) != 0)
            continue;

        const CurveObject& curve_object = static_cast<const CurveObject&>(object);

        // Retrieve the object instance transform.
        const Transformd::MatrixType& transform =
            object_instance->get_transform().get_local_to_parent();

        // Store degree-1 curves, curve keys and curve bounding boxes.
        const size_t curve1_count = curve_object.get_curve1_count();
        for (size_t j = 0; j < curve1_count; ++j)
        {
            const Curve1Type curve(curve_object.get_curve1(j), transform);
            const CurveKey curve_key(
                i,                  // object instance index
                j,                  // curve index in object
                m_curves1.size(),   // curve index in tree
                0,                  // for now we assume all the curves have the same material
                1);                 // curve degree

            GAABB3 curve_bbox = curve.compute_bbox();
            curve_bbox.grow(GVector3(GScalar(0.5) * curve.compute_max_width()));

            m_curves1.push_back(curve);
            m_curve_keys.push_back(curve_key);
            curve_bboxes.push_back(curve_bbox);
        }

        // Store degree-3 curves, curve keys and curve bounding boxes.
        const size_t curve3_count = curve_object.get_curve3_count();
        for (size_t j = 0; j < curve3_count; ++j)
        {
            const Curve3Type curve(curve_object.get_curve3(j), transform);
            const CurveKey curve_key(
                i,                  // object instance index
                j,                  // curve index in object
                m_curves3.size(),   // curve index in tree
                0,                  // for now we assume all the curves have the same material
                3);                 // curve degree

            GAABB3 curve_bbox = curve.compute_bbox();
            curve_bbox.grow(GVector3(GScalar(0.5) * curve.compute_max_width()));

            m_curves3.push_back(curve);
            m_curve_keys.push_back(curve_key);
            curve_bboxes.push_back(curve_bbox);
        }
    }
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
        m_arguments.m_assembly.get_path().c_str());
    std::vector<GAABB3> curve_bboxes;
    collect_curves(curve_bboxes);

    // Print statistics about the input geometry.
    RENDERER_LOG_INFO(
        "building curve tree #" FMT_UNIQUE_ID " (bvh, %s %s)...",
        m_arguments.m_curve_tree_uid,
        pretty_uint(m_curve_keys.size()).c_str(),
        plural(m_curve_keys.size(), "curve").c_str());

    // Create the partitioner.
    typedef bvh::SAHPartitioner<std::vector<GAABB3>> Partitioner;
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
    if (!m_curves1.empty() || !m_curves3.empty())
    {
        const std::vector<size_t>& ordering = partitioner.get_item_ordering();
        reorder_curve_keys(ordering);
        reorder_curves(ordering);
        reorder_curve_keys_in_leaf_nodes();
    }
}

void CurveTree::reorder_curve_keys(const std::vector<size_t>& ordering)
{
    std::vector<CurveKey> temp_keys(m_curve_keys.size());
    small_item_reorder(&m_curve_keys[0], &temp_keys[0], &ordering[0], ordering.size());
}

void CurveTree::reorder_curves(const std::vector<size_t>& ordering)
{
    std::vector<Curve1Type> new_curves1(m_curves1.size());
    std::vector<Curve3Type> new_curves3(m_curves3.size());

    size_t curve1_index = 0;
    size_t curve3_index = 0;

    for (size_t i = 0; i < ordering.size(); ++i)
    {
        const CurveKey& key = m_curve_keys[i];

        if (key.get_curve_degree() == 1)
        {
            new_curves1[curve1_index] = m_curves1[key.get_curve_index_tree()];
            m_curve_keys[i].set_curve_index_tree(curve1_index);
            ++curve1_index;
        }
        else
        {
            assert(key.get_curve_degree() == 3);
            new_curves3[curve3_index] = m_curves3[key.get_curve_index_tree()];
            m_curve_keys[i].set_curve_index_tree(curve3_index);
            ++curve3_index;
        }
    }

    assert(curve1_index == m_curves1.size());
    assert(curve3_index == m_curves3.size());

    m_curves1.swap(new_curves1);
    m_curves3.swap(new_curves3);
}

void CurveTree::reorder_curve_keys_in_leaf_nodes()
{
    for (size_t i = 0; i < m_nodes.size(); ++i)
    {
        if (!m_nodes[i].is_leaf())
            continue;

        const size_t item_index = m_nodes[i].get_item_index();
        const size_t item_count = m_nodes[i].get_item_count();

        // Collect the curve keys for this leaf node.
        std::vector<CurveKey> curve1_keys;
        std::vector<CurveKey> curve3_keys;
        for (size_t j = 0; j < item_count; ++j)
        {
            const CurveKey& key = m_curve_keys[item_index + j];
            if (key.get_curve_degree() == 1)
                curve1_keys.push_back(key);
            else curve3_keys.push_back(key);
        }

        // Store count and start offset in the leaf node's user data.
        LeafUserData& user_data = m_nodes[i].get_user_data<LeafUserData>();
        user_data.m_curve1_offset = curve1_keys.empty() ? 0 : static_cast<std::uint32_t>(curve1_keys[0].get_curve_index_tree());
        user_data.m_curve1_count = static_cast<std::uint32_t>(curve1_keys.size());
        user_data.m_curve3_offset = curve3_keys.empty() ? 0 : static_cast<std::uint32_t>(curve3_keys[0].get_curve_index_tree());
        user_data.m_curve3_count = static_cast<std::uint32_t>(curve3_keys.size());

        // Reorder the curve keys in the original list.
        size_t output_index = item_index;
        for (size_t j = 0; j < curve1_keys.size(); ++j)
            m_curve_keys[output_index++] = curve1_keys[j];
        for (size_t j = 0; j < curve3_keys.size(); ++j)
            m_curve_keys[output_index++] = curve3_keys[j];
    }
}


//
// CurveTreeFactory class implementation.
//

CurveTreeFactory::CurveTreeFactory(const CurveTree::Arguments& arguments)
  : m_arguments(arguments)
{
}

std::unique_ptr<CurveTree> CurveTreeFactory::create()
{
    return std::unique_ptr<CurveTree>(new CurveTree(m_arguments));
}

}   // namespace renderer
