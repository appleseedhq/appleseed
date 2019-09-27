
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Petra Gospodnetic, The appleseedhq Organization
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
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/light/light.h"
#include "renderer/modeling/material/material.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"
#include "foundation/math/distance.h"
#include "foundation/math/permutation.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/timers.h"
#include "foundation/utility/vpythonfile.h"

// Standard headers.
#include <cassert>
#include <cmath>
#include <limits>

using namespace foundation;

namespace renderer
{

//
// LightTree class implementation.
//

LightTree::LightTree(
    const std::vector<NonPhysicalLightInfo>&      non_physical_lights,
    const std::vector<EmittingShape>&             emitting_shapes)
  : m_non_physical_lights(non_physical_lights)
  , m_emitting_shapes(emitting_shapes)
  , m_tree_depth(0)
  , m_is_built(false)
{
}

std::vector<size_t> LightTree::build()
{
    AABBVector light_bboxes;

    // Collect non-physical light sources.
    for (size_t i = 0, e = m_non_physical_lights.size(); i < e; ++i)
    {
        const Light* light = m_non_physical_lights[i].m_light;

        // Retrieve the exact position of the light.
        const Vector3d position = light->get_transform()
                                    .get_local_to_parent()
                                    .extract_translation();

        // Non physical light has no real size - hence some arbitrary small
        // value is assigned.
        const double BboxSize = 0.001f;
        const AABB3d bbox = AABB3d(Vector3d(position[0] - BboxSize,
                                            position[1] - BboxSize,
                                            position[2] - BboxSize),
                                   Vector3d(position[0] + BboxSize,
                                            position[1] + BboxSize,
                                            position[2] + BboxSize));
        light_bboxes.push_back(bbox);

        m_items.emplace_back(bbox, i, NonPhysicalLightType);
    }

    // Collect emitting shapes.
    for (size_t i = 0, e = m_emitting_shapes.size(); i < e; ++i)
    {
        const EmittingShape& shape = m_emitting_shapes[i];

        const AABB3d& bbox = shape.get_bbox();

        light_bboxes.push_back(bbox);
        m_items.emplace_back(bbox, i, EmittingShapeType);
    }

    // Create the partitioner.
    typedef bvh::MiddlePartitioner<AABBVector> Partitioner;
    Partitioner partitioner(light_bboxes);

    // Build the light tree.
    typedef bvh::Builder<LightTree, Partitioner> Builder;
    Builder builder;
    builder.build<DefaultWallclockTimer>(*this, partitioner, m_items.size(), 1);

    // Reorder m_items vector to match the ordering in the LightTree.
    if (!m_items.empty())
    {
        m_is_built = true;

        const std::vector<size_t>& ordering = partitioner.get_item_ordering();
        assert(m_items.size() == ordering.size());

        // Reorder items according to the tree ordering.
        ItemVector temp(ordering.size());
        small_item_reorder(
            &m_items[0],
            &temp[0],
            &ordering[0],
            ordering.size());

        // Set total node importance and level for each node of the LightTree.
        IndexLUT tri_index_to_node_index;
        tri_index_to_node_index.resize(m_emitting_shapes.size());
        recursive_node_update(0, 0, 0, tri_index_to_node_index);

        // Print light tree statistics.
        Statistics statistics;
        statistics.insert("nodes", m_nodes.size());
        statistics.insert("max tree depth", m_tree_depth);
        statistics.insert_time("total build time", builder.get_build_time());
        RENDERER_LOG_INFO("%s",
            StatisticsVector::make(
                "light tree statistics",
                statistics).to_string().c_str());

        return tri_index_to_node_index;
    }

    RENDERER_LOG_INFO("no light tree compatible lights in the scene; light tree not built.");
    return IndexLUT();
}

bool LightTree::is_built() const
{
    return m_is_built;
}

float LightTree::recursive_node_update(
    const size_t    parent_index,
    const size_t    node_index,
    const size_t    node_level,
    IndexLUT&       tri_index_to_node_index)
{
    float importance = 0.0f;

    if (!m_nodes[node_index].is_leaf())
    {
        const auto& child1 = m_nodes[node_index].get_child_node_index();
        const auto& child2 = m_nodes[node_index].get_child_node_index() + 1;

        const float importance1 = recursive_node_update(node_index, child1, node_level + 1, tri_index_to_node_index);
        const float importance2 = recursive_node_update(node_index, child2, node_level + 1, tri_index_to_node_index);

        importance = importance1 + importance2;
    }
    else
    {
        // Retrieve the light source associated to this leaf.
        const size_t item_index = m_nodes[node_index].get_item_index();
        const size_t light_index = m_items[item_index].m_light_index;

        if (m_items[item_index].m_light_type == NonPhysicalLightType)
        {
            const Light* light = m_non_physical_lights[light_index].m_light;

            // Retrieve the non-physical light importance.
            Spectrum spectrum;
            light->get_inputs().find("intensity").source()->evaluate_uniform(spectrum);
            importance = average_value(spectrum);
        }
        else
        {
            assert(m_items[item_index].m_light_type == EmittingShapeType);

            const EmittingShape& shape = m_emitting_shapes[light_index];

            // Retrieve the emitting shape importance.
            const EDF* edf = shape.get_material()->get_uncached_edf();
            assert(edf != nullptr);

            const float max_contribution = edf->get_uncached_max_contribution();

            // max_contribution is reported as std::numeric_limits<float>::max() when
            // we can't compute the max_contribution easily (ex: textured lights)
            // In such cases, we can use a default importance value of 1.0 to avoid
            // infinite importance values in the light tree nodes.
            if (max_contribution == std::numeric_limits<float>::max())
                importance = 1.0f;
            else
                importance = max_contribution * edf->get_uncached_importance_multiplier();

            // Save the index of the light tree node containing the EMT in the look up table.
            tri_index_to_node_index[light_index] = node_index;
        }

        // Keep track of the tree depth.
        if (m_tree_depth < node_level)
            m_tree_depth = node_level;
    }

    if (node_index == 0)
        m_nodes[node_index].set_root();
    else m_nodes[node_index].set_parent(parent_index);

    m_nodes[node_index].set_importance(importance);
    m_nodes[node_index].set_level(node_level);

    return importance;
}

void LightTree::sample(
    const ShadingPoint&     shading_point,
    float                   s,
    LightType&              light_type,
    size_t&                 light_index,
    float&                  light_probability) const
{
    assert(is_built());

    light_probability = 1.0f;
    size_t node_index = 0;

    while (!m_nodes[node_index].is_leaf())
    {
        const auto& node = m_nodes[node_index];

        float p1, p2;
        child_node_probabilites(node, shading_point, p1, p2);

        if (s < p1)
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
    const Item& item = m_items[item_index];
    light_type = item.m_light_type;
    light_index = item.m_light_index;
}

float LightTree::evaluate_node_pdf(
    const ShadingPoint&     shading_point,
    size_t                  node_index) const
{
    size_t parent_index = m_nodes[node_index].get_parent();
    float pdf = 1.0f;

    do
    {
        const LightTreeNode<AABB3d>& node = m_nodes[parent_index];

        float p1, p2;
        child_node_probabilites(node, shading_point, p1, p2);

        pdf *= node.get_child_node_index() == node_index ? p1 : p2;

        // Save the child index to be sure which probability should be taken
        // into consideration.
        node_index = parent_index;
        parent_index = m_nodes[node_index].get_parent();
    } while (!m_nodes[node_index].is_root());

    return pdf;
}

namespace
{
    // [1] Section 2.2.
    float sub_hemispherical_light_source_contribution(
        const float     cos_omega,
        const float     cos_sigma)
    {
        assert(cos_omega >= -1.0f && cos_omega <= 1.0f);
        assert(cos_sigma >= 0.0f && cos_sigma <= 1.0f);

        const float sin_omega = std::sqrt(1.0f - cos_omega * cos_omega);

        const float sin_sigma2 = 1.0f - (cos_sigma * cos_sigma);
        const float sin_sigma = std::sqrt(sin_sigma2);

        const float sin_gamma = cos_sigma / sin_omega;
        const float cos_gamma2 = 1.0f - (sin_gamma * sin_gamma);
        const float cos_gamma = std::sqrt(cos_gamma2);

        const float g = -2.0f * sin_omega * cos_sigma * cos_gamma
                    + HalfPi<float>()
                    - std::asin(sin_gamma)
                    + sin_gamma * cos_gamma;

        const float h =
            cos_omega * (
                cos_gamma * std::sqrt(sin_sigma2 - cos_gamma2)
                + sin_sigma2 * std::asin(cos_gamma / sin_sigma));

        const float omega = std::acos(cos_omega);
        const float sigma = std::acos(cos_sigma);

        float contribution;
        if (omega < (HalfPi<float>() - sigma))
            contribution = cos_omega * sin_sigma2;
        else if (omega < HalfPi<float>())
            contribution = cos_omega * sin_sigma2 + RcpPi<float>() * (g - h);
        else if (omega < (HalfPi<float>() + sigma))
            contribution = RcpPi<float>() * (g + h);
        else
            contribution = default_eps<float>();

        // Avoid returning zero contribution.
        if (fz(contribution))
            return default_eps<float>();
        else
            return contribution;
    }
}

float LightTree::compute_node_probability(
    const LightTreeNode<AABB3d>&    node,
    const AABB3d&                   bbox,
    const ShadingPoint&             shading_point) const
{
    // Calculate probability of a single node based on its contribution over solid angle.
    const float r2 = static_cast<float>(bbox.square_radius());
    const float rcp_surface_area = 1.0f / r2;

    // Triangle centroid is a more precise position than the center of the bbox.
    Vector3d position;
    if (node.is_leaf())
    {
        const Item& item = m_items[node.get_item_index()];
        if (item.m_light_type == EmittingShapeType)
            position = m_emitting_shapes[item.m_light_index].get_centroid();
        else position = bbox.center();
    }
    else position = bbox.center();

    const Vector3d& surface_point = shading_point.get_point();

    const float distance2 =
        static_cast<float>(square_distance(surface_point, position));

    // Evaluated point is outside the bbox.
    // The original Nathan's implementation returns importance divided by the node surface area.
    // However, replacing the surface area by the square distance showed to result in less noise.
    if (distance2 <= r2)
        return node.get_importance() / distance2;

    //
    // Implementation of Lambertian lighting model for sub-hemispherical light sources.
    // Reference:
    //  [1] Area Light Sources for Real-Time Graphics
    //      https://www.microsoft.com/en-us/research/wp-content/uploads/1996/03/arealights.pdf
    //
    const Vector3d outcoming_light_direction = normalize(bbox.center() - surface_point);
    const float sin_sigma2 = std::min(1.0f, (r2 / distance2));
    const float cos_sigma = std::sqrt(1.0f - sin_sigma2);

    const Vector3d& incoming_light_direction = shading_point.get_ray().m_dir;

    // [1] "Arbitrary direction D receives light only if dot(D,L) >= 0".
    const Vector3d& N = (dot(shading_point.get_geometric_normal(), incoming_light_direction) <= 0.0f)
        ? shading_point.get_shading_normal()
        : -shading_point.get_shading_normal();

    const float cos_omega = clamp(static_cast<float>(dot(N, outcoming_light_direction)), -1.0f, 1.0f);
    const float approx_contribution = sub_hemispherical_light_source_contribution(cos_omega, cos_sigma);

    assert(approx_contribution > 0.0f);
    return node.get_importance() * rcp_surface_area * approx_contribution;
}

void LightTree::child_node_probabilites(
    const LightTreeNode<AABB3d>&    node,
    const ShadingPoint&             shading_point,
    float&                          p1,
    float&                          p2) const
{
    const auto& child1 = m_nodes[node.get_child_node_index()];
    const auto& child2 = m_nodes[node.get_child_node_index() + 1];

    // Node has currently no info about its own bbox characteristics.
    // Hence we have to extract it before from its parent.
    // todo: make LightTreeNode aware of its bbox!
    const auto& bbox_left = node.get_left_bbox();
    const auto& bbox_right = node.get_right_bbox();

    p1 = compute_node_probability(child1, bbox_left, shading_point);
    p2 = compute_node_probability(child2, bbox_right, shading_point);

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

    assert(feq(p1 + p2, 1.0f));
}

void LightTree::draw_tree_structure(
    const std::string&       filename_base,
    const AABB3d&            root_bbox,
    const bool               separate_by_levels) const
{
    // todo: add a possibility to shift each level of bboxes along the z-axis.

    const double Width = 0.1;

    if (separate_by_levels)
    {
        const char* color = "color.green";

        // Find nodes on each level of the tree and draw their child bboxes.
        for (size_t parent_level = 0; parent_level < m_tree_depth; parent_level++)
        {
            const auto filename = format("{0}_{1}.py", filename_base, parent_level + 1);
            VPythonFile file(filename.c_str());
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
        const auto filename = format("{0}.py", filename_base);
        VPythonFile file(filename.c_str());
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

}   // namespace renderer

