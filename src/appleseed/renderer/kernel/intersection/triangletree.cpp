
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "triangletree.h"

// appleseed.renderer headers.
#include "renderer/kernel/intersection/triangleinfo.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/object/iregion.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/object/triangle.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/objectinstance.h"

// appleseed.foundation headers.
#include "foundation/math/area.h"
#include "foundation/math/intersection.h"
#include "foundation/math/sah.h"
#include "foundation/math/split.h"
#include "foundation/math/transform.h"
#include "foundation/platform/snprintf.h"
#include "foundation/utility/maplefile.h"
#include "foundation/utility/memory.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <cstring>
#include <stack>

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    typedef vector<GAABB3> GAABB3Vector;


    //
    // Leaf of an intermediate triangle tree.
    //

    class IntermTriangleLeaf
      : public NonCopyable
    {
      public:
        // Types.
        typedef bsp::LeafInfo<GScalar, 3>   LeafInfoType;
        typedef Split<GScalar>              SplitType;

        // Constructor.
        explicit IntermTriangleLeaf(const GAABB3Vector& triangle_bboxes)
          : m_triangle_bboxes(triangle_bboxes)
        {
        }

        // Remove all triangles from the leaf.
        void clear()
        {
            clear_keep_memory(m_triangles);
        }

        // Insert a triangle into the leaf.
        void insert(const size_t triangle_index)
        {
            m_triangles.push_back(triangle_index);
        }

        // Return a given triangle.
        const size_t get_triangle(const size_t i) const
        {
            return m_triangles[i];
        }

        // Return the number of triangles in the leaf.
        size_t get_size() const
        {
            return m_triangles.size();
        }

        // Return the parent space bounding box of the leaf.
        GAABB3 get_bbox() const
        {
            GAABB3 bbox;
            bbox.invalidate();

            const size_t triangle_count = m_triangles.size();
            for (size_t i = 0; i < triangle_count; ++i)
                bbox.insert(m_triangle_bboxes[m_triangles[i]]);

            return bbox;
        }

        // Return the size (in bytes) of this object in memory.
        // Can only be implemented once TriangleLeafPacker is defined.
        size_t get_memory_size() const;

      private:
        const GAABB3Vector&     m_triangle_bboxes;
        vector<size_t>          m_triangles;            // contents of the leaf
    };


    //
    // Intermediate triangle leaf factory.
    //

    class IntermTriangleLeafFactory
      : public NonCopyable
    {
      public:
        // Constructor.
        explicit IntermTriangleLeafFactory(const GAABB3Vector& triangle_bboxes)
          : m_triangle_bboxes(triangle_bboxes)
        {
        }

        // Create a new leaf.
        IntermTriangleLeaf* create_leaf()
        {
            return new IntermTriangleLeaf(m_triangle_bboxes);
        }

      private:
        const GAABB3Vector&     m_triangle_bboxes;
    };


    //
    // Intermediate triangle leaf splitter.
    //

    class IntermTriangleLeafSplitter
      : public NonCopyable
    {
      public:
        // Constructor.
        IntermTriangleLeafSplitter(
            const TriangleInfoVector&               triangle_infos,
            const GAABB3Vector&                     triangle_bboxes)
          : m_triangle_infos(triangle_infos)
          , m_triangle_bboxes(triangle_bboxes)
        {
            // Precompute 1/exp(i).
            for (size_t i = 0; i < TriangleTreeMaxDepth; ++i)
                m_rcp_exp_depth[i] = 1.0 / exp(static_cast<double>(i));

            if (TriangleTreeTraceConstruction)
                m_tracer.reset(new Tracer());
        }

        // Return the splitting priority of a leaf.
        double get_priority(
            const IntermTriangleLeaf&               leaf,
            const IntermTriangleLeaf::LeafInfoType& leaf_info) const
        {
            const size_t size = leaf.get_size();
            return size > TriangleTreeMaxLeafSize
                ? size * m_rcp_exp_depth[leaf_info.get_node_depth()]
                : 0.0;
        }

        // Find a split.
        bool split(
            const IntermTriangleLeaf&               leaf,
            const IntermTriangleLeaf::LeafInfoType& leaf_info,
            IntermTriangleLeaf::SplitType&          split)
        {
            const size_t triangle_count = leaf.get_size();
            assert(triangle_count > 0);

            if (triangle_count <= TriangleTreeO2Threshold)
            {
                // Small leaf: use an exact SAH function.
                return exact_sah_split(leaf, leaf_info, split);
            }
            else if (triangle_count <= TriangleTreeO1Threshold)
            {
                // Medium leaf: use an approximate SAH function.
                return approximate_sah_split(leaf, leaf_info, split);
            }
            else
            {
                // Large leaf: split the leaf in the geometric middle.
                split = IntermTriangleLeaf::SplitType::middle(leaf_info.get_bbox());
                return true;
            }
        }

        // Sort a leaf into child leaves, according to a given split.
        void sort(
            const IntermTriangleLeaf&               leaf,
            const IntermTriangleLeaf::LeafInfoType& leaf_info,
            const IntermTriangleLeaf::SplitType&    split,
            IntermTriangleLeaf&                     left_leaf,
            const IntermTriangleLeaf::LeafInfoType& left_leaf_info,
            IntermTriangleLeaf&                     right_leaf,
            const IntermTriangleLeaf::LeafInfoType& right_leaf_info) const
        {
            const size_t triangle_count = leaf.get_size();
            assert(triangle_count > 0);

            if (m_tracer.get())
                m_tracer->begin_sort(triangle_count);

            // Fetch the bounding box of the leaves.
            GAABB3 left_bbox = left_leaf_info.get_bbox();
            GAABB3 right_bbox = right_leaf_info.get_bbox();

            // Prevent numerical instabilities by slightly enlarging the bounding boxes.
            const GScalar eps = bsp::get_bbox_grow_eps<GScalar>();
            left_bbox.robust_grow(eps);
            right_bbox.robust_grow(eps);

            for (size_t i = 0; i < triangle_count; ++i)
            {
                // Fetch the triangle info.
                const size_t triangle_index = leaf.get_triangle(i);
                const GAABB3& triangle_bbox = m_triangle_bboxes[triangle_index];

                // Insert the triangle into the appropriate leaves.
                if (triangle_bbox.max[split.m_dimension] <= split.m_abscissa)
                {
                    left_leaf.insert(triangle_index);
                }
                else if (triangle_bbox.min[split.m_dimension] >= split.m_abscissa)
                {
                    right_leaf.insert(triangle_index);
                }
                else
                {
                    // Fetch triangle vertices.
                    const TriangleInfo& triangle_info = m_triangle_infos[triangle_index];
                    const GVector3& v0 = triangle_info.get_vertex(0);
                    const GVector3& v1 = triangle_info.get_vertex(1);
                    const GVector3& v2 = triangle_info.get_vertex(2);

                    // Determine to which leaves the triangle belongs.
                    const bool in_left = intersect(left_bbox, v0, v1, v2);
                    const bool in_right = !in_left || intersect(right_bbox, v0, v1, v2);
                    assert(in_left || in_right);

                    // Insert the triangle into the appropriate leaves.
                    if (in_left) left_leaf.insert(triangle_index);
                    if (in_right) right_leaf.insert(triangle_index);
                }
            }

            if (m_tracer.get())
            {
                m_tracer->conclude_sort(
                    leaf,
                    leaf_info,
                    left_leaf,
                    left_leaf_info,
                    right_leaf,
                    right_leaf_info);
            }
        }

      private:
        class Tracer
        {
          public:
            Tracer()
              : m_visitor(m_logger)
            {
                m_log_target.reset(create_file_log_target());
                m_log_target->open("split-trace.txt");
                m_log_target->set_formatting_flags(LogMessage::Info, LogMessage::DisplayMessage);
                m_log_target->set_formatting_flags(LogMessage::Debug, LogMessage::DisplayMessage);
                m_logger.add_target(m_log_target.get());
            }

            void begin_exact_sah_split(const size_t triangle_count)
            {
                LOG_INFO(
                    m_logger,
                    "\nSplitting " FMT_SIZE_T " triangles using exact SAH:",
                    triangle_count);
            }

            void begin_approximate_sah_split(const size_t triangle_count)
            {
                LOG_INFO(
                    m_logger,
                    "\nSplitting " FMT_SIZE_T " triangles using approximate SAH:",
                    triangle_count);
            }

            template <typename SAHFunc>
            void dump_sah_function(SAHFunc& func, const GAABB3& bbox, const size_t dim)
            {
                m_visitor.clear();

                func.visit(bbox, dim, m_visitor);

                LOG_INFO(
                    m_logger,
                    "    %s: extent %f, function: plot([%s]):",
                    get_axis_name(dim),
                    bbox.extent()[dim],
                    m_visitor.str().c_str());
            }

            void conclude_no_split(const GScalar leaf_cost, const GScalar min_cost)
            {
                char buf[100];

                if (min_cost == numeric_limits<GScalar>::max())
                    strcpy(buf, "n/a");
                else portable_snprintf(buf, sizeof(buf), "%f", min_cost);

                LOG_INFO(
                    m_logger,
                    "    Outcome: DON'T SPLIT (best split cost: %s > leaf cost: %f)",
                    buf,
                    leaf_cost);
            }

            void conclude_split(const GScalar leaf_cost, const GScalar min_cost, const Split<GScalar>& split)
            {
                assert(min_cost < numeric_limits<GScalar>::max());

                LOG_INFO(
                    m_logger,
                    "    Outcome: SPLIT along %s at %f (best split cost: %f < leaf cost: %f)",
                    get_axis_name(split.m_dimension),
                    split.m_abscissa,
                    min_cost,
                    leaf_cost);
            }

            void begin_sort(const size_t triangle_count)
            {
                LOG_INFO(
                    m_logger,
                    "\nSorting " FMT_SIZE_T " triangle%s:",
                    triangle_count,
                    triangle_count > 1 ? "s" : "");
            }

            void conclude_sort(
                const IntermTriangleLeaf&               parent_leaf,
                const IntermTriangleLeaf::LeafInfoType& parent_leaf_info,
                const IntermTriangleLeaf&               left_leaf,
                const IntermTriangleLeaf::LeafInfoType& left_leaf_info,
                const IntermTriangleLeaf&               right_leaf,
                const IntermTriangleLeaf::LeafInfoType& right_leaf_info)
            {
                const GAABB3 parent_bbox = parent_leaf_info.get_bbox();
                const GAABB3 left_bbox = left_leaf_info.get_bbox();
                const GAABB3 right_bbox = right_leaf_info.get_bbox();

                const GScalar left_extent = GAABB3::extent_ratio(left_bbox, parent_bbox);
                const GScalar right_extent = GAABB3::extent_ratio(right_bbox, parent_bbox);

                const GScalar parent_sa = parent_bbox.surface_area();
                const GScalar left_sa = left_bbox.surface_area();
                const GScalar right_sa = right_bbox.surface_area();

                const size_t left_count = left_leaf.get_size();
                const size_t right_count = right_leaf.get_size();

                LOG_INFO(
                    m_logger,
                    "    Left leaf: " FMT_SIZE_T " triangle%s, extent %s, surface area %s\n"
                    "    Right leaf: " FMT_SIZE_T " triangle%s, extent %s, surface area %s",
                    left_count,
                    left_count > 1 ? "s" : "",
                    pretty_percent(left_extent, GScalar(1.0)).c_str(),
                    pretty_percent(left_sa, parent_sa).c_str(),
                    right_count,
                    right_count > 1 ? "s" : "",
                    pretty_percent(right_extent, GScalar(1.0)).c_str(),
                    pretty_percent(right_sa, parent_sa).c_str());
            }

          private:
            class SAHFunctionVisitor
            {
              public:
                explicit SAHFunctionVisitor(Logger& logger)
                  : m_logger(logger)
                {
                }

                void clear()
                {
                    clear_keep_memory(m_string);
                }

                void visit(
                    const GScalar   domain_begin,
                    const GScalar   domain_end,
                    const GScalar   left_length,
                    const GScalar   right_length,
                    const size_t    left_count,
                    const size_t    right_count,
                    const GScalar   split_cost)
                {
                    if (!m_string.empty())
                        m_string.append(", ");

                    char buf[100];

                    m_string.append("[");

                    const GScalar abscissa = domain_begin + left_length;
                    portable_snprintf(buf, sizeof(buf), "%f", abscissa);
                    m_string.append(buf);

                    m_string.append(",");

                    portable_snprintf(buf, sizeof(buf), "%f", split_cost);
                    m_string.append(buf);

                    m_string.append("]");
                }

                const string& str() const
                {
                    return m_string;
                }

              private:
                Logger& m_logger;
                string  m_string;
            };

            auto_release_ptr<FileLogTarget> m_log_target;
            Logger                          m_logger;
            SAHFunctionVisitor              m_visitor;

            static const char* get_axis_name(const size_t axis)
            {
                static const char* AxisNames[3] = { "X", "Y", "Z" };
                return AxisNames[axis];
            }
        };

        typedef ApproxSAHFunction<
            GScalar,
            TriangleTreeApproxSAHBinCount
        > ApproxSAHFunc;

        const TriangleInfoVector&   m_triangle_infos;
        const GAABB3Vector&         m_triangle_bboxes;
        double                      m_rcp_exp_depth[TriangleTreeMaxDepth];
        ExactSAHFunction<GScalar>   m_exact_sah_function;
        auto_ptr<Tracer>            m_tracer;

        // Find a split using an exact SAH function.
        bool exact_sah_split(
            const IntermTriangleLeaf&               leaf,
            const IntermTriangleLeaf::LeafInfoType& leaf_info,
            IntermTriangleLeaf::SplitType&          split)
        {
            const GAABB3& leaf_bbox = leaf_info.get_bbox();
            const size_t triangle_count = leaf.get_size();

            if (m_tracer.get())
                m_tracer->begin_exact_sah_split(triangle_count);

            GScalar global_min_cost = numeric_limits<GScalar>::max();
            size_t global_min_dim = 0;
            GScalar global_min_abscissa = GScalar(0.0);

            // Optimize across all dimensions.
            for (size_t dim = 0; dim < 3; ++dim)
            {
                // Build the SAH function.
                m_exact_sah_function.reset(leaf_bbox.min[dim], leaf_bbox.max[dim]);
                for (size_t i = 0; i < triangle_count; ++i)
                {
                    const size_t triangle_index = leaf.get_triangle(i);
                    const GAABB3& triangle_bbox = m_triangle_bboxes[triangle_index];
                    m_exact_sah_function.insert(
                        triangle_bbox.min[dim],
                        triangle_bbox.max[dim]);
                }

                if (m_tracer.get())
                    m_tracer->dump_sah_function(m_exact_sah_function, leaf_bbox, dim);

                // Minimize the SAH function.
                GScalar min_cost, min_abscissa;
                m_exact_sah_function.minimize(
                    leaf_bbox,
                    dim,
                    min_cost,
                    min_abscissa);

                // Keep track of the split with the lowest cost.
                if (global_min_cost > min_cost)
                {
                    global_min_cost = min_cost;
                    global_min_dim = dim;
                    global_min_abscissa = min_abscissa;
                }
            }

            // Compute the cost of keeping the leaf unsplit.
            const GScalar leaf_cost =
                triangle_count * leaf_bbox.half_surface_area() * TriangleTreeLeafCostMultiplier;

            // Don't split the leaf if it's too costly.
            if (global_min_cost >= leaf_cost)
            {
                if (m_tracer.get())
                    m_tracer->conclude_no_split(leaf_cost, global_min_cost);

                return false;
            }

            // Found a good split, return it.
            split.m_dimension = global_min_dim;
            split.m_abscissa = global_min_abscissa;

            if (m_tracer.get())
                m_tracer->conclude_split(leaf_cost, global_min_cost, split);

            return true;
        }

        // Find a split using an approximate SAH function.
        bool approximate_sah_split(
            const IntermTriangleLeaf&               leaf,
            const IntermTriangleLeaf::LeafInfoType& leaf_info,
            IntermTriangleLeaf::SplitType&          split)
        {
            const GAABB3& leaf_bbox = leaf_info.get_bbox();
            const size_t triangle_count = leaf.get_size();

            if (m_tracer.get())
                m_tracer->begin_approximate_sah_split(triangle_count);

#ifdef RENDERER_TRIANGLE_TREE_SPLIT_LONGEST_AXIS

            const size_t dim = max_index(leaf_bbox.extent());

            // Build the SAH function.
            ApproxSAHFunc approx_sah_function(
                leaf_bbox.min[dim],
                leaf_bbox.max[dim]);
            for (size_t i = 0; i < triangle_count; ++i)
            {
                const size_t triangle_index = leaf.get_triangle(i);
                const GAABB3& triangle_bbox = m_triangle_bboxes[triangle_index];
                approx_sah_function.insert(
                    triangle_bbox.min[dim],
                    triangle_bbox.max[dim]);
            }

            // Minimize the SAH function.
            const size_t global_min_dim = dim;
            GScalar global_min_cost;
            GScalar global_min_abscissa;
            approx_sah_function.minimize(
                leaf_bbox,
                dim,
                global_min_cost,
                global_min_abscissa);

#else

            ApproxSAHFunc sah_func_x(leaf_bbox.min[0], leaf_bbox.max[0]);
            ApproxSAHFunc sah_func_y(leaf_bbox.min[1], leaf_bbox.max[1]);
            ApproxSAHFunc sah_func_z(leaf_bbox.min[2], leaf_bbox.max[2]);

            // Build the SAH functions over all three axes.
            for (size_t i = 0; i < triangle_count; ++i)
            {
                const size_t triangle_index = leaf.get_triangle(i);
                const GAABB3& triangle_bbox = m_triangle_bboxes[triangle_index];
                sah_func_x.insert(triangle_bbox.min[0], triangle_bbox.max[0]);
                sah_func_y.insert(triangle_bbox.min[1], triangle_bbox.max[1]);
                sah_func_z.insert(triangle_bbox.min[2], triangle_bbox.max[2]);
            }

            GScalar global_min_cost = numeric_limits<GScalar>::max();
            size_t global_min_dim = 0;
            GScalar global_min_abscissa = GScalar(0.0);

            // Minimize the SAH function along the X axis.
            {
                sah_func_x.minimize(
                    leaf_bbox,
                    0,
                    global_min_cost,
                    global_min_abscissa);
            }

            // Minimize the SAH function along the Y axis.
            {
                GScalar min_cost, min_abscissa;
                sah_func_y.minimize(
                    leaf_bbox,
                    1,
                    min_cost,
                    min_abscissa);

                if (global_min_cost > min_cost)
                {
                    global_min_cost = min_cost;
                    global_min_dim = 1;
                    global_min_abscissa = min_abscissa;
                }
            }

            // Minimize the SAH function along the Z axis.
            {
                GScalar min_cost, min_abscissa;
                sah_func_z.minimize(
                    leaf_bbox,
                    2,
                    min_cost,
                    min_abscissa);

                if (global_min_cost > min_cost)
                {
                    global_min_cost = min_cost;
                    global_min_dim = 2;
                    global_min_abscissa = min_abscissa;
                }
            }

#endif

            if (m_tracer.get())
            {
                m_tracer->dump_sah_function(sah_func_x, leaf_bbox, 0);
                m_tracer->dump_sah_function(sah_func_y, leaf_bbox, 1);
                m_tracer->dump_sah_function(sah_func_z, leaf_bbox, 2);
            }

            // Compute the cost of keeping the leaf unsplit.
            const GScalar leaf_cost =
                triangle_count * leaf_bbox.half_surface_area() * TriangleTreeLeafCostMultiplier;

            // Don't split the leaf if it's too costly.
            if (global_min_cost >= leaf_cost)
            {
                if (m_tracer.get())
                    m_tracer->conclude_no_split(leaf_cost, global_min_cost);

                return false;
            }

            // Found a good split, return it.
            split.m_dimension = global_min_dim;
            split.m_abscissa = global_min_abscissa;

            if (m_tracer.get())
                m_tracer->conclude_split(leaf_cost, global_min_cost, split);

            return true;
        }
    };


    //
    // Intermediate triangle tree builder.
    //

    class IntermTriangleTree;

    typedef bsp::Builder<
        IntermTriangleTree,
        IntermTriangleLeafFactory,
        IntermTriangleLeafSplitter
    > IntermTriangleTreeBuilder;


    //
    // Intermediate triangle tree statistics.
    //

    typedef bsp::TreeStatistics<
        IntermTriangleTree,
        IntermTriangleTreeBuilder
    > IntermTriangleTreeStatistics;


    //
    // Intermediate triangle tree.
    //

    class IntermTriangleTree
      : public bsp::Tree<GScalar, 3, IntermTriangleLeaf>
    {
      public:
        // Constructor, builds the tree for a given assembly.
        explicit IntermTriangleTree(const TriangleTree::Arguments& arguments)
        {
            // Create the leaf factory.
            IntermTriangleLeafFactory factory(m_triangle_bboxes);

            // Collect all triangles for this tree.
            const size_t region_count = arguments.m_regions.size();
            for (size_t region_index = 0; region_index < region_count; ++region_index)
            {
                // Fetch the region info.
                const RegionInfo& region_info = arguments.m_regions[region_index];

                // Retrieve the object instance and its transformation.
                const ObjectInstance* object_instance =
                    arguments.m_assembly.object_instances().get_by_index(
                        region_info.get_object_instance_index());
                assert(object_instance);
                const Transformd& transform = object_instance->get_transform();

                // Retrieve the object.
                Object& object = object_instance->get_object();

                // Retrieve the region kit of the object.
                Access<RegionKit> region_kit(&object.get_region_kit());

                // Retrieve the region.
                const IRegion* region = (*region_kit)[region_info.get_region_index()];

                // Retrieve the tessellation of the region.
                Access<StaticTriangleTess> tess(&region->get_static_triangle_tess());

                // Collect all triangles of the region that intersect the bounding box of the tree.
                const size_t triangle_count = tess->m_primitives.size();
                for (size_t triangle_index = 0; triangle_index < triangle_count; ++triangle_index)
                {
                    // Fetch the triangle.
                    const Triangle& triangle = tess->m_primitives[triangle_index];

                    // Retrieve object space vertices of the triangle.
                    const GVector3& v0_os = tess->m_vertices[triangle.m_v0];
                    const GVector3& v1_os = tess->m_vertices[triangle.m_v1];
                    const GVector3& v2_os = tess->m_vertices[triangle.m_v2];

                    // Transform triangle vertices to assembly space.
                    const GVector3 v0 = transform.transform_point_to_parent(v0_os);
                    const GVector3 v1 = transform.transform_point_to_parent(v1_os);
                    const GVector3 v2 = transform.transform_point_to_parent(v2_os);

                    // Calculate the (square of the) area of this triangle.
                    const GScalar triangle_square_area = square_area(v0, v1, v2);

                    // Ignore degenerate triangles.
                    if (triangle_square_area == GScalar(0.0))
                        continue;

                    // Insert this triangle into the root leaf if it intersects the
                    // bounding box of the tree.
                    if (intersect(arguments.m_bbox, v0, v1, v2))
                    {
                        const TriangleInfo triangle_info(
                            region_info.get_object_instance_index(),
                            region_info.get_region_index(),
                            triangle_index,
                            v0, v1, v2);
                        m_triangle_infos.push_back(triangle_info);

                        GAABB3 bbox;
                        bbox.invalidate();
                        bbox.insert(v0);
                        bbox.insert(v1);
                        bbox.insert(v2);
                        m_triangle_bboxes.push_back(bbox);
                    }
                }
            }

            const size_t triangle_count = m_triangle_infos.size();

            // Log a progress message.
            RENDERER_LOG_INFO(
                "building triangle bsp tree #" FMT_UNIQUE_ID " (%s %s)...",
                arguments.m_triangle_tree_uid,
                pretty_int(triangle_count).c_str(),
                plural(triangle_count, "triangle").c_str());

            // Create the root leaf of the triangle tree.
            auto_ptr<IntermTriangleLeaf> root_leaf(factory.create_leaf());
            for (size_t i = 0; i < triangle_count; ++i)
                root_leaf->insert(i);

            // Build the triangle tree.
            IntermTriangleLeafSplitter splitter(m_triangle_infos, m_triangle_bboxes);
            IntermTriangleTreeBuilder builder;
            builder.build(
                *this,
                root_leaf,
                factory,
                splitter,
                TriangleTreeMaxDuplication);

            // Collect and print triangle tree statistics.
            IntermTriangleTreeStatistics tree_stats(*this, builder);
            RENDERER_LOG_DEBUG(
                "triangle bsp tree #" FMT_UNIQUE_ID " statistics:",
                arguments.m_triangle_tree_uid);
            tree_stats.print(global_logger());

            // If uncommented, generate a Maple file to plot an histogram of the leaf sizes.
            // generate_leaf_size_histogram(arguments.m_triangle_tree_uid);
        }

      private:
        friend class renderer::TriangleTree;

        TriangleInfoVector  m_triangle_infos;
        GAABB3Vector        m_triangle_bboxes;

        void generate_leaf_size_histogram(const UniqueID triangle_tree_uid) const
        {
            const size_t MaxLeafSize = 1000;

            vector<size_t> histogram(MaxLeafSize + 1, 0);
            for (size_t i = 0; i < m_leaves.size(); ++i)
            {
                const size_t leaf_size = m_leaves[i]->get_size();
                ++histogram[min(leaf_size, MaxLeafSize)];
            }

            size_t max_abscissa = MaxLeafSize;
            while (max_abscissa > 0 && histogram[max_abscissa] == 0)
                --max_abscissa;

            const size_t Margin = 2;
            max_abscissa = min(max_abscissa + Margin, MaxLeafSize);

            vector<size_t> abscissa(max_abscissa + 1);
            for (size_t i = 0; i < abscissa.size(); ++i)
                abscissa[i] = i;

            const string tree_uid_str = to_string(triangle_tree_uid);

            MapleFile maple_file("leaf_size_histogram_" + tree_uid_str + ".mpl");
            maple_file.restart();
            maple_file.define("histogram", abscissa, histogram);
            maple_file.plot(
                "histogram",
                "red",
                "Triangle tree #" + tree_uid_str + ": X=number of triangles, Y=number of leaves");
        }
    };


    //
    // TriangleLeaf packer.
    //

    typedef vector<IntermTriangleLeaf*> IntermTriangleLeafVector;

    struct TriangleLeafPacker
    {
        // Compute the size, in 4-byte words, of one packed triangle leaf.
        static size_t compute_packed_size(
            const IntermTriangleLeaf*       interm_leaf)
        {
            assert(interm_leaf);

            // One word to store the number of triangles in the leaf.
            size_t packed_size = 1;

            // todo: change to compile-time assertion.
            assert(sizeof(GTriangleType) % sizeof(uint32) == 0);

            // Compute the size of one packed triangle.
            const size_t triangle_size =
                  1                                         // 1 word for the object instance index
                + 1                                         // 1 word for the region index
                + 1                                         // 1 word for the triangle index
                + sizeof(GTriangleType) / sizeof(uint32);   // triangle geometry

            // Compute the size of N packed triangles.
            packed_size += triangle_size * interm_leaf->get_size();

            return packed_size;
        }

        // Compute the total size, in 4-byte words, of a vector of packed triangle leaves.
        static size_t compute_total_packed_size(
            const IntermTriangleLeafVector& interm_leaves)
        {
            size_t total_packed_size = 0;

            const size_t leaf_count = interm_leaves.size();
            for (size_t i = 0; i < leaf_count; ++i)
                total_packed_size += compute_packed_size(interm_leaves[i]);

            return total_packed_size;
        }

        // Pack one leaf.
        static void pack(
            const TriangleInfoVector&       triangle_infos,
            const IntermTriangleLeaf*       interm_leaf,
            uint32*                         final_leaf,
            const size_t                    packed_size)
        {
            assert(interm_leaf);
            assert(final_leaf);
            assert(packed_size >= 1);

            const size_t triangle_count = interm_leaf->get_size();
            uint32* ptr = final_leaf;

            // Store the number of triangles in the leaf.
            *ptr++ = static_cast<uint32>(triangle_count);

            // Store hot section.
            for (size_t i = 0; i < triangle_count; ++i)
            {
                // Fetch the triangle info.
                const size_t triangle_index = interm_leaf->get_triangle(i);
                const TriangleInfo& triangle_info = triangle_infos[triangle_index];

                // Store the object instance index.
                *ptr++ = static_cast<uint32>(triangle_info.get_object_instance_index());

                // Construct the final form of the triangle geometry.
                const GTriangleType triangle_geometry(
                    triangle_info.get_vertex(0),
                    triangle_info.get_vertex(1),
                    triangle_info.get_vertex(2));

                // Store the triangle geometry.
                memcpy(ptr, &triangle_geometry, sizeof(GTriangleType));
                ptr += sizeof(GTriangleType) / sizeof(uint32);
            }

            // Store cold section.
            for (size_t i = 0; i < triangle_count; ++i)
            {
                // Fetch the triangle info.
                const size_t triangle_index = interm_leaf->get_triangle(i);
                const TriangleInfo& triangle_info = triangle_infos[triangle_index];

                // Store the region index.
                *ptr++ = static_cast<uint32>(triangle_info.get_region_index());

                // Store the triangle index.
                *ptr++ = static_cast<uint32>(triangle_info.get_triangle_index());
            }

            assert(static_cast<size_t>(ptr - final_leaf) == packed_size);
        }
    };

    size_t IntermTriangleLeaf::get_memory_size() const
    {
        return
              sizeof(IntermTriangleLeaf*)
            + TriangleLeafPacker::compute_packed_size(this) * 4;
    }


    //
    // Rearrange an array of nodes to follow the van Emde Boas layout.
    //

    template <typename Node>
    void optimize_subtree(
        const size_t            node_index,
        const size_t            subtree_depth,
        const vector<Node>&     input_nodes,
        vector<Node>&           output_nodes,
        stack<size_t>&          roots)
    {
        Node& node = output_nodes[node_index];

        if (node.get_type() == Node::Leaf)
            return;

        const size_t old_child_index = node.get_child_node_index();
        const size_t new_child_index = output_nodes.size();
        node.set_child_node_index(new_child_index);

        output_nodes.push_back(input_nodes[old_child_index]);
        output_nodes.push_back(input_nodes[old_child_index + 1]);

        if (subtree_depth > 0)
        {
            optimize_subtree(
                new_child_index,
                subtree_depth - 1,
                input_nodes,
                output_nodes,
                roots);
            optimize_subtree(
                new_child_index + 1,
                subtree_depth - 1,
                input_nodes,
                output_nodes,
                roots);
        }
        else
        {
            roots.push(new_child_index);
            roots.push(new_child_index + 1);
        }
    }

    template <typename Node>
    void optimize_node_layout(
        const size_t            subtree_depth,
        const vector<Node>&     input_nodes,
        vector<Node>&           output_nodes)
    {
        stack<size_t> roots;
        roots.push(0);
        output_nodes.push_back(input_nodes[0]);

        while (!roots.empty())
        {
            const size_t root_index = roots.top();
            roots.pop();
            optimize_subtree(
                root_index,
                subtree_depth,
                input_nodes,
                output_nodes,
                roots);
        }

        assert(output_nodes.size() == input_nodes.size());
    }
}


//
// TriangleTree class implementation.
//

TriangleTree::Arguments::Arguments(
    const UniqueID          triangle_tree_uid,
    const GAABB3&           bbox,
    const Assembly&         assembly,
    const RegionInfoVector& regions)
  : m_triangle_tree_uid(triangle_tree_uid)
  , m_bbox(bbox)
  , m_assembly(assembly)
  , m_regions(regions)
{
}

TriangleTree::TriangleTree(const Arguments& arguments)
  : m_triangle_tree_uid(arguments.m_triangle_tree_uid)
{
    // Build the intermediate representation of the tree.
    IntermTriangleTree interm_tree(arguments);

    // Copy tree bounding box.
    m_bbox = interm_tree.m_bbox;

    // Copy tree nodes.
    optimize_node_layout(
        TriangleTreeSubtreeDepth,
        interm_tree.m_nodes,
        m_nodes);

    // Print the alignment of the node array base address.
    assert(!m_nodes.empty());
    RENDERER_LOG_DEBUG(
        "triangle bsp tree node array is %u-byte aligned",
        alignment(&m_nodes[0]));

    // Convert the minimum page size from bytes to 4-byte words.
    const size_t MinPageSize = TriangleTreeMinLeafPageSize / 4;

    const size_t leaf_count = interm_tree.m_leaves.size();
    m_leaves.resize(leaf_count);

    for (size_t begin = 0; begin < leaf_count;)
    {
        // Gather consecutive leaves into one page.
        size_t end = begin;
        size_t page_size = 0;
        for (; end < leaf_count && page_size < MinPageSize; ++end)
        {
            // Fetch the intermediate representation of this leaf.
            const IntermTriangleLeaf* interm_leaf = interm_tree.m_leaves[end];

            // Compute the size of the leaf once packed.
            const size_t leaf_size =
                TriangleLeafPacker::compute_packed_size(interm_leaf);

            // Compute the accumulated size of the leaves.
            page_size += leaf_size;
        }

        // Allocate a new page.
        uint32* page = new uint32[page_size];
        size_t page_index = 0;

        // Pack leaves into the page.
        for (size_t i = begin; i < end; ++i)
        {
            // Fetch the intermediate representation of this leaf.
            const IntermTriangleLeaf* interm_leaf = interm_tree.m_leaves[i];

            // Compute the location of the final representation of this leaf.
            m_leaves[i] = &page[page_index];

            // Pack this leaf.
            const size_t leaf_size =
                TriangleLeafPacker::compute_packed_size(interm_leaf);
            TriangleLeafPacker::pack(
                interm_tree.m_triangle_infos,
                interm_leaf,
                m_leaves[i],
                leaf_size);

            // Advance into the page.
            page_index += leaf_size;
        }

        // Store the page into the page array.
        m_leaf_page_array.push_back(page);

        begin = end;
    }
}

TriangleTree::~TriangleTree()
{
    // Log a progress message.
    RENDERER_LOG_INFO(
        "deleting triangle bsp tree #" FMT_UNIQUE_ID "...",
        m_triangle_tree_uid);

    // Make sure we delete the leaves ourselves before the destructor of
    // the parent class (foundation::bsp::Tree) gets called, since it tries
    // to delete each leaf individually, which is incorrect in this case
    // because leaves are really just pointers into pages of memory, and
    // thus cannot be deleted individually.
    m_leaves.clear();

    // Delete the pages.
    for (size_t i = 0; i < m_leaf_page_array.size(); ++i)
        delete [] m_leaf_page_array[i];
}


//
// TriangleTreeFactory class implementation.
//

TriangleTreeFactory::TriangleTreeFactory(
    const TriangleTree::Arguments& arguments)
  : m_arguments(arguments)
{
}

auto_ptr<TriangleTree> TriangleTreeFactory::create()
{
    return auto_ptr<TriangleTree>(new TriangleTree(m_arguments));
}


//
// TriangleLeafVisitor class implementation.
//

double TriangleLeafVisitor::visit(
    const TriangleLeaf*             leaf,
    const ShadingRay::RayType&      /*ray*/,
    const ShadingRay::RayInfoType&  /*ray_info*/)
{
    assert(leaf);

    // Size, in 4-byte words, of one triangle.
    const size_t TriangleSize = sizeof(GTriangleType) / sizeof(uint32);

    // Start reading at the beginning of the hot section of the leaf.
    const uint32* ptr = leaf;

    // Read the number of triangles in the leaf.
    const uint32 triangle_count = *ptr++;
    assert(triangle_count > 0);

    // Sequentially intersect all triangles of this leaf.
    size_t i = triangle_count;
    do
    {
        // Read the object instance index.
        const uint32 object_instance_index = *ptr++;

        // todo: check object instance flags.

        // Read the triangle geometry.
        const TriangleGeometryReader reader(ptr);

        // Intersect the triangle.
        double t, u, v;
        if (reader.m_triangle.intersect(m_shading_point.m_ray, t, u, v))
        {
            m_triangle_ptr = ptr;
            m_cold_data_index = 2 * triangle_count + i * (TriangleSize - 1) - 1;
            m_shading_point.m_ray.m_tmax = t;
            m_shading_point.m_hit = true;
            m_shading_point.m_bary[0] = u;
            m_shading_point.m_bary[1] = v;
            m_shading_point.m_object_instance_index = static_cast<size_t>(object_instance_index);
        }

        // Next triangle.
        ptr += TriangleSize;
    }
    while (--i);

    // Return the distance to the closest intersection so far.
    return m_shading_point.m_ray.m_tmax;
}

void TriangleLeafVisitor::read_hit_triangle_data() const
{
    if (m_triangle_ptr)
    {
        // Compute and store the support plane of the hit triangle.
        const TriangleGeometryReader reader(m_triangle_ptr);
        m_shading_point.m_triangle_support_plane.initialize(reader.m_triangle);

        // Read region and triangle indices.
        const uint32* cold_data_ptr = m_triangle_ptr + m_cold_data_index;
        m_shading_point.m_region_index = static_cast<size_t>(cold_data_ptr[0]);
        m_shading_point.m_triangle_index = static_cast<size_t>(cold_data_ptr[1]);
    }
}


//
// TriangleLeafProbeVisitor class implementation.
//

double TriangleLeafProbeVisitor::visit(
    const TriangleLeaf*             leaf,
    const ShadingRay::RayType&      ray,
    const ShadingRay::RayInfoType&  /*ray_info*/)
{
    assert(leaf);

    // Size, in 4-byte words, of one triangle.
    const size_t TriangleSize = sizeof(GTriangleType) / sizeof(uint32);

    // Start reading at the beginning of the hot section of the leaf.
    const uint32* ptr = leaf;

    // Read the number of triangles in the leaf.
    uint32 triangle_count = *ptr++;
    assert(triangle_count > 0);

    // Sequentially intersect all triangles of this leaf.
    do
    {
        // Read the object instance index.
        const uint32 object_instance_index = *ptr++;

        // todo: check object instance flags.

        // Read the triangle geometry.
        const TriangleGeometryReader reader(ptr);

        // Intersect triangle.
        if (reader.m_triangle.intersect(ray))
        {
            // Terminate traversal.
            m_hit = true;
            return ray.m_tmin;
        }

        // Next triangle.
        ptr += TriangleSize;
    }
    while (--triangle_count);

    // Continue traversal.
    return ray.m_tmax;
}

}   // namespace renderer
