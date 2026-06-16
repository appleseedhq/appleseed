
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Stephen Agyemang, The appleseedhq Organization
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

#pragma once

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/lighting/gpt/gptparameters.h"
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/math/aabb.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <array>
#include <atomic>
#include <fstream>
#include <list>
#include <memory>
#include <utility>
#include <vector>

//
// SD-Tree implementation for "Practical Path Guiding for Efficient Light-Transport Simulation" [Müller et al. 2017].
//

namespace renderer {

// Forward declarations.
struct DTreeRecord;
struct DTreeStatistics;
struct VisualizerNode;

struct DTreeSample
{
    foundation::Vector3f                direction;
    float                               pdf;
    ScatteringMode::Mode                scattering_mode;
};

// The node type for the D-Tree.

class QuadTreeNode
{
  public:
    QuadTreeNode(
        const bool                          create_children,
        const float                         radiance_sum = 0.0f);

    QuadTreeNode(
        const QuadTreeNode&                 other);

    QuadTreeNode& operator=(const QuadTreeNode& other) = delete;

    // Recursively add radiance unfiltered.
    void add_radiance(
        foundation::Vector2f&               direction,
        const float                         radiance);

    // Recursively add radiance filtered.
    void add_radiance(
        const foundation::AABB2f&           splat_aabb,
        const foundation::AABB2f&           node_aabb,
        const float                         radiance);

    size_t max_depth() const;
    size_t node_count() const;
    float radiance_sum() const;

    // Recursively sum and store each node's children's radiance.
    float build_radiance_sums();

    // Recursively restructure the D-tree based on the directional radiance distribution.
    void restructure(
        const float                         total_radiance_sum,
        const float                         subdiv_threshold,
        std::vector<
          std::pair<float, float>>*         sorted_energy_ratios,
        const size_t                        depth = 1);

    // Reset to state of an initial root node.
    void reset();

    // Sample a direction in cylindrical coordinates based on the directional radiance distribution.
    const foundation::Vector2f sample(
        foundation::Vector2f&               sample,
        float&                              pdf) const;

    float pdf(
        foundation::Vector2f&               direction) const;

    size_t depth(
        foundation::Vector2f&               direction) const;

    // Flatten the node based D-tree representation to a list format compatible with the visualizer tool.
    void flatten(
        std::list<VisualizerNode>&          nodes) const;

  private:
      // Recursively sample a direction based on the directional radiance distribution.
    const foundation::Vector2f sample_recursive(
        foundation::Vector2f&               sample,
        float&                              pdf) const;

    QuadTreeNode* choose_node(
        foundation::Vector2f&               direction) const;

    std::unique_ptr<QuadTreeNode>       m_upper_left_node;
    std::unique_ptr<QuadTreeNode>       m_upper_right_node;
    std::unique_ptr<QuadTreeNode>       m_lower_right_node;
    std::unique_ptr<QuadTreeNode>       m_lower_left_node;

    // The active radiance sum for recording incoming light.
    std::atomic<float>                  m_current_iter_radiance_sum;

    // The last completed iteration's radiance sum to guide the direction sampling.
    float                               m_previous_iter_radiance_sum;
    
    bool                                m_is_leaf;
};

// The D-tree interface.

class DTree
{
  public:
    DTree(
        const GPTParameters&                parameters);

    DTree(
        const DTree&                        other);

    // Record radiance to the D-tree.
    void record(
        const DTreeRecord&                  d_tree_record);

    // Sample a direction based on the directional radiance distribution.
    void sample(
        SamplingContext&                    sampling_context,
        DTreeSample&                        d_tree_sample,
        const int                           modes) const;

    float pdf(
        const foundation::Vector3f&         direction,
        const int                           modes) const;
    
    // Divide the tree's sample weight by two.
    void halve_sample_weight();
    size_t node_count() const;
    size_t max_depth() const;
    size_t depth(
        const foundation::Vector2f&         direction) const;

    // Recursively sum the directional radiance contributions from leaf to root.
    void build();

    // Recursively restructure the D-tree based on the directional radiance distribution.
    void restructure(
        const float                         subdiv_threshold);

    float sample_weight() const;
    float mean() const;
    float bsdf_sampling_fraction() const;
    ScatteringMode::Mode get_scattering_mode() const;

    void write_to_disk(
        std::ofstream&                      os) const;

  private:
    void acquire_optimization_spin_lock();
    void release_optimization_spin_lock();

    // Perform an bsdf sampling fraction optimization step.
    void optimization_step(
        const DTreeRecord&                  d_tree_record);

    void adam_step(
        const float                         gradient);

    QuadTreeNode                        m_root_node;
    std::atomic<float>                  m_current_iter_sample_weight;
    float                               m_previous_iter_sample_weight;
    bool                                m_is_built;
    ScatteringMode::Mode                m_scattering_mode;

    // BSDF sampling fraction optimization variables.
    std::atomic_flag                    m_atomic_flag;
    size_t                              m_optimization_step_count;
    float                               m_first_moment;
    float                               m_second_moment;
    float                               m_theta;

    const GPTParameters&                m_parameters;
};

// The S-tree node class.

class STreeNode
{
  public:
    STreeNode(
        const GPTParameters&                parameters);

    STreeNode(
        const unsigned int                  parent_axis,
        const DTree*                        parent_d_tree);

    // Get the D-tree at a scene position.
    DTree* get_d_tree(
        foundation::Vector3f&               point,
        foundation::Vector3f&               size);

    // Recursively refine the spatial resolution.
    void subdivide(
        const size_t                        required_samples);

    // Record radiance box filtered.
    void record(
        const foundation::AABB3f&           splat_aabb,
        const foundation::AABB3f&           node_aabb,
        const DTreeRecord&                  d_tree_record);

    // Refine the D-tree at spatial leaf nodes.
    void restructure(
        const float                         subdiv_threshold);

    // Build the D-tree at spatial leaf nodes.
    void build();

    void gather_statistics(
        DTreeStatistics&                    statistics,
        const size_t                        depth = 1) const;

    void write_to_disk(
        std::ofstream&                      os,
        const foundation::AABB3f&           aabb) const;

  private:
    STreeNode* choose_node(
        foundation::Vector3f&               point) const;

    void subdivide();
    bool is_leaf() const;

    std::unique_ptr<STreeNode>          m_first_node;
    std::unique_ptr<STreeNode>          m_second_node;

    // This member is only set if the node is a leaf node and nullptr otherwise.
    std::unique_ptr<DTree>              m_d_tree;

    // This node's split axis.
    unsigned int                        m_axis;
};

// The root class of the SD-tree.

class STree {
  public:
    STree(
        const renderer::Scene&              scene,
        const GPTParameters&                parameters);

    // Get the D-tree and its size at a scene position.
    DTree* get_d_tree(
        const foundation::Vector3f&         point,
        foundation::Vector3f&               d_tree_voxel_size);

    // Get the D-tree at a scene position.
    DTree* get_d_tree(
        const foundation::Vector3f&         point);

    // Record radiance.
    void record(
        DTree*                              d_tree,
        const foundation::Vector3f&         point,
        const foundation::Vector3f&         d_tree_node_size,
        DTreeRecord&                        d_tree_record,
        SamplingContext&                    sampling_context);

    // Refine the SD-tree's radiance distribution after an iteration has completed.
    void build(
        const size_t                        iteration);

    bool is_built() const;

    void start_final_iteration();

    bool is_final_iteration() const;

    void write_to_disk(
        const size_t                        iteration,
        const bool                          append_iteration) const;

  private:
    void box_filter_splat(
        const foundation::Vector3f&         point,
        const foundation::Vector3f&         d_tree_node_size,
        DTreeRecord&                        d_tree_record);

    /// Clip a point to lie within the scene bounding box.
    foundation::Vector3f clip_vector_to_aabb(
        const foundation::Vector3f&         point);

    const GPTParameters                 m_parameters;
    const renderer::Scene&              m_scene;
    foundation::AABB3f                  m_scene_aabb;
    std::unique_ptr<STreeNode>          m_root_node;
    bool                                m_is_built;
    bool                                m_is_final_iteration;
};

// Guided path tracing vertex used for keeping track of information at scattering events along a path.

class GPTVertex
{
  public:

    // Add radiance conbributions lying further beyond this vertex' scattering event.
    void add_radiance(
        const renderer::Spectrum&           radiance);
    
    // Record the accumulated radiance to the sd_tree on path completion.
    void record_to_tree(
        STree&                              sd_tree,
        SamplingContext&                    sampling_context);

    DTree*                              m_d_tree;
    foundation::Vector3f                m_d_tree_node_size;
    foundation::Vector3f                m_point;
    foundation::Vector3f                m_direction;
    renderer::Spectrum                  m_throughput;
    renderer::Spectrum                  m_bsdf_value;
    renderer::Spectrum                  m_radiance;
    float                               m_wi_pdf;
    float                               m_bsdf_pdf;
    float                               m_d_tree_pdf;
    bool                                m_is_delta;
};

// A trail of guided path tracing vertices.

class GPTVertexPath
{
  public:
    GPTVertexPath();

    // Add a vertex to the path.
    void add_vertex(
        const GPTVertex&                    vertex);

    // Add radiance to all vertices in the path.
    void add_radiance(
        const renderer::Spectrum&           r);

    // Add radiance to all but the last vertex in the path.
    void add_indirect_radiance(
        const renderer::Spectrum&           r);

    // Record the accumulated radiance of all vertices in the path to the sd_tree on path completion.
    void record_to_tree(
        STree&                              sd_tree,
        SamplingContext&                    sampling_context);

    // Test if this path has capacity for another vertex.
    bool is_full() const;

  private:
    std::array<GPTVertex, 32>           m_path; // a path can hold 32 vertices
    int                                 m_path_index; // index into the next free array position
};

}   // namespace renderer