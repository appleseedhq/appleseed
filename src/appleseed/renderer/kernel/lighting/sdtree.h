
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

// appleseed.foundation headers.
#include "foundation/math/aabb.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <array>
#include <atomic>
#include <memory>

//
// SD-Tree implementation for "Practical Path Guiding for Efficient Light-Transport Simulation" [Müller et al. 2017].
//

namespace renderer {

// Forward declarations.
struct DTreeRecord;
struct DTreeStatistics;

struct DTreeSample
{
    foundation::Vector3f                direction;
    float                               pdf;
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

    QuadTreeNode& operator=(const QuadTreeNode& other) = delete; // TODO: is this necessary

    void add_radiance(
        foundation::Vector2f&               direction,
        const float                         radiance);

    void add_radiance(
        const foundation::AABB2f&           splat_aabb,
        const foundation::AABB2f&           node_aabb,
        const float                         radiance);

    size_t max_depth() const;

    size_t node_count() const;

    float radiance_sum() const;

    float build_radiance_sums();

    void restructure(
        const float                         total_radiance_sum,
        const float                         subdiv_threshold,
        const size_t                        depth = 1);

    const foundation::Vector2f sample(
        foundation::Vector2f&               sample,
        float&                              pdf) const;

    float pdf(
        foundation::Vector2f&               direction) const;

    size_t depth(
        foundation::Vector2f&               direction) const;

  private:
    QuadTreeNode* choose_node(
        foundation::Vector2f&               direction) const;


    std::unique_ptr<QuadTreeNode>       m_upper_left_node;
    std::unique_ptr<QuadTreeNode>       m_upper_right_node;
    std::unique_ptr<QuadTreeNode>       m_lower_right_node;
    std::unique_ptr<QuadTreeNode>       m_lower_left_node;

    std::atomic<float>                  m_current_iter_radiance_sum;
    float                               m_previous_iter_radiance_sum;
    
    bool                                m_is_leaf;
};

class DTree
{
  public:
    DTree(
        const GPTParameters&                parameters);

    DTree(
        const DTree&                        other);

    void record(
        const DTreeRecord&                  d_tree_record);

    void sample(
        SamplingContext&                    sampling_context,
        DTreeSample&                        d_tree_sample) const;

    float pdf(
        const foundation::Vector3f&         direction) const;
    
    void halve_sample_weight();

    size_t node_count() const;

    size_t max_depth() const;

    size_t depth(
        const foundation::Vector2f&         direction) const;

    void build();

    void restructure(
        const float                         subdiv_threshold);

    float sample_weight() const;

    float mean() const;

    float bsdf_sampling_fraction() const;

  private:
    void acquire_optimization_spin_lock();

    void release_optimization_spin_lock();

    // BSDF sampling fraction optimization procedure.
    // Implementation of Algorithm 3 in chapter "Practical Path Guiding in Production" [Müller 2019]
    // released in "Path Guiding in Production" Siggraph Course 2019, [Vorba et. al. 2019]

    void adam_step(
        const float                         gradient);

    void optimization_step(
        const DTreeRecord&                  d_tree_record);


    QuadTreeNode                        m_root_node;
    std::atomic<float>                  m_current_iter_sample_weight;
    float                               m_previous_iter_sample_weight;
    bool                                m_is_built;

    std::atomic_flag                    m_atomic_flag;
    size_t                              m_optimization_step_count;
    float                               m_first_moment;
    float                               m_second_moment;
    float                               m_theta;

    const GPTParameters&                m_parameters;
};

class STreeNode
{
  public:
    STreeNode(
        const GPTParameters&                parameters);

    STreeNode(
        const unsigned int                  parent_axis,
        const DTree*                        parent_d_tree);

    DTree* get_d_tree(
        foundation::Vector3f&               point,
        foundation::Vector3f&               size);

    void subdivide(
        const size_t                        required_samples);

    void record(
        const foundation::AABB3f&           splat_aabb,
        const foundation::AABB3f&           node_aabb,
        const DTreeRecord&                  d_tree_record);

    void restructure(
        const float                         subdiv_threshold);

    void build();

    void gather_statistics(
        DTreeStatistics&                    statistics,
        const size_t                        depth = 1) const;

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

class STree {
  public:
    STree(
        const foundation::AABB3f&           scene_aabb,
        const GPTParameters&                parameters);

    DTree* get_d_tree(
        const foundation::Vector3f&         point,
        foundation::Vector3f&               d_tree_voxel_size);

    DTree* get_d_tree(
        const foundation::Vector3f&         point);

    void record(
        DTree*                              d_tree,
        const foundation::Vector3f&         point,
        const foundation::Vector3f&         d_tree_node_size,
        DTreeRecord                         d_tree_record,
        SamplingContext&                    sampling_context);

    const foundation::AABB3f& aabb() const;

    void build(
        const size_t                        iteration);

    bool is_built() const;

    void start_final_iteration();

    bool is_final_iteration() const;

  private:
    void box_filter_splat(
        const foundation::Vector3f&         point,
        const foundation::Vector3f&         d_tree_node_size,
        DTreeRecord&                        d_tree_record);

    /// Clip a point to lie within bounding box.
    foundation::Vector3f clip_vector_to_aabb(
        const foundation::Vector3f&         point);


    const GPTParameters                 m_parameters;
    std::unique_ptr<STreeNode>          m_root_node;
    foundation::AABB3f                  m_scene_aabb;
    bool                                m_is_built;
    bool                                m_is_final_iteration;
};

class GPTVertex
{
  public:
    void add_radiance(
        const renderer::Spectrum&           radiance);
    
    void record_to_tree(
        STree&                              sd_tree,
        float                               statistical_weight,
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

class GPTVertexPath
{
  public:
    GPTVertexPath();
    void add_vertex(
        const GPTVertex&                    vertex);

    void add_radiance(
        const renderer::Spectrum&           r);

    void record_to_tree(
        STree&                              sd_tree,
        float                               statistical_weight,
        SamplingContext&                    sampling_context);

    bool is_full() const;

    void set_sampling_fraction(
        const float                         sampling_fraction);

    float get_sampling_fraction() const;


  private:
    std::array<GPTVertex, 32>           m_path;
    int                                 m_path_index;
    float                               m_sampling_fraction;
};

}   // namespace renderer