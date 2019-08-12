//
// Code in this file is from Müller et al's implementation of practical path guiding (https://github.com/Tom94/practical-path-guiding)
// which was released under the GNU GENERAL PUBLIC LICENSE

#pragma once

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/lighting/gpt/gptparameters.h"

// appleseed.foundation headers.
#include "foundation/math/aabb.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <array>
#include <atomic>
#include <cmath>
#include <functional>
#include <memory>
#include <mutex>
#include <stack>
#include <vector>

//
// SD-Tree mplementation for "Practical Path Guiding for Efficient Light-Transport Simulation" [Müller et al. 2017].
//


namespace renderer {

const float SDTreeEpsilon = 1e-4f;
const size_t SpatialSubdivisionThreshold = 4000;
const float DTreeThreshold = 0.01;
const size_t DTreeMaxDepth = 20;

// Sampling fraction optimization constants

const float Beta1 = 0.9f;
const float Beta2 = 0.99f;
const float OptimizationEpsilon = 10e-8f;
const float Regularization = 0.01f;

static void atomic_add(std::atomic<float>& atomic, const float value)
{
    float current = atomic.load(std::memory_order_relaxed);
    while (!atomic.compare_exchange_weak(current, current + value))
        ;
}

inline float logistic(float x)
{
    return 1.0f / (1.0f + std::exp(-x));
}

foundation::Vector3f cylindrical_to_cartesian(const foundation::Vector2f &cylindrical_direction);

foundation::Vector2f cartesian_to_cylindrical(const foundation::Vector3f &d);

class QuadTreeNode
{
  public:
    QuadTreeNode(const bool create_children, const float radiance_sum = 0.0)
      : m_is_leaf(!create_children)
      , m_current_iter_radiance_sum(radiance_sum)
      , m_previous_iter_radiance_sum(radiance_sum)
    {   
        if(create_children)
        {
            m_upper_left_node.reset(new QuadTreeNode(false));
            m_upper_right_node.reset(new QuadTreeNode(false));
            m_lower_right_node.reset(new QuadTreeNode(false));
            m_lower_left_node.reset(new QuadTreeNode(false));
        }
    }

    QuadTreeNode(const QuadTreeNode& other)
      : m_current_iter_radiance_sum(other.m_current_iter_radiance_sum.load(std::memory_order_relaxed))
      , m_previous_iter_radiance_sum(other.m_previous_iter_radiance_sum)
      , m_is_leaf(other.m_is_leaf)
    {   
        if(!other.m_is_leaf)
        {
            m_upper_left_node.reset(new QuadTreeNode(*other.m_upper_left_node));
            m_upper_right_node.reset(new QuadTreeNode(*other.m_upper_right_node));
            m_lower_right_node.reset(new QuadTreeNode(*other.m_lower_right_node));
            m_lower_left_node.reset(new QuadTreeNode(*other.m_lower_left_node));
        }
    }

    QuadTreeNode& operator=(const QuadTreeNode& other) = delete;

    void add_radiance(foundation::Vector2f& direction, const float radiance)
    {
        if(m_is_leaf)
            atomic_add(m_current_iter_radiance_sum, radiance);
        else
            choose_node(direction)->add_radiance(direction, radiance);
    }

    void add_radiance(const foundation::AABB2f& splat_aabb, const foundation::AABB2f& node_aabb, const float radiance)
    {
        const foundation::AABB2f intersection_aabb(foundation::AABB2f::intersect(splat_aabb, node_aabb));

        if(!intersection_aabb.is_valid())
            return;

        const float intersection_volume = intersection_aabb.volume();

        if(intersection_volume <= 0.0f)
            return;

        if(m_is_leaf)
        {
            atomic_add(m_current_iter_radiance_sum, radiance * intersection_volume);
        }
        else
        {
            const foundation::Vector2f node_size = node_aabb.extent();
            foundation::AABB2f child_aabb(node_aabb.min, node_aabb.min + 0.5f * node_size);
            m_upper_left_node->add_radiance(splat_aabb, child_aabb, radiance);
            
            child_aabb.translate(foundation::Vector2f(0.5f * node_size.x, 0.0f));
            m_upper_right_node->add_radiance(splat_aabb, child_aabb, radiance);

            child_aabb.translate(foundation::Vector2f(0.0f, 0.5f * node_size.x));
            m_lower_right_node->add_radiance(splat_aabb, child_aabb, radiance);

            child_aabb.translate(foundation::Vector2f(-0.5f * node_size.x, 0.0f));
            m_lower_left_node->add_radiance(splat_aabb, child_aabb, radiance);
        }
    }

    size_t max_depth() const
    {
        if(m_is_leaf)
            return 1;
        
        size_t max_child_depth = m_upper_left_node->max_depth();
        max_child_depth = std::max(m_upper_right_node->max_depth(), max_child_depth);
        max_child_depth = std::max(m_lower_right_node->max_depth(), max_child_depth);
        max_child_depth = std::max(m_lower_left_node->max_depth(), max_child_depth);
        return 1 + max_child_depth;
    }

    size_t node_count() const
    {
        if(m_is_leaf)
            return 1;
        
        return 1
            + m_upper_left_node->node_count()
            + m_upper_right_node->node_count()
            + m_lower_right_node->node_count()
            + m_lower_left_node->node_count();
    }

    float radiance_sum() const
    {
        return m_previous_iter_radiance_sum;
    }

    float build_radiance_sums()
    {
        if(m_is_leaf)
        {
            m_previous_iter_radiance_sum = m_current_iter_radiance_sum.load(std::memory_order_relaxed);
            return m_previous_iter_radiance_sum;
        }

        m_previous_iter_radiance_sum = 0.0f;
        m_previous_iter_radiance_sum += m_upper_left_node->build_radiance_sums();
        m_previous_iter_radiance_sum += m_upper_right_node->build_radiance_sums();
        m_previous_iter_radiance_sum += m_lower_right_node->build_radiance_sums();
        m_previous_iter_radiance_sum += m_lower_left_node->build_radiance_sums();
        return m_previous_iter_radiance_sum;
    }

    void restructure(const float total_radiance_sum, const float subdiv_threshold, const size_t depth = 1)
    {   
        if(total_radiance_sum <= 0.0f) // Should we still grow?
            return;

        const float fraction = m_previous_iter_radiance_sum / total_radiance_sum;

        if(fraction > subdiv_threshold && depth < DTreeMaxDepth)
        {
            if(m_is_leaf)
            {
                m_is_leaf = false;
                const float quarter_sum = 0.25f * m_previous_iter_radiance_sum;
                m_upper_left_node.reset(new QuadTreeNode(false, quarter_sum));
                m_upper_right_node.reset(new QuadTreeNode(false, quarter_sum));
                m_lower_right_node.reset(new QuadTreeNode(false, quarter_sum));
                m_lower_left_node.reset(new QuadTreeNode(false, quarter_sum));
            }
            m_upper_left_node->restructure(total_radiance_sum, subdiv_threshold, depth + 1);
            m_upper_right_node->restructure(total_radiance_sum, subdiv_threshold, depth + 1);
            m_lower_right_node->restructure(total_radiance_sum, subdiv_threshold, depth + 1);
            m_lower_left_node->restructure(total_radiance_sum, subdiv_threshold, depth + 1);            
        }
        else if(!m_is_leaf)
        {
            m_is_leaf = true;
            m_upper_left_node.reset(nullptr);
            m_upper_right_node.reset(nullptr);
            m_lower_right_node.reset(nullptr);
            m_lower_left_node.reset(nullptr);
        }

        m_current_iter_radiance_sum.store(0.0f, std::memory_order_relaxed);
    }

    const foundation::Vector2f sample(foundation::Vector2f& sample, float& pdf) const
    {
        assert(sample.x >= 0.0f && sample.x < 1.0f);
        assert(sample.y >= 0.0f && sample.y < 1.0f);

        if(sample.x >= 1.0f)
            sample.x = std::nextafter(1.0f, 0.0f);

        if(sample.y >= 1.0f)
            sample.y = std::nextafter(1.0f, 0.0f);

        if(m_is_leaf)
        {
            pdf *= foundation::RcpFourPi<float>();
            return sample;
        }

        const float upper_left = m_upper_left_node->m_previous_iter_radiance_sum;
        const float upper_right = m_upper_right_node->m_previous_iter_radiance_sum;
        const float lower_right = m_lower_right_node->m_previous_iter_radiance_sum;
        const float lower_left = m_lower_left_node->m_previous_iter_radiance_sum;
        const float sum_left_half = upper_left + lower_left;
        const float sum_right_half = upper_right + lower_right;

        // TODO: Handle floating point imprecision

        float factor = sum_left_half / m_previous_iter_radiance_sum;

        if(sample.x < factor)
        {
            sample.x /= factor;
            factor = upper_left / sum_left_half;

            if(sample.y < factor)
            {
                sample.y /= factor;
                const foundation::Vector2f sampled_direction =
                    foundation::Vector2f(0.0f, 0.0f) + 0.5f * m_upper_left_node->sample(sample, pdf);

                const float probability_factor = 4.0f * upper_left / m_previous_iter_radiance_sum;
                pdf *= probability_factor;
                return sampled_direction;
            }

            sample.y = (sample.y - factor) / (1.0f - factor);
            const foundation::Vector2f sampled_direction =
                foundation::Vector2f(0.0f, 0.5f) + 0.5f * m_lower_left_node->sample(sample, pdf);

            const float probability_factor = 4.0f * lower_left / m_previous_iter_radiance_sum;
            pdf *= probability_factor;
            return sampled_direction;
        }
        else
        {
            sample.x = (sample.x - factor) / (1.0f - factor);
            factor = upper_right / sum_right_half;

            if (sample.y < factor)
            {
                sample.y /= factor;
                const foundation::Vector2f sampled_direction =
                    foundation::Vector2f(0.5f, 0.0f) + 0.5f * m_upper_right_node->sample(sample, pdf);

                const float probability_factor = 4.0f * upper_right / m_previous_iter_radiance_sum;
                pdf *= probability_factor;
                return sampled_direction;
            }

            sample.y = (sample.y - factor) / (1.0f - factor);
            const foundation::Vector2f sampled_direction =
                foundation::Vector2f(0.5f, 0.5f) + 0.5f * m_lower_right_node->sample(sample, pdf);

            const float probability_factor = 4.0f * lower_right / m_previous_iter_radiance_sum;
            pdf *= probability_factor;
            return sampled_direction;
        }
    }

    float pdf(foundation::Vector2f& direction) const
    {
        if(m_is_leaf)
            return foundation::RcpFourPi<float>();
        
        const QuadTreeNode* sub_node = choose_node(direction);
        const float factor = 4.0f * sub_node->m_previous_iter_radiance_sum / m_previous_iter_radiance_sum;
        return factor * sub_node->pdf(direction);
    }

    size_t depth(foundation::Vector2f& direction) const
    {
        if(m_is_leaf)
            return 1;
        
        return 1 + choose_node(direction)->depth(direction);
    }

  private:
    QuadTreeNode* choose_node(foundation::Vector2f& direction) const
    {
        if(direction.x < 0.5f)
        {
            direction.x *= 2.0f;
            if(direction.y < 0.5f)
            {
                direction.y *= 2.0f;
                return m_upper_left_node.get();
            }
            else
            {
                direction.y = direction.y * 2.0f - 1.0f;
                return m_lower_left_node.get();
            }
        }
        else
        {
            direction.x = direction.x * 2.0f - 1.0f;
            if(direction.y < 0.5f)
            {
                direction.y *= 2.0f;
                return m_upper_right_node.get();
            }
            else
            {
                direction.y = direction.y * 2.0f - 1.0f;
                return m_lower_right_node.get();
            }
        }
    }

    std::unique_ptr<QuadTreeNode> m_upper_left_node;
    std::unique_ptr<QuadTreeNode> m_upper_right_node;
    std::unique_ptr<QuadTreeNode> m_lower_right_node;
    std::unique_ptr<QuadTreeNode> m_lower_left_node;

    std::atomic<float> m_current_iter_radiance_sum;
    float m_previous_iter_radiance_sum;
    
    bool m_is_leaf;
};

struct DTreeRecord
{
    foundation::Vector3f direction;
    float                radiance;
    float                wo_pdf;
    float                bsdf_pdf;
    float                d_tree_pdf;
    float                sample_weight;
    float                product;
    bool                 is_delta;
};

struct DTreeSample
{
    foundation::Vector3f direction;
    float                pdf;
};

class DTree
{
  public:
    DTree(const GPTParameters& parameters)
      : m_parameters(parameters)
      , m_root_node(true)
      , m_current_iter_sample_weight(0.0f)
      , m_previous_iter_sample_weight(0.0f)
      , m_optimization_step_count(0)
      , m_first_moment(0.0f)
      , m_second_moment(0.0f)
      , m_theta(0.0f)
      , m_atomic_flag(ATOMIC_FLAG_INIT)
      , m_is_built(false)
    {}

    DTree(const DTree& other)
      : m_parameters(other.m_parameters)
      , m_current_iter_sample_weight(other.m_current_iter_sample_weight.load(std::memory_order_relaxed))
      , m_previous_iter_sample_weight(other.m_previous_iter_sample_weight)
      , m_root_node(other.m_root_node)
      , m_optimization_step_count(other.m_optimization_step_count)
      , m_first_moment(other.m_first_moment)
      , m_second_moment(other.m_second_moment)
      , m_theta(other.m_theta)
      , m_atomic_flag(ATOMIC_FLAG_INIT)
      , m_is_built(other.m_is_built)
    {}

    void record(const DTreeRecord& d_tree_record)
    {
        if(d_tree_record.is_delta || !std::isfinite(d_tree_record.sample_weight) || d_tree_record.sample_weight <= 0.0f)
            return;

        atomic_add(m_current_iter_sample_weight, d_tree_record.sample_weight);

        const float radiance = d_tree_record.radiance / d_tree_record.wo_pdf * d_tree_record.sample_weight;
        
        foundation::Vector2f direction = cartesian_to_cylindrical(d_tree_record.direction);

        switch (m_parameters.m_directional_filter)
        {
        case DirectionalFilter::Nearest:
            m_root_node.add_radiance(direction, radiance);
            break;

        case DirectionalFilter::Box:
        {
            const size_t leaf_depth = depth(direction);
            const foundation::Vector2f leaf_size(std::pow(0.5f, leaf_depth - 1));

            const foundation::AABB2f node_aabb(foundation::Vector2f(0.0f), foundation::Vector2f(1.0f));
            const foundation::AABB2f splat_aabb(direction - 0.5f * leaf_size, direction + 0.5f * leaf_size);

            if(!splat_aabb.is_valid())
                return;

            m_root_node.add_radiance(splat_aabb, node_aabb, radiance / splat_aabb.volume());
            break;
        }
        default:
            break;
        }

        if(m_parameters.m_bsdf_sampling_fraction_mode == BSDFSamplingFractionMode::Learn && m_is_built && d_tree_record.product > 0.0f)
        {
            optimization_step(d_tree_record);
        }
    }

    void sample(SamplingContext& sampling_context, DTreeSample& d_tree_sample) const
    {
        sampling_context.split_in_place(2, 1);
        foundation::Vector2f s = sampling_context.next2<foundation::Vector2f>();

        if (m_previous_iter_sample_weight <= 0.0f || m_root_node.radiance_sum() <= 0.0f)
        {
            d_tree_sample.direction = foundation::sample_sphere_uniform(s);
            d_tree_sample.pdf = foundation::RcpFourPi<float>();
        }
        else
        {
            d_tree_sample.pdf = 1.0f;
            const foundation::Vector2f direction = m_root_node.sample(s, d_tree_sample.pdf);
            d_tree_sample.direction = cylindrical_to_cartesian(direction);
        }
    }

    float pdf(const foundation::Vector3f& direction) const
    {
        if(m_previous_iter_sample_weight <= 0.0f || m_root_node.radiance_sum() <= 0.0f)
            return foundation::RcpFourPi<float>();

        foundation::Vector2f dir = cartesian_to_cylindrical(direction);
        return m_root_node.pdf(dir);
    }
    
    void halve_sample_weight()
    {
        m_current_iter_sample_weight = 0.5f * m_current_iter_sample_weight.load(std::memory_order_relaxed);
        m_previous_iter_sample_weight *= 0.5f;
    }

    size_t node_count() const
    {
        return m_root_node.node_count();
    }

    size_t max_depth() const
    {
        return m_root_node.max_depth();
    }

    size_t depth(const foundation::Vector2f& direction) const
    {
        foundation::Vector2f local_direction = direction;

        return m_root_node.depth(local_direction);
    }

    void build()
    {
        m_previous_iter_sample_weight = m_current_iter_sample_weight.load(std::memory_order_relaxed);
        m_root_node.build_radiance_sums();
    }

    void restructure(const float subdiv_threshold)
    {
        m_root_node.restructure(m_root_node.radiance_sum(), subdiv_threshold);
        m_current_iter_sample_weight.store(0.0f, std::memory_order_relaxed);
        m_is_built = true;
    }

    float sample_weight() const
    {
        return m_previous_iter_sample_weight;
    }

    float mean() const
    {
        if (m_previous_iter_sample_weight <= 0.0f)
            return 0.0f;

        return m_root_node.radiance_sum() * (1.0f / m_previous_iter_sample_weight) * foundation::RcpFourPi<float>();
    }

    inline float bsdf_sampling_fraction() const
    {
        if(m_parameters.m_bsdf_sampling_fraction_mode == BSDFSamplingFractionMode::Learn)
            return logistic(m_theta);
        else
            return m_parameters.m_fixed_bsdf_sampling_fraction;
    }

private:

    void acquire_optimization_spin_lock()
    {
        while(m_atomic_flag.test_and_set(std::memory_order_acquire))
            ;
    }

    void release_optimization_spin_lock()
    {
        m_atomic_flag.clear(std::memory_order_release);
    }

    // BSDF sampling fraction optimization procedure.
    // Implementation of Algorithm 3 in chapter "Practical Path Guiding in Production" Müller, 2019
    // released in "Path Guiding in Production" Siggraph Course 2019, Vorba et. al.

    void adam_step(const float d_theta)
    {
        ++m_optimization_step_count;
        // TODO: square root of Beta1 or Beta2?
        const float debiased_learning_rate = m_parameters.m_learning_rate * std::sqrt(1.0f - std::pow(Beta1, m_optimization_step_count)) /
                                             (1.0f - std::pow(Beta2, m_optimization_step_count));
        m_first_moment = Beta1 * m_first_moment + (1.0f - Beta1) * d_theta;
        m_second_moment = Beta2 * m_second_moment + (1.0f - Beta2) * d_theta * d_theta;
        m_theta -= debiased_learning_rate * m_first_moment / (std::sqrt(m_second_moment) + OptimizationEpsilon);

        m_theta = foundation::clamp(m_theta, -20.0f, 20.0f);
    }

    void optimization_step(const DTreeRecord& dtree_record)
    {
        acquire_optimization_spin_lock();

        const float sampling_fraction = bsdf_sampling_fraction();
        const float combined_pdf = sampling_fraction * dtree_record.bsdf_pdf + (1.0f - sampling_fraction) * dtree_record.d_tree_pdf;
        const float d_sampling_fraction = -dtree_record.product * (dtree_record.bsdf_pdf - dtree_record.d_tree_pdf) /
                                  (dtree_record.wo_pdf * combined_pdf);
        const float d_theta = d_sampling_fraction * sampling_fraction * (1.0f - sampling_fraction);
        const float reg_gradient = m_theta * Regularization;

        // TODO: account for sample weight
        adam_step(d_theta + reg_gradient);

        release_optimization_spin_lock();
    }

    QuadTreeNode m_root_node;
    std::atomic<float> m_current_iter_sample_weight;
    float m_previous_iter_sample_weight;
    bool m_is_built;

    std::atomic_flag m_atomic_flag;

    size_t m_optimization_step_count;
    float m_first_moment;
    float m_second_moment;
    float m_theta;

    const GPTParameters& m_parameters;
};

struct DTreeStatistics
{
    DTreeStatistics()
      : max_dtree_depth(0)
      , min_max_dtree_depth(std::numeric_limits<size_t>::max())
      , average_max_dtree_depth(0)
      , max_stree_depth(0)
      , min_max_stree_depth(std::numeric_limits<size_t>::max())
      , average_max_stree_depth(0)
      , max_mean_radiance(0)
      , min_mean_radiance(std::numeric_limits<float>::max())
      , average_mean_radiance(0)
      , max_dtree_nodes(0)
      , min_dtree_nodes(std::numeric_limits<size_t>::max())
      , average_dtree_nodes(0)
      , max_sample_weight(0)
      , min_sample_weight(std::numeric_limits<float>::max())
      , average_sample_weight(0)
      , num_dtrees(0)
      , num_stree_nodes(0)
    {}

    size_t max_dtree_depth;
    size_t min_max_dtree_depth;
    float  average_max_dtree_depth;
    size_t max_stree_depth;
    size_t min_max_stree_depth;
    float  average_max_stree_depth;
    float  max_mean_radiance;
    float  min_mean_radiance;
    float  average_mean_radiance;
    size_t max_dtree_nodes;
    size_t min_dtree_nodes;
    float  average_dtree_nodes;
    float  max_sample_weight;
    float  min_sample_weight;
    float  average_sample_weight;
    size_t num_dtrees;
    size_t num_stree_nodes;

    void build()
    {
        assert(num_dtrees > 0);

        average_max_dtree_depth /= num_dtrees;
        average_max_stree_depth /= num_dtrees;
        average_dtree_nodes /= num_dtrees;
        average_mean_radiance /= num_dtrees;
        average_sample_weight /= num_dtrees;
    }
};


class STreeNode {
  public:
    STreeNode(const GPTParameters& parameters)
      : m_axis(0)
      , m_d_tree(new DTree(parameters))
    {}

    STreeNode(const unsigned int parent_axis, const DTree* parent_d_tree)
      : m_axis((parent_axis + 1) % 3)
      , m_d_tree(new DTree(*parent_d_tree))
    {
        m_d_tree->halve_sample_weight();
    }

    DTree* get_d_tree(foundation::Vector3f &point, foundation::Vector3f &size)
    {
        if(is_leaf())
            return m_d_tree.get();
        else
        {
            size[m_axis] *= 0.5f;
            return choose_node(point)->get_d_tree(point, size);
        }
    }

    void subdivide(const size_t required_samples)
    {
        if(is_leaf())
        {
            if (m_d_tree->sample_weight() > required_samples)
                subdivide();
            else
                return;
        }
        
        m_first_node->subdivide(required_samples);
        m_second_node->subdivide(required_samples);
    }

    void record(const foundation::AABB3f& splat_aabb, const foundation::AABB3f& node_aabb, const DTreeRecord& d_tree_record)
    {
        const foundation::AABB3f intersection_aabb(foundation::AABB3f::intersect(splat_aabb, node_aabb));

        if(!intersection_aabb.is_valid())
            return;

        const float intersection_volume = intersection_aabb.volume();

        if(intersection_volume <= 0.0f)
            return;

        if(is_leaf())
            m_d_tree->record(DTreeRecord{
                                d_tree_record.direction,
                                d_tree_record.radiance,
                                d_tree_record.wo_pdf,
                                d_tree_record.bsdf_pdf,
                                d_tree_record.d_tree_pdf,
                                d_tree_record.sample_weight * intersection_volume,
                                d_tree_record.product,
                                d_tree_record.is_delta});
        else
        {
            const foundation::Vector3f node_size = node_aabb.extent();
            foundation::Vector3f offset(0.0f);
            offset[m_axis] = node_size[m_axis] * 0.5f;

            m_first_node->record(splat_aabb, foundation::AABB3f(node_aabb.min, node_aabb.max - offset), d_tree_record);
            m_second_node->record(splat_aabb, foundation::AABB3f(node_aabb.min + offset, node_aabb.max), d_tree_record);
        }
    }

    void restructure(const float subdiv_threshold)
    {
        if(is_leaf())
            m_d_tree->restructure(subdiv_threshold);
        else
        {
            m_first_node->restructure(subdiv_threshold);
            m_second_node->restructure(subdiv_threshold);
        }
    }

    void build()
    {
        if(is_leaf())
            m_d_tree->build();
        else
        {
            m_first_node->build();
            m_second_node->build();
        }
    }

    void gather_statistics(DTreeStatistics& statistics, const size_t depth = 1) const
    {
        statistics.num_stree_nodes++;
        if(is_leaf())
        {
            ++statistics.num_dtrees;
            const size_t d_tree_depth = m_d_tree->max_depth();
            statistics.max_dtree_depth = std::max(statistics.max_dtree_depth, d_tree_depth);
            statistics.min_max_dtree_depth = std::min(statistics.min_max_dtree_depth, d_tree_depth);
            statistics.average_max_dtree_depth += d_tree_depth;
            statistics.max_stree_depth = std::max(statistics.max_stree_depth, depth);
            statistics.min_max_stree_depth = std::min(statistics.min_max_stree_depth, depth);
            statistics.average_max_stree_depth += depth;

            const float mean_radiance = m_d_tree->mean();
            statistics.max_mean_radiance = std::max(statistics.max_mean_radiance, mean_radiance);
            statistics.min_mean_radiance = std::min(statistics.max_mean_radiance, mean_radiance);
            statistics.average_mean_radiance += mean_radiance;

            const size_t node_count = m_d_tree->node_count();
            statistics.max_dtree_nodes = std::max(statistics.max_dtree_nodes, node_count);
            statistics.min_dtree_nodes = std::min(statistics.min_dtree_nodes, node_count);
            statistics.average_dtree_nodes += node_count;

            const float sample_weight = m_d_tree->sample_weight();
            statistics.max_sample_weight = std::max(statistics.max_sample_weight, sample_weight);
            statistics.min_sample_weight = std::min(statistics.min_sample_weight, sample_weight);
            statistics.average_sample_weight += sample_weight;
        }
        else
        {
            m_first_node->gather_statistics(statistics, depth + 1);
            m_second_node->gather_statistics(statistics, depth + 1);
        }
    }

private:
    STreeNode* choose_node(foundation::Vector3f &point) const
    {
        if(point[m_axis] < 0.5f)
        {
            point[m_axis] *= 2.0f;
            return m_first_node.get();
        }
        else
        {
            point[m_axis] = (point[m_axis] - 0.5f) * 2.0f;
            return m_second_node.get();
        }
    }

    void subdivide()
    {
        if(is_leaf())
        {
            m_first_node.reset(new STreeNode(m_axis, m_d_tree.get()));
            m_second_node.reset(new STreeNode(m_axis, m_d_tree.get()));
            m_d_tree.reset(nullptr);
        }
    }

    inline bool is_leaf() const
    {
        return m_d_tree != nullptr;
    }

    std::unique_ptr<STreeNode> m_first_node;
    std::unique_ptr<STreeNode> m_second_node;

    // This member is only set if the node is a leaf node and nullptr otherwise.
    std::unique_ptr<DTree> m_d_tree;

    // This node's split axis.
    unsigned int m_axis;
};



class STree {
public:
    STree(const foundation::AABB3f& scene_aabb, const GPTParameters& parameters)
     : m_parameters(parameters)
     , m_scene_aabb(scene_aabb)
     , m_is_built(false)
     , m_is_final_iteration(false)
    {
        m_root_node.reset(new STreeNode(m_parameters));

        // Grow the AABB into a cube for nicer hierarchical subdivisions.
        const foundation::Vector3f size = m_scene_aabb.extent();
        const float maxSize = foundation::max_value(size);
        m_scene_aabb.max = m_scene_aabb.min + foundation::Vector3f(maxSize);
    }

    DTree* get_d_tree(const foundation::Vector3f& point, foundation::Vector3f& d_tree_voxel_size) {
        d_tree_voxel_size = m_scene_aabb.extent();
        foundation::Vector3f transformed_point = point - m_scene_aabb.min;
        transformed_point /= d_tree_voxel_size;

        return m_root_node->get_d_tree(transformed_point, d_tree_voxel_size);
    }

    DTree* get_d_tree(const foundation::Vector3f& point) {
        foundation::Vector3f d_tree_voxel_size;
        return get_d_tree(point, d_tree_voxel_size);
    }

    void record(DTree* dtree, const foundation::Vector3f& point, const foundation::Vector3f& dtree_node_size, DTreeRecord dtree_record, SamplingContext& sampling_context)
    {
        switch (m_parameters.m_spatial_filter)
        {
        case SpatialFilter::Nearest:
            dtree->record(dtree_record);
            break;

        case SpatialFilter::Stochastic:
        {
            foundation::Vector3f offset = dtree_node_size;

            sampling_context.split_in_place(3, 1);

            offset *= (sampling_context.next2<foundation::Vector3f>() - foundation::Vector3f(0.5f));

            foundation::Vector3f origin = clip_vector_to_aabb(m_scene_aabb, point + offset);
            DTree* stochastic_dtree = get_d_tree(origin);
            stochastic_dtree->record(dtree_record);
            break;
        }

        case SpatialFilter::Box:
            box_filter_splat(point, dtree_node_size, dtree_record);
            break;
        }
    }

    const foundation::AABB3f& aabb() const
    {
        return m_scene_aabb;
    }

    void build(const size_t iteration)
    {
        m_root_node->build();

        const size_t required_samples = std::sqrt(std::pow(2, iteration) * m_parameters.m_samples_per_pass * 0.25f) * SpatialSubdivisionThreshold;
        m_root_node->subdivide(required_samples);
        m_root_node->restructure(DTreeThreshold);

        DTreeStatistics statistics;
        m_root_node->gather_statistics(statistics);
        statistics.build();

        RENDERER_LOG_INFO(
            "SD tree statistics: [min, max, avg]\n"
            "  DTree Depth     = [%s, %s, %s]\n"
            "  STree Depth     = [%s, %s, %s]\n"
            "  Mean radiance   = [%s, %s, %s]\n"
            "  Node count      = [%s, %s, %s]\n"
            "  Sample weight   = [%s, %s, %s]\n\n",
            foundation::pretty_uint(statistics.min_max_dtree_depth).c_str(), foundation::pretty_uint(statistics.max_dtree_depth).c_str(), foundation::pretty_scalar(statistics.average_max_dtree_depth, 2).c_str(),
            foundation::pretty_uint(statistics.min_max_stree_depth).c_str(), foundation::pretty_uint(statistics.max_stree_depth).c_str(), foundation::pretty_scalar(statistics.average_max_stree_depth, 2).c_str(),
            foundation::pretty_scalar(statistics.min_mean_radiance, 4).c_str(), foundation::pretty_scalar(statistics.max_mean_radiance, 4).c_str(), foundation::pretty_scalar(statistics.average_mean_radiance, 4).c_str(),
            foundation::pretty_uint(statistics.min_dtree_nodes).c_str(), foundation::pretty_uint(statistics.max_dtree_nodes).c_str(), foundation::pretty_scalar(statistics.average_dtree_nodes, 4).c_str(),
            foundation::pretty_scalar(statistics.min_sample_weight, 4).c_str(), foundation::pretty_scalar(statistics.max_sample_weight, 4).c_str(), foundation::pretty_scalar(statistics.average_sample_weight, 4).c_str());

        m_is_built = true;
    }

    bool is_built()
    {
      return m_is_built;
    }

    void start_final_iteration()
    {
        m_is_final_iteration = true;
    }

    bool is_final_iteration()
    {
        return m_is_final_iteration;
    }

private:
    void box_filter_splat(const foundation::Vector3f &p, const foundation::Vector3f &dtree_node_size, DTreeRecord dtree_record)
    {
        const foundation::AABB3f splat_aabb(p - dtree_node_size * 0.5f, p + dtree_node_size * 0.5f);

        assert(splat_aabb.is_valid());

        dtree_record.sample_weight /= splat_aabb.volume();
        m_root_node->record(foundation::AABB3f(p - dtree_node_size * 0.5f, p + dtree_node_size * 0.5f), m_scene_aabb, dtree_record);
    }

    /// Clip point to lie within bounding box.
    foundation::Vector3f clip_vector_to_aabb(const foundation::AABB3f &aabb, const foundation::Vector3f &p)
    {
        foundation::Vector3f result = p;
        for (int i = 0; i < foundation::Vector3f::Dimension; ++i)
        {
            result[i] = std::min(std::max(result[i], aabb.min[i]), aabb.max[i]);
        }
        return result;
    }

    const GPTParameters m_parameters;
    std::unique_ptr<STreeNode> m_root_node;
    foundation::AABB3f m_scene_aabb;
    bool m_is_built;
    bool m_is_final_iteration;
};

struct GPTVertex
{
    DTree* dtree;
    foundation::Vector3f dtree_node_size;
    foundation::Vector3f point;
    foundation::Vector3f dir;

    renderer::Spectrum throughput;
    renderer::Spectrum bsdf_val;
    renderer::Spectrum radiance;

    float wo_pdf;
    float bsdf_pdf;
    float dtree_pdf;
    bool  is_delta;

    void add_radiance(const renderer::Spectrum& r);
    
    void record_to_tree(STree&                    sd_tree,
                        float                     statistical_weight,
                        SamplingContext&          sampling_context);
};

class GPTVertexPath
{
  public:
    GPTVertexPath();
    void add_vertex(const GPTVertex&              vertex);
    void add_radiance(const renderer::Spectrum&   r);

    void record_to_tree(STree&                    sd_tree,
                        float                     statistical_weight,
                        SamplingContext&          sampling_context);

    bool is_full() const;

    void set_sampling_fraction(const float sampling_fraction)
    {
        m_sampling_fraction = sampling_fraction;
    }

    float get_sampling_fraction() const
    {
        return m_sampling_fraction;
    }


  private:
    std::array<GPTVertex, 32> path;
    int index;
    float m_sampling_fraction;
};

}