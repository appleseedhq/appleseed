//
// Code in this file is from Müller et al's implementation of practical path guiding (https://github.com/Tom94/practical-path-guiding)
// which was released under the GNU GENERAL PUBLIC LICENSE

#pragma once

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"

// appleseed.foundation headers.
#include "foundation/math/aabb.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <array>
#include <atomic>
#include <cmath>
#include <functional>
#include <stack>
#include <vector>

//
// SD-Tree mplementation for "Practical Path Guiding for Efficient Light-Transport Simulation" [Müller et al. 2017].
//


namespace renderer {

// enum class ESpatialFilter {
//     ENearest,
//     EStochasticBox,
//     EBox,
// };

// enum class EDirectionalFilter
// {
//     ENearest,
//     EBox,
// };

// class DTreeSample
// {
//   public:
//     foundation::Vector3f m_dir;
//     float m_probability;
// };

// class DTreeWrapper
// {
//   public:
//     class DTreeRecord
//     {};

//     void record(
//         const DTreeRecord&           rec,
//         EDirectionalFilter           directionalFilter);

//     float pdf(
//         const foundation::Vector3f&  dir) const;

//     void sample(
//         SamplingContext&             sampling_context,
//         DTreeSample&                 sample);

// };

// class STree
// {
//   public:
//     DTreeWrapper* get_d_tree_wrapper(const foundation::Vector3d& point);

//   private:
//     DTreeWrapper m_d_tree_wrapper;
// };





enum class ESampleCombination {
    EDiscard,
    EDiscardWithAutomaticBudget,
    EInverseVariance,
};

enum class EBsdfSamplingFractionLoss {
    ENone,
    EKL,
    EVariance,
};

enum class ESpatialFilter {
    ENearest,
    EStochasticBox,
    EBox,
};

enum class EDirectionalFilter {
    ENearest,
    EBox,
};

const float sd_tree_epsilon = 1e-4f;
const size_t sTreeThreshold = 12000;
const float dTreeThreshold = 0.01;
const size_t sdTreeMaxMemory = -1;

static void addToAtomicfloatSDTree(std::atomic<float>& var, float val)
{
  auto current = var.load();
  while (!var.compare_exchange_weak(current, current + val))
    ;
}

inline float logisticSDTree(float x)
{
  return 1 / (1 + std::exp(-x));
}

class QuadTreeNode {
public:
    QuadTreeNode() {
        m_children = {};
        for (size_t i = 0; i < m_sum.size(); ++i) {
            m_sum[i].store(0, std::memory_order_relaxed);
        }
    }

    void setSum(int index, float val) {
        m_sum[index].store(val, std::memory_order_relaxed);
    }

    float sum(int index) const {
        return m_sum[index].load(std::memory_order_relaxed);
    }

    void copyFrom(const QuadTreeNode& arg) {
        for (int i = 0; i < 4; ++i) {
            setSum(i, arg.sum(i));
            m_children[i] = arg.m_children[i];
        }
    }

    QuadTreeNode(const QuadTreeNode& arg) {
        copyFrom(arg);
    }

    QuadTreeNode& operator=(const QuadTreeNode& arg) {
        copyFrom(arg);
        return *this;
    }

    void setChild(int idx, uint16_t val) {
        m_children[idx] = val;
    }

    uint16_t child(int idx) const {
        return m_children[idx];
    }

    void setSum(float val) {
        for (int i = 0; i < 4; ++i) {
            setSum(i, val);
        }
    }

    int childIndex(foundation::Vector2f& p) const {
        int res = 0;
        for (int i = 0; i < foundation::Vector2f::Dimension; ++i) {
            if (p[i] < 0.5f) {
                p[i] *= 2;
            } else {
                p[i] = (p[i] - 0.5f) * 2;
                res |= 1 << i;
            }
        }

        return res;
    }

    // Evaluates the directional irradiance *sum density* (i.e. sum / area) at a given location p.
    // To obtain radiance, the sum density (result of this function) must be divided
    // by the total statistical weight of the estimates that were summed up.
    float eval(foundation::Vector2f& p, const std::vector<QuadTreeNode>& nodes) const {
        assert(p.x >= 0 && p.x <= 1 && p.y >= 0 && p.y <= 1);
        const int index = childIndex(p);
        if (isLeaf(index)) {
            return 4 * sum(index);
        } else {
            return 4 * nodes[child(index)].eval(p, nodes);
        }
    }

    float pdf(foundation::Vector2f& p, const std::vector<QuadTreeNode>& nodes) const {
        assert(p.x >= 0 && p.x <= 1 && p.y >= 0 && p.y <= 1);
        const int index = childIndex(p);
        if (!(sum(index) > 0)) {
            return 0;
        }

        const float factor = 4 * sum(index) / (sum(0) + sum(1) + sum(2) + sum(3));
        if (isLeaf(index)) {
            return factor;
        } else {
            return factor * nodes[child(index)].pdf(p, nodes);
        }
    }

    int depthAt(foundation::Vector2f& p, const std::vector<QuadTreeNode>& nodes) const {
        assert(p.x >= 0 && p.x <= 1 && p.y >= 0 && p.y <= 1);
        const int index = childIndex(p);
        if (isLeaf(index)) {
            return 1;
        } else {
            return 1 + nodes[child(index)].depthAt(p, nodes);
        }
    }

    foundation::Vector2f sample(SamplingContext& sampling_context, const std::vector<QuadTreeNode>& nodes) const {
        int index = 0;

        float topLeft = sum(0);
        float topRight = sum(1);
        float partial = topLeft + sum(2);
        float total = partial + topRight + sum(3);

        // Should only happen when there are numerical instabilities.
        if (!(total > 0.0f)) {
            sampling_context.split_in_place(2, 1);
            return sampling_context.next2<foundation::Vector2f>();
        }

        float boundary = partial / total;
        foundation::Vector2f origin(0.0f);

        sampling_context.split_in_place(1, 1);
        float sample = sampling_context.next2<float>();

        if (sample < boundary) {
            assert(partial > 0);
            sample /= boundary;
            boundary = topLeft / partial;
        } else {
            partial = total - partial;
            assert(partial > 0);
            origin.x = 0.5f;
            sample = (sample - boundary) / (1.0f - boundary);
            boundary = topRight / partial;
            index |= 1 << 0;
        }

        if (sample < boundary) {
            sample /= boundary;
        } else {
            origin.y = 0.5f;
            sample = (sample - boundary) / (1.0f - boundary);
            index |= 1 << 1;
        }

        if (isLeaf(index)) {
          sampling_context.split_in_place(2, 1);
          return origin + 0.5f * sampling_context.next2<foundation::Vector2f>();
        } else {
            return origin + 0.5f * nodes[child(index)].sample(sampling_context, nodes);
        }
    }

    void record(foundation::Vector2f& p, float irradiance, std::vector<QuadTreeNode>& nodes) {
        assert(p.x >= 0 && p.x <= 1 && p.y >= 0 && p.y <= 1);
        int index = childIndex(p);

        if (isLeaf(index)) {
            addToAtomicfloatSDTree(m_sum[index], irradiance);
        } else {
            nodes[child(index)].record(p, irradiance, nodes);
        }
    }

    float computeOverlappingArea(const foundation::Vector2f& min1, const foundation::Vector2f& max1, const foundation::Vector2f& min2, const foundation::Vector2f& max2) {
        float lengths[2];
        for (int i = 0; i < 2; ++i) {
            lengths[i] = std::max(std::min(max1[i], max2[i]) - std::max(min1[i], min2[i]), 0.0f);
        }
        return lengths[0] * lengths[1];
    }

    void record(const foundation::Vector2f& origin, float size, foundation::Vector2f nodeOrigin, float nodeSize, float value, std::vector<QuadTreeNode>& nodes) {
        float childSize = nodeSize / 2;
        for (int i = 0; i < 4; ++i) {
            foundation::Vector2f childOrigin = nodeOrigin;
            if (i & 1) { childOrigin[0] += childSize; }
            if (i & 2) { childOrigin[1] += childSize; }

            float w = computeOverlappingArea(origin, origin + foundation::Vector2f(size), childOrigin, childOrigin + foundation::Vector2f(childSize));
            if (w > 0.0f) {
                if (isLeaf(i)) {
                    addToAtomicfloatSDTree(m_sum[i], value * w);
                } else {
                    nodes[child(i)].record(origin, size, childOrigin, childSize, value, nodes);
                }
            }
        }
    }

    bool isLeaf(int index) const {
        return child(index) == 0;
    }

    // Ensure that each quadtree node's sum of irradiance estimates
    // equals that of all its children.
    void build(std::vector<QuadTreeNode>& nodes) {
        for (int i = 0; i < 4; ++i) {
            // During sampling, all irradiance estimates are accumulated in
            // the leaves, so the leaves are built by definition.
            if (isLeaf(i)) {
                continue;
            }

            QuadTreeNode& c = nodes[child(i)];

            // Recursively build each child such that their sum becomes valid...
            c.build(nodes);

            // ...then sum up the children's sums.
            float sum = 0;
            for (int j = 0; j < 4; ++j) {
                sum += c.sum(j);
            }
            setSum(i, sum);
        }
    }

private:
    std::array<std::atomic<float>, 4> m_sum;
    std::array<uint16_t, 4> m_children;
};



class DTree {
public:
    DTree() {
        m_atomic.sum.store(0, std::memory_order_relaxed);
        m_maxDepth = 0;
        m_nodes.emplace_back();
        m_nodes.front().setSum(0.0f);
    }

    const QuadTreeNode& node(size_t i) const {
        return m_nodes[i];
    }

    float mean() const {
        if (m_atomic.statisticalWeight == 0) {
            return 0;
        }
        const float factor = 1 / (M_PI * 4 * m_atomic.statisticalWeight);
        return factor * m_atomic.sum;
    }

    void recordIrradiance(foundation::Vector2f p, float irradiance, float statisticalWeight, EDirectionalFilter directionalFilter) {
        if (std::isfinite(statisticalWeight) && statisticalWeight > 0) {
            addToAtomicfloatSDTree(m_atomic.statisticalWeight, statisticalWeight);

            if (std::isfinite(irradiance) && irradiance > 0) {
                if (directionalFilter == EDirectionalFilter::ENearest) {
                    m_nodes[0].record(p, irradiance * statisticalWeight, m_nodes);
                } else {
                    int depth = depthAt(p);
                    float size = std::pow(0.5f, depth);

                    foundation::Vector2f origin = p;
                    origin.x -= size / 2;
                    origin.y -= size / 2;
                    m_nodes[0].record(origin, size, foundation::Vector2f(0.0f), 1.0f, irradiance * statisticalWeight / (size * size), m_nodes);
                }
            }
        }
    }

    float pdf(foundation::Vector2f p) const {
        if (!(mean() > 0)) {
            return 1 / (4 * M_PI);
        }

        return m_nodes[0].pdf(p, m_nodes) / (4 * M_PI);
    }

    int depthAt(foundation::Vector2f p) const {
        return m_nodes[0].depthAt(p, m_nodes);
    }

    int depth() const {
        return m_maxDepth;
    }

    foundation::Vector2f sample(SamplingContext& sampling_context) const {
        if (!(mean() > 0)) {
            sampling_context.split_in_place(2, 1);
            return sampling_context.next2<foundation::Vector2f>();
        }

        foundation::Vector2f res = m_nodes[0].sample(sampling_context, m_nodes);

        res.x = foundation::clamp(res.x, 0.0f, 1.0f);
        res.y = foundation::clamp(res.y, 0.0f, 1.0f);

        return res;
    }

    size_t numNodes() const {
        return m_nodes.size();
    }

    float statisticalWeight() const {
        return m_atomic.statisticalWeight;
    }

    void setStatisticalWeight(float statisticalWeight) {
        m_atomic.statisticalWeight = statisticalWeight;
    }

    void reset(const DTree& previousDTree, int newMaxDepth, float subdivisionThreshold) {
        m_atomic = Atomic{};
        m_maxDepth = 0;
        m_nodes.clear();
        m_nodes.emplace_back();

        struct StackNode {
            size_t nodeIndex;
            size_t otherNodeIndex;
            const DTree* otherDTree;
            int depth;
        };

        std::stack<StackNode> nodeIndices;
        nodeIndices.push({0, 0, &previousDTree, 1});

        const float total = previousDTree.m_atomic.sum;
        
        // Create the topology of the new DTree to be the refined version
        // of the previous DTree. Subdivision is recursive if enough energy is there.
        while (!nodeIndices.empty()) {
            StackNode sNode = nodeIndices.top();
            nodeIndices.pop();

            m_maxDepth = std::max(m_maxDepth, sNode.depth);

            for (int i = 0; i < 4; ++i) {
                const QuadTreeNode& otherNode = sNode.otherDTree->m_nodes[sNode.otherNodeIndex];
                const float fraction = total > 0 ? (otherNode.sum(i) / total) : std::pow(0.25f, sNode.depth);
                assert(fraction <= 1.0f + sd_tree_epsilon);

                if (sNode.depth < newMaxDepth && fraction > subdivisionThreshold) {
                    if (!otherNode.isLeaf(i)) {
                        assert(sNode.otherDTree == &previousDTree);
                        nodeIndices.push(StackNode{m_nodes.size(), otherNode.child(i), &previousDTree, sNode.depth + 1});
                    } else {
                        nodeIndices.push(StackNode{m_nodes.size(), m_nodes.size(), this, sNode.depth + 1});
                    }

                    m_nodes[sNode.nodeIndex].setChild(i, static_cast<uint16_t>(m_nodes.size()));
                    m_nodes.emplace_back();
                    m_nodes.back().setSum(otherNode.sum(i) / 4);

                    if (m_nodes.size() > std::numeric_limits<uint16_t>::max()) {
                        RENDERER_LOG_WARNING("DTreeWrapper hit maximum children count.");
                        nodeIndices = std::stack<StackNode>();
                        break;
                    }
                }
            }
        }

        // Uncomment once memory becomes an issue.
        //m_nodes.shrink_to_fit();

        for (auto& node : m_nodes) {
            node.setSum(0);
        }
    }

    size_t approxMemoryFootprint() const {
        return m_nodes.capacity() * sizeof(QuadTreeNode) + sizeof(*this);
    }

    void build() {
        auto& root = m_nodes[0];

        // Build the quadtree recursively, starting from its root.
        root.build(m_nodes);

        // Ensure that the overall sum of irradiance estimates equals
        // the sum of irradiance estimates found in the quadtree.
        float sum = 0;
        for (int i = 0; i < 4; ++i) {
            sum += root.sum(i);
        }
        m_atomic.sum.store(sum);
    }

private:
    std::vector<QuadTreeNode> m_nodes;

    struct Atomic {
        Atomic() {
            sum.store(0, std::memory_order_relaxed);
            statisticalWeight.store(0, std::memory_order_relaxed);
        }

        Atomic(const Atomic& arg) {
            *this = arg;
        }

        Atomic& operator=(const Atomic& arg) {
            sum.store(arg.sum.load(std::memory_order_relaxed), std::memory_order_relaxed);
            statisticalWeight.store(arg.statisticalWeight.load(std::memory_order_relaxed), std::memory_order_relaxed);
            return *this;
        }

        std::atomic<float> sum;
        std::atomic<float> statisticalWeight;

    } m_atomic;

    int m_maxDepth;
};

struct DTreeRecord {
    foundation::Vector3f d;
    float radiance, product;
    float woPdf, bsdfPdf, dTreePdf;
    float statisticalWeight;
    bool isDelta;
};

struct DTreeWrapper {
public:
    DTreeWrapper() {
    }

    void record(const DTreeRecord& rec, EDirectionalFilter directionalFilter, EBsdfSamplingFractionLoss bsdfSamplingFractionLoss) {
        if (!rec.isDelta) {
            float irradiance = rec.radiance / rec.woPdf;
            building.recordIrradiance(dirToCanonical(rec.d), irradiance, rec.statisticalWeight, directionalFilter);
        }

        // if (bsdfSamplingFractionLoss != EBsdfSamplingFractionLoss::ENone && rec.product > 0) {
        //     optimizeBsdfSamplingFraction(rec, bsdfSamplingFractionLoss == EBsdfSamplingFractionLoss::EKL ? 1.0f : 2.0f);
        // }
    }

    static foundation::Vector3f canonicalToDir(foundation::Vector2f p) {
        const float cosTheta = 2 * p.x - 1;
        const float phi = 2 * M_PI * p.y;

        const float sinTheta = sqrt(1 - cosTheta * cosTheta);
        float sinPhi = std::sin(phi), cosPhi = std::cos(phi);

        return {sinTheta * cosPhi, sinTheta * sinPhi, cosTheta};
    }

    static foundation::Vector2f dirToCanonical(const foundation::Vector3f& d) {
        if (!std::isfinite(d.x) || !std::isfinite(d.y) || !std::isfinite(d.z)) {
            return {0, 0};
        }

        const float cosTheta = std::min(std::max(d.z, -1.0f), 1.0f);
        float phi = std::atan2(d.y, d.x);
        while (phi < 0)
            phi += 2.0 * M_PI;

        return {(cosTheta + 1) / 2, phi / (2 * M_PI)};
    }

    void build() {
        building.build();
        sampling = building;
    }

    void reset(int maxDepth, float subdivisionThreshold) {
        building.reset(sampling, maxDepth, subdivisionThreshold);
    }

    foundation::Vector3f sample(SamplingContext& sampling_context) const {
        return canonicalToDir(sampling.sample(sampling_context));
    }

    float pdf(const foundation::Vector3f& dir) const {
        return sampling.pdf(dirToCanonical(dir));
    }

    float diff(const DTreeWrapper& other) const {
        return 0.0f;
    }

    int depth() const {
        return sampling.depth();
    }

    size_t numNodes() const {
        return sampling.numNodes();
    }

    float meanRadiance() const {
        return sampling.mean();
    }

    float statisticalWeight() const {
        return sampling.statisticalWeight();
    }

    float statisticalWeightBuilding() const {
        return building.statisticalWeight();
    }

    void setStatisticalWeightBuilding(float statisticalWeight) {
        building.setStatisticalWeight(statisticalWeight);
    }

    size_t approxMemoryFootprint() const {
        return building.approxMemoryFootprint() + sampling.approxMemoryFootprint();
    }

    // inline float bsdfSamplingFraction(float variable) const {
    //     return logisticSDTree(variable);
    // }

    // inline float dBsdfSamplingFraction_dVariable(float variable) const {
    //     float fraction = bsdfSamplingFraction(variable);
    //     return fraction * (1 - fraction);
    // }

    // inline float bsdfSamplingFraction() const {
    //     return bsdfSamplingFraction(bsdfSamplingFractionOptimizer.variable());
    // }

    // void optimizeBsdfSamplingFraction(const DTreeRecord& rec, float ratioPower) {
    //     m_lock.lock();

    //     // GRADIENT COMPUTATION
    //     float variable = bsdfSamplingFractionOptimizer.variable();
    //     float samplingFraction = bsdfSamplingFraction(variable);

    //     // Loss gradient w.r.t. sampling fraction
    //     float mixPdf = samplingFraction * rec.bsdfPdf + (1 - samplingFraction) * rec.dTreePdf;
    //     float ratio = std::pow(rec.product / mixPdf, ratioPower);
    //     float dLoss_dSamplingFraction = -ratio / rec.woPdf * (rec.bsdfPdf - rec.dTreePdf);

    //     // Chain rule to get loss gradient w.r.t. trainable variable
    //     float dLoss_dVariable = dLoss_dSamplingFraction * dBsdfSamplingFraction_dVariable(variable);

    //     // We want some regularization such that our parameter does not become too big.
    //     // We use l2 regularization, resulting in the following linear gradient.
    //     float l2RegGradient = 0.01f * variable;

    //     float lossGradient = l2RegGradient + dLoss_dVariable;

    //     // ADAM GRADIENT DESCENT
    //     bsdfSamplingFractionOptimizer.append(lossGradient, rec.statisticalWeight);

    //     m_lock.unlock();
    // }

    // void dump(BlobWriter& blob, const foundation::Vector3f& p, const foundation::Vector3f& size) const {
    //     blob
    //         << (float)p.x << (float)p.y << (float)p.z
    //         << (float)size.x << (float)size.y << (float)size.z
    //         << (float)sampling.mean() << (uint64_t)sampling.statisticalWeight() << (uint64_t)sampling.numNodes();

    //     for (size_t i = 0; i < sampling.numNodes(); ++i) {
    //         const auto& node = sampling.node(i);
    //         for (int j = 0; j < 4; ++j) {
    //             blob << (float)node.sum(j) << (uint16_t)node.child(j);
    //         }
    //     }
    // }

private:
    DTree building;
    DTree sampling;

    // AdamOptimizer bsdfSamplingFractionOptimizer{0.01f};

    class SpinLock {
    public:
        SpinLock() {
            m_mutex.clear(std::memory_order_release);
        }

        SpinLock(const SpinLock& other) { m_mutex.clear(std::memory_order_release); }
        SpinLock& operator=(const SpinLock& other) { return *this; }

        void lock() {
            while (m_mutex.test_and_set(std::memory_order_acquire)) { }
        }

        void unlock() {
            m_mutex.clear(std::memory_order_release);
        }
    private:
        std::atomic_flag m_mutex;
    } m_lock;
};


struct STreeNode {
    STreeNode() {
        children = {};
        isLeaf = true;
        axis = 0;
    }

    int childIndex(foundation::Vector3f& p) const {
        if (p[axis] < 0.5f) {
            p[axis] *= 2;
            return 0;
        } else {
            p[axis] = (p[axis] - 0.5f) * 2;
            return 1;
        }
    }

    int nodeIndex(foundation::Vector3f& p) const {
        return children[childIndex(p)];
    }

    DTreeWrapper* dTreeWrapper(foundation::Vector3f& p, foundation::Vector3f& size, std::vector<STreeNode>& nodes) {
        assert(p[axis] >= 0 && p[axis] <= 1);
        if (isLeaf) {
            return &dTree;
        } else {
            size[axis] /= 2;
            return nodes[nodeIndex(p)].dTreeWrapper(p, size, nodes);
        }
    }

    const DTreeWrapper* dTreeWrapper() const {
        return &dTree;
    }

    int depth(foundation::Vector3f& p, const std::vector<STreeNode>& nodes) const {
        assert(p[axis] >= 0 && p[axis] <= 1);
        if (isLeaf) {
            return 1;
        } else {
            return 1 + nodes[nodeIndex(p)].depth(p, nodes);
        }
    }

    int depth(const std::vector<STreeNode>& nodes) const {
        int result = 1;

        if (!isLeaf) {
            for (auto c : children) {
                result = std::max(result, 1 + nodes[c].depth(nodes));
            }
        }

        return result;
    }

    void forEachLeaf(
        std::function<void(const DTreeWrapper*, const foundation::Vector3f&, const foundation::Vector3f&)> func,
        foundation::Vector3f p, foundation::Vector3f size, const std::vector<STreeNode>& nodes) const {

        if (isLeaf) {
            func(&dTree, p, size);
        } else {
            size[axis] /= 2;
            for (int i = 0; i < 2; ++i) {
                foundation::Vector3f childP = p;
                if (i == 1) {
                    childP[axis] += size[axis];
                }

                nodes[children[i]].forEachLeaf(func, childP, size, nodes);
            }
        }
    }

    float computeOverlappingVolume(const foundation::Vector3f& min1, const foundation::Vector3f& max1, const foundation::Vector3f& min2, const foundation::Vector3f& max2) {
        float lengths[3];
        for (int i = 0; i < 3; ++i) {
            lengths[i] = std::max(std::min(max1[i], max2[i]) - std::max(min1[i], min2[i]), 0.0f);
        }
        return lengths[0] * lengths[1] * lengths[2];
    }

    void record(const foundation::Vector3f& min1, const foundation::Vector3f& max1, foundation::Vector3f min2, foundation::Vector3f size2, const DTreeRecord& rec, EDirectionalFilter directionalFilter, EBsdfSamplingFractionLoss bsdfSamplingFractionLoss, std::vector<STreeNode>& nodes) {
        float w = computeOverlappingVolume(min1, max1, min2, min2 + size2);
        if (w > 0) {
            if (isLeaf) {
                dTree.record({ rec.d, rec.radiance, rec.product, rec.woPdf, rec.bsdfPdf, rec.dTreePdf, rec.statisticalWeight * w, rec.isDelta }, directionalFilter, bsdfSamplingFractionLoss);
            } else {
                size2[axis] /= 2;
                for (int i = 0; i < 2; ++i) {
                    if (i & 1) {
                        min2[axis] += size2[axis];
                    }

                    nodes[children[i]].record(min1, max1, min2, size2, rec, directionalFilter, bsdfSamplingFractionLoss, nodes);
                }
            }
        }
    }

    bool isLeaf;
    DTreeWrapper dTree;
    int axis;
    std::array<uint32_t, 2> children;
};



class STree {
public:
    STree(const foundation::AABB3f& aabb)
     : m_aabb(aabb)
     , m_is_built(false)
     , m_is_final_iteration(false)
    {
        clear();

        // Enlarge AABB to turn it into a cube. This has the effect
        // of nicer hierarchical subdivisions.
        foundation::Vector3f size = m_aabb.max - m_aabb.min;
        float maxSize = std::max(std::max(size.x, size.y), size.z);
        m_aabb.max = m_aabb.min + foundation::Vector3f(maxSize);
    }

    void clear() {
        m_nodes.clear();
        m_nodes.emplace_back();
    }

    void subdivideAll() {
        int nNodes = (int)m_nodes.size();
        for (int i = 0; i < nNodes; ++i) {
            if (m_nodes[i].isLeaf) {
                subdivide(i, m_nodes);
            }
        }
    }

    void subdivide(int nodeIdx, std::vector<STreeNode>& nodes) {
        // Add 2 child nodes
        nodes.resize(nodes.size() + 2);

        if (nodes.size() > std::numeric_limits<uint32_t>::max()) {
            RENDERER_LOG_WARNING("DTreeWrapper hit maximum children count.");
            return;
        }

        STreeNode& cur = nodes[nodeIdx];
        for (int i = 0; i < 2; ++i) {
            uint32_t idx = (uint32_t)nodes.size() - 2 + i;
            cur.children[i] = idx;
            nodes[idx].axis = (cur.axis + 1) % 3;
            nodes[idx].dTree = cur.dTree;
            nodes[idx].dTree.setStatisticalWeightBuilding(nodes[idx].dTree.statisticalWeightBuilding() / 2);
        }
        cur.isLeaf = false;
        cur.dTree = {}; // Reset to an empty dtree to save memory.
    }

    DTreeWrapper* dTreeWrapper(foundation::Vector3f p, foundation::Vector3f& size) {
        size = m_aabb.extent();
        p = foundation::Vector3f(p - m_aabb.min);
        p.x /= size.x;
        p.y /= size.y;
        p.z /= size.z;

        return m_nodes[0].dTreeWrapper(p, size, m_nodes);
    }

    DTreeWrapper* dTreeWrapper(foundation::Vector3f p) {
        foundation::Vector3f size;
        return dTreeWrapper(p, size);
    }

    void forEachDTreeWrapperConst(std::function<void(const DTreeWrapper*)> func) const {
        for (auto& node : m_nodes) {
            if (node.isLeaf) {
                func(&node.dTree);
            }
        }
    }

    void forEachDTreeWrapperConstP(std::function<void(const DTreeWrapper*, const foundation::Vector3f&, const foundation::Vector3f&)> func) const {
        m_nodes[0].forEachLeaf(func, m_aabb.min, m_aabb.max - m_aabb.min, m_nodes);
    }

    void forEachDTreeWrapperParallel(std::function<void(DTreeWrapper*)> func) {
        int nDTreeWrappers = static_cast<int>(m_nodes.size());

        for (int i = 0; i < nDTreeWrappers; ++i) {
            if (m_nodes[i].isLeaf) {
                func(&m_nodes[i].dTree);
            }
        }
    }

    void record(const foundation::Vector3f& p, const foundation::Vector3f& dTreeVoxelSize, DTreeRecord rec, EDirectionalFilter directionalFilter, EBsdfSamplingFractionLoss bsdfSamplingFractionLoss) {
        float volume = 1;
        for (int i = 0; i < 3; ++i) {
            volume *= dTreeVoxelSize[i];
        }

        rec.statisticalWeight /= volume;
        m_nodes[0].record(p - dTreeVoxelSize * 0.5f, p + dTreeVoxelSize * 0.5f, m_aabb.min, m_aabb.extent(), rec, directionalFilter, bsdfSamplingFractionLoss, m_nodes);
    }

    // void dump(BlobWriter& blob) const {
    //     forEachDTreeWrapperConstP([&blob](const DTreeWrapper* dTree, const foundation::Vector3f& p, const foundation::Vector3f& size) {
    //         if (dTree->statisticalWeight() > 0) {
    //             dTree->dump(blob, p, size);
    //         }
    //     });
    // }

    bool shallSplit(const STreeNode& node, int depth, size_t samplesRequired) {
        return m_nodes.size() < std::numeric_limits<uint32_t>::max() - 1 && node.dTree.statisticalWeightBuilding() > samplesRequired;
    }

    void refine(size_t sTreeThreshold, int maxMB) {
        if (maxMB >= 0) {
            size_t approxMemoryFootprint = 0;
            for (const auto& node : m_nodes) {
                approxMemoryFootprint += node.dTreeWrapper()->approxMemoryFootprint();
            }

            if (approxMemoryFootprint / 1000000 >= (size_t)maxMB) {
                return;
            }
        }
        
        struct StackNode {
            size_t index;
            int depth;
        };

        std::stack<StackNode> nodeIndices;
        nodeIndices.push({0,  1});
        while (!nodeIndices.empty()) {
            StackNode sNode = nodeIndices.top();
            nodeIndices.pop();

            // Subdivide if needed and leaf
            if (m_nodes[sNode.index].isLeaf) {
                if (shallSplit(m_nodes[sNode.index], sNode.depth, sTreeThreshold)) {
                    subdivide((int)sNode.index, m_nodes);
                }
            }

            // Add children to stack if we're not
            if (!m_nodes[sNode.index].isLeaf) {
                const STreeNode& node = m_nodes[sNode.index];
                for (int i = 0; i < 2; ++i) {
                    nodeIndices.push({node.children[i], sNode.depth + 1});
                }
            }
        }

        // Uncomment once memory becomes an issue.
        //m_nodes.shrink_to_fit();
    }

    const foundation::AABB3f& aabb() const {
        return m_aabb;
    }

    void resetSDTree(size_t iter, size_t spp_per_pass) {
        RENDERER_LOG_INFO("Resetting distributions for sampling.");

        refine((size_t)(std::sqrt(std::pow(2, iter) * spp_per_pass / 4) * sTreeThreshold), sdTreeMaxMemory);
        forEachDTreeWrapperParallel([this](DTreeWrapper* dTree) { dTree->reset(20, dTreeThreshold); });
    }

    void buildSDTree() {
        RENDERER_LOG_INFO("Building distributions for sampling.");

        // Build distributions
        forEachDTreeWrapperParallel([](DTreeWrapper* dTree) { dTree->build(); });

        // Gather statistics
        int maxDepth = 0;
        int minDepth = std::numeric_limits<int>::max();
        float avgDepth = 0;
        float maxAvgRadiance = 0;
        float minAvgRadiance = std::numeric_limits<float>::max();
        float avgAvgRadiance = 0;
        size_t maxNodes = 0;
        size_t minNodes = std::numeric_limits<size_t>::max();
        float avgNodes = 0;
        float maxStatisticalWeight = 0;
        float minStatisticalWeight = std::numeric_limits<float>::max();
        float avgStatisticalWeight = 0;

        int nPoints = 0;
        int nPointsNodes = 0;

        forEachDTreeWrapperConst([&](const DTreeWrapper* dTree) {
            const int depth = dTree->depth();
            maxDepth = std::max(maxDepth, depth);
            minDepth = std::min(minDepth, depth);
            avgDepth += depth;

            const float avgRadiance = dTree->meanRadiance();
            maxAvgRadiance = std::max(maxAvgRadiance, avgRadiance);
            minAvgRadiance = std::min(minAvgRadiance, avgRadiance);
            avgAvgRadiance += avgRadiance;

            if (dTree->numNodes() > 1) {
                const size_t nodes = dTree->numNodes();
                maxNodes = std::max(maxNodes, nodes);
                minNodes = std::min(minNodes, nodes);
                avgNodes += nodes;
                ++nPointsNodes;
            }

            const float statisticalWeight = dTree->statisticalWeight();
            maxStatisticalWeight = std::max(maxStatisticalWeight, statisticalWeight);
            minStatisticalWeight = std::min(minStatisticalWeight, statisticalWeight);
            avgStatisticalWeight += statisticalWeight;

            ++nPoints;
        });

        if (nPoints > 0) {
            avgDepth /= nPoints;
            avgAvgRadiance /= nPoints;

            if (nPointsNodes > 0) {
                avgNodes /= nPointsNodes;
            }

            avgStatisticalWeight /= nPoints;
        }

        RENDERER_LOG_INFO(
            "Distribution statistics:\n"
            "  Depth         = [%s, %s, %s]\n"
            "  Mean radiance = [%s, %s, %s]\n"
            "  Node count    = [%s, %s, %s]\n"
            "  Stat. weight  = [%s, %s, %s]\n",
            foundation::pretty_uint(minDepth).c_str(), foundation::pretty_scalar(avgDepth, 5).c_str(), foundation::pretty_uint(maxDepth).c_str(),
            foundation::pretty_scalar(minAvgRadiance, 5).c_str(), foundation::pretty_scalar(avgAvgRadiance, 5).c_str(), foundation::pretty_scalar(maxAvgRadiance, 5).c_str(),
            foundation::pretty_uint(minNodes).c_str(), foundation::pretty_scalar(avgNodes, 5).c_str(), foundation::pretty_uint(maxNodes).c_str(),
            foundation::pretty_scalar(minStatisticalWeight, 5).c_str(), foundation::pretty_scalar(avgStatisticalWeight, 5).c_str(), foundation::pretty_scalar(maxStatisticalWeight, 5).c_str()
        );

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
    std::vector<STreeNode> m_nodes;
    foundation::AABB3f m_aabb;
    bool m_is_built;
    bool m_is_final_iteration;
};







struct GPTVertex
{
    DTreeWrapper *dTree;
    foundation::Vector3f dTreeVoxelSize;
    foundation::Vector3f point;
    foundation::Vector3f dir;

    renderer::Spectrum throughput;
    renderer::Spectrum bsdfVal;
    renderer::Spectrum radiance;

    float woPdf, bsdfPdf, dTreePdf;
    bool isDelta;

    void add_radiance(const renderer::Spectrum& r);
    
    void record_to_tree(STree&                    sd_tree,
                        float                     statistical_weight,
                        ESpatialFilter            spatial_filter,
                        EDirectionalFilter        directional_filter,
                        EBsdfSamplingFractionLoss bsdf_sampling_fraction_loss,
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
                        ESpatialFilter            spatial_filter,
                        EDirectionalFilter        directional_filter,
                        EBsdfSamplingFractionLoss bsdf_sampling_fraction_loss,
                        SamplingContext&          sampling_context);

    bool is_full() const;

  private:
    std::array<GPTVertex, 32> path;
    int index;
};

}