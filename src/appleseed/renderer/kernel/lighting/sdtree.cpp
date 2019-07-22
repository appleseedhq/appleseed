// Interface header.
#include "sdtree.h"

// appleseed.foundation headers.
#include "foundation/math/scalar.h"
#include "foundation/math/sampling/mappings.h"

namespace renderer
{
// void DTreeWrapper::record(
//     const DTreeRecord&                          rec,
//     EDirectionalFilter                          directionalFilter)
// {
// }

// float DTreeWrapper::pdf(
//     const foundation::Vector3f&                 dir) const
// {
//     return foundation::RcpFourPi<float>();
// }

// void DTreeWrapper::sample(
//     SamplingContext&                            sampling_context,
//     DTreeSample&                                sample)
// {
//     sampling_context.split_in_place(2, 1);
//     const foundation::Vector2f s = sampling_context.next2<foundation::Vector2f>();
//     sample.m_dir = foundation::sample_sphere_uniform(s);
//     sample.m_probability = pdf(sample.m_dir);
// }

// DTreeWrapper* STree::get_d_tree_wrapper(
//     const foundation::Vector3d&                 point)
// {
//     return &m_d_tree_wrapper;
// }

void GPTVertex::add_radiance(
    const renderer::Spectrum&                   r)
{
    radiance += r;
}

bool isValidSpectrum(const Spectrum& s)
{
    for (int i = 0; i < s.size(); i++)
        if (!std::isfinite(s[i]) || s[i] < 0.0f)
            return false;
    return true;
}

/// Clip point to lie within bounding box.
foundation::Vector3f clip_vector(const foundation::AABB3f& aabb, const foundation::Vector3f &p)
{
    foundation::Vector3f result = p;
    for (int i = 0; i < foundation::Vector3f::Dimension; ++i)
    {
        result[i] = std::min(std::max(result[i], aabb.min[i]), aabb.max[i]);
    }
    return result;
}

void GPTVertex::record_to_tree(
    STree&                                      sd_tree,
    float                                       statistical_weight,
    ESpatialFilter                              spatial_filter,
    EDirectionalFilter                          directional_filter,
    EBsdfSamplingFractionLoss                   bsdf_sampling_fraction_loss,
    SamplingContext&                            sampling_context)
{
    if (!(woPdf > 0) || !isValidSpectrum(radiance) || !isValidSpectrum(bsdfVal))
    {
        return;
    }

    Spectrum localRadiance = Spectrum{0.0f};
    if (throughput[0] * woPdf > sd_tree_epsilon) localRadiance[0] = radiance[0] / throughput[0];
    if (throughput[1] * woPdf > sd_tree_epsilon) localRadiance[1] = radiance[1] / throughput[1];
    if (throughput[2] * woPdf > sd_tree_epsilon) localRadiance[2] = radiance[2] / throughput[2];
    Spectrum product = localRadiance * bsdfVal;

    DTreeRecord rec{ dir, foundation::average_value(localRadiance), foundation::average_value(product), woPdf, bsdfPdf, dTreePdf, statistical_weight, isDelta };
    switch (spatial_filter) {
        case ESpatialFilter::ENearest:
            dTree->record(rec, directional_filter, bsdf_sampling_fraction_loss);
            break;
        case ESpatialFilter::EStochasticBox:
            {
                DTreeWrapper* splatDTree = dTree;

                // Jitter the actual position within the
                // filter box to perform stochastic filtering.
                foundation::Vector3f offset = dTreeVoxelSize;

                sampling_context.split_in_place(3, 1);

                offset *= (sampling_context.next2<foundation::Vector3f>() - foundation::Vector3f(0.5f));

                foundation::Vector3f origin = clip_vector(sd_tree.aabb(), point + offset);
                splatDTree = sd_tree.dTreeWrapper(origin);
                if (splatDTree) {
                    splatDTree->record(rec, directional_filter, bsdf_sampling_fraction_loss);
                }
                break;
            }
        case ESpatialFilter::EBox:
            // sd_tree.record(point, dTreeVoxelSize, rec, directional_filter, bsdf_sampling_fraction_loss);
            break;
    }
}

GPTVertexPath::GPTVertexPath()
  : index(0)
{
}

void GPTVertexPath::add_vertex(
    const GPTVertex&                            vertex)
{
    if(index < path.size())
        path[index++] = vertex;
}

void GPTVertexPath::add_radiance(
    const renderer::Spectrum&                   r)
{
    for(int i = 0; i < index; ++i)
        path[i].add_radiance(r);
}

bool GPTVertexPath::is_full() const
{
    return index >= path.size();
}

void GPTVertexPath::record_to_tree(
    STree&                                      sd_tree,
    float                                       statistical_weight,
    ESpatialFilter                              spatial_filter,
    EDirectionalFilter                          directional_filter,
    EBsdfSamplingFractionLoss                   bsdf_sampling_fraction_loss,
    SamplingContext&                            sampling_context)
{
    for(int i = 0; i < index; ++i)
        path[i].record_to_tree(sd_tree,
                            statistical_weight,
                            spatial_filter,
                            directional_filter,
                            bsdf_sampling_fraction_loss,
                            sampling_context);
}

}   // namespace renderer
