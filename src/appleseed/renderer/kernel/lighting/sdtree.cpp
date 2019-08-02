// Interface header.
#include "sdtree.h"

// appleseed.foundation headers.
#include "foundation/math/scalar.h"
#include "foundation/math/sampling/mappings.h"

namespace renderer
{

foundation::Vector3f cylindrical_to_cartesian(const foundation::Vector2f &cylindrical_direction)
{
    return foundation::sample_sphere_uniform(cylindrical_direction);
}

foundation::Vector2f cartesian_to_cylindrical(const foundation::Vector3f &d)
{
    // TODO: Handle float imprecision

    const float cosTheta = d.y;
    float phi = std::atan2(d.z, d.x);

    if (phi < 0.0f)
        phi = std::max(phi + foundation::TwoPi<float>(), 0.0f);

    return foundation::Vector2f(phi * foundation::RcpTwoPi<float>(), 1.0f - 0.5f * (cosTheta + 1.0f));
}

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
    SpatialFilter                              spatial_filter,
    DirectionalFilter                          directional_filter,
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

    DTreeRecord rec{ dir, foundation::average_value(localRadiance), foundation::average_value(product), woPdf, bsdfPdf, dTreePdf, statistical_weight, is_delta, directional_filter };
    switch (spatial_filter) {
        case SpatialFilter::Nearest:
            dTree->record(rec/* , directional_filter, bsdf_sampling_fraction_loss*/);
            break;
        case SpatialFilter::StochasticBox:
            {
                DTree* splatDTree = dTree;

                // Jitter the actual position within the
                // filter box to perform stochastic filtering.
                foundation::Vector3f offset = dTreeVoxelSize;

                sampling_context.split_in_place(3, 1);

                offset *= (sampling_context.next2<foundation::Vector3f>() - foundation::Vector3f(0.5f));

                foundation::Vector3f origin = clip_vector(sd_tree.aabb(), point + offset);
                splatDTree = sd_tree.get_d_tree(origin);
                if (splatDTree) {
                    splatDTree->record(rec/* , directional_filter, bsdf_sampling_fraction_loss*/);
                }
                break;
            }
        case SpatialFilter::Box:
            sd_tree.record(point, dTreeVoxelSize, rec, directional_filter);
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
    SpatialFilter                              spatial_filter,
    DirectionalFilter                          directional_filter,
    SamplingContext&                            sampling_context)
{
    for(int i = 0; i < index; ++i)
        path[i].record_to_tree(sd_tree,
                            statistical_weight,
                            spatial_filter,
                            directional_filter,
                            sampling_context);
}

}   // namespace renderer
