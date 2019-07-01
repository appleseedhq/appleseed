// Interface header.
#include "sdtree.h"

// appleseed.foundation headers.
#include "foundation/math/scalar.h"
#include "foundation/math/sampling/mappings.h"

namespace renderer
{
void DTreeWrapper::record(
    const DTreeRecord&                          rec,
    EDirectionalFilter                          directionalFilter)
{
}

float DTreeWrapper::pdf(
    const foundation::Vector3f&                 dir) const
{
    return foundation::RcpFourPi<float>();
}

void DTreeWrapper::sample(
    SamplingContext&                            sampling_context,
    DTreeSample&                                sample)
{
    sampling_context.split_in_place(2, 1);
    const foundation::Vector2f s = sampling_context.next2<foundation::Vector2f>();
    sample.m_dir = foundation::sample_sphere_uniform(s);
    sample.m_probability = pdf(sample.m_dir);
}

DTreeWrapper* STree::get_d_tree_wrapper(
    const foundation::Vector3d&                 point)
{
    return &m_d_tree_wrapper;
}

void GPTVertex::add_radiance(
    const renderer::Spectrum&                   radiance)
{
}

void GPTVertex::record_to_tree(
    STree&                                      sd_tree,
    float                                       statistical_weight,
    ESpatialFilter                              spatial_filter,
    EDirectionalFilter                          directional_filter,
    SamplingContext&                            sampling_context)
{
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
    const renderer::Spectrum&                   radiance)
{
    for(auto v : path)
        v.add_radiance(radiance);
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
    SamplingContext&                            sampling_context)
{
    for(auto v : path)
        v.record_to_tree(sd_tree,
                         statistical_weight,
                         spatial_filter,
                         directional_filter,
                         sampling_context);
}

}   // namespace renderer
