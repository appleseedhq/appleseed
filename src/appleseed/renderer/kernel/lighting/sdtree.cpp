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

void GPTVertex::record_to_tree(
    STree&                                      sd_tree,
    float                                       statistical_weight,
    SamplingContext&                            sampling_context)
{
    if (!(wo_pdf > 0) || !isValidSpectrum(radiance) || !isValidSpectrum(bsdf_val))
    {
        return;
    }

    Spectrum incoming_radiance(0.0f);

    for(size_t i = 0; i < incoming_radiance.size(); ++i)
    {
        if(throughput[i] * wo_pdf > SDTreeEpsilon)
            incoming_radiance[i] = radiance[i] / throughput[i];
    }

    Spectrum product = incoming_radiance * bsdf_val;

    DTreeRecord dtree_record{dir,
                             foundation::average_value(incoming_radiance),
                             wo_pdf,
                             bsdf_pdf,
                             dtree_pdf,
                             statistical_weight,
                             foundation::average_value(product),
                             is_delta};

    sd_tree.record(dtree, point, dtree_node_size, dtree_record, sampling_context);
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
    SamplingContext&                            sampling_context)
{
    for(int i = 0; i < index; ++i)
        path[i].record_to_tree(sd_tree,
                            statistical_weight,
                            sampling_context);
}

}   // namespace renderer
