// Interface header.
#include "sdtree.h"

// appleseed.foundation headers.
#include "foundation/math/scalar.h"
#include "foundation/math/sampling/mappings.h"

namespace renderer
{

void DTreeWrapper::record(const DTreeRecord &rec, EDirectionalFilter directionalFilter)
{

}

float DTreeWrapper::pdf(const foundation::Vector3f &dir) const
{
    return foundation::RcpFourPi<float>();
}

void DTreeWrapper::sample(
    SamplingContext &sampling_context,
    DTreeSample &sample)
{
    sampling_context.split_in_place(2, 1);
    const foundation::Vector2f s = sampling_context.next2<foundation::Vector2f>();
    sample.m_dir = foundation::sample_sphere_uniform(s);
    sample.m_probability = pdf(sample.m_dir);
}

DTreeWrapper* STree::get_dTreeWrapper()
{
    return &m_dTreeWrapper;
}

}   // namespace renderer
