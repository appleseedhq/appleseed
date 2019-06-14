#pragma once

// appleseed.renderer headers.
#include "renderer/kernel/lighting/materialsamplers.h"
#include "renderer/kernel/lighting/sdtree.h"
#include "renderer/modeling/bsdf/bsdfsample.h"


namespace renderer
{

class PathGuidedSampler
  : public BSDFSampler
{
  public:
    PathGuidedSampler(
        STree*                          sd_tree,
        const float                     bsdf_sampling_fraction,
        const BSDF&                     bsdf,
        const void*                     bsdf_data,
        const int                       bsdf_sampling_modes,
        const ShadingPoint&             shading_point);

    bool sample(
        SamplingContext&                sampling_context,
        const foundation::Dual3d&       outgoing,
        foundation::Dual3f&             incoming,
        DirectShadingComponents&        value,
        float&                          pdf) const override;

    bool sample(
        SamplingContext&                sampling_context,
        BSDFSample&                     bsdf_sample,
        float&                          wo_pdf,
        float&                          bsdf_pdf,
        float&                          d_tree_pdf) const;

    float evaluate(
        const foundation::Vector3f&     outgoing,
        const foundation::Vector3f&     incoming,
        const int                       light_sampling_modes,
        DirectShadingComponents&        value) const override;

  private:
    bool guide_path_extension(
        SamplingContext&                sampling_context,
        BSDFSample&                     bsdf_sample,
        float&                          wo_pdf,
        float&                          bsdf_pdf,
        float&                          d_tree_pdf) const;

    void guided_path_extension_pdf(
        const BSDFSample&               bsdf_sample,
        float&                          wo_pdf,
        float&                          bsdf_pdf,
        float&                          d_tree_pdf) const;

    void guided_bsdf_evaluation(
        const BSDFSample&               bsdf_sample,
        float&                          wo_pdf,
        float&                          bsdf_pdf,
        float&                          d_tree_pdf,
        DirectShadingComponents&        value) const;

    STree*                              m_sd_tree;
    DTreeWrapper*                       m_d_tree;
    const float                         m_bsdf_sampling_fraction;
};

}   // namespace render