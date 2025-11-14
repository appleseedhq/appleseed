#pragma once

// appleseed.renderer headers.
#include "renderer/kernel/lighting/materialsamplers.h"
#include "renderer/kernel/lighting/sdtree.h"
#include "renderer/modeling/bsdf/bsdfsample.h"


namespace renderer
{

//
// Sampler acting as a wrapper for path guided sampling at shading points for the implementation of
// "Practical Path Guiding for Efficient Light-Transport Simulation" [Müller et al. 2017].
//

class PathGuidedSampler
  : public BSDFSampler
{
  public:
    PathGuidedSampler(
        const bool                      enable_path_guiding,
        const GuidedBounceMode          guided_bounce_mode,
        DTree*                          d_tree,
        const float                     bsdf_sampling_fraction,
        const BSDF&                     bsdf,
        const void*                     bsdf_data,
        const int                       bsdf_sampling_modes,
        const ShadingPoint&             shading_point,
        const bool                      sd_tree_is_built);

    bool sample(
        SamplingContext&                sampling_context,
        const foundation::Dual3d&       outgoing,
        foundation::Dual3f&             incoming,
        DirectShadingComponents&        value,
        float&                          pdf) const override;

    bool sample(
        SamplingContext&                sampling_context,
        BSDFSample&                     bsdf_sample,
        const foundation::Dual3d&       outgoing,
        float&                          wi_pdf,
        float&                          d_tree_pdf) const;

    float evaluate(
        const foundation::Vector3f&     outgoing,
        const foundation::Vector3f&     incoming,
        const int                       light_sampling_modes,
        DirectShadingComponents&        value) const override;

  private:
    float guided_path_extension_pdf(
        const foundation::Vector3f&     incoming,
        const float&                    bsdf_pdf,
        float&                          d_tree_pdf,
        const bool                      d_tree_pdf_is_set) const;

    const int enable_modes_before_sampling(
        const int                       sample_modes) const;

    ScatteringMode::Mode set_mode_after_sampling(
        const ScatteringMode::Mode      sampled_mode) const;

    DTree*                              m_d_tree;
    const float                         m_bsdf_sampling_fraction;
    const bool                          m_sd_tree_is_built;
    const bool                          m_enable_path_guiding;
    const GuidedBounceMode              m_guided_bounce_mode;
};

}   // namespace render