// Interface header.
#include "pathguidedsampler.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/tracer.h"
#include "renderer/kernel/shading/directshadingcomponents.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfsample.h"
#include "renderer/modeling/volume/volume.h"

// Standard headers.
#include <cassert>

using namespace foundation;
using namespace std;

namespace renderer
{

PathGuidedSampler::PathGuidedSampler(
    DTree*                          d_tree,
    const float                     bsdf_sampling_fraction,
    const BSDF&                     bsdf,
    const void*                     bsdf_data,
    const int                       bsdf_sampling_modes,
    const ShadingPoint&             shading_point,
    const bool                      sd_tree_is_built)
  : BSDFSampler(
      bsdf,
      bsdf_data,
      bsdf_sampling_modes,
      shading_point)
  , m_d_tree(d_tree)
  , m_bsdf_sampling_fraction(bsdf_sampling_fraction)
  , m_sd_tree_is_built(sd_tree_is_built)
{
    assert(m_d_tree);
}

bool PathGuidedSampler::sample(
    SamplingContext&                sampling_context,
    const Dual3d&                   outgoing,
    Dual3f&                         incoming,
    DirectShadingComponents&        value,
    float&                          pdf) const
{
    BSDFSample sample(&m_shading_point, Dual3f(outgoing));
    float wo_pdf, bsdf_pdf, d_tree_pdf;

    bool is_path_guided = guide_path_extension(
        sampling_context,
        sample,
        wo_pdf,
        bsdf_pdf,
        d_tree_pdf
    );

    // Filter scattering modes.
    if (!is_path_guided && !(m_bsdf_sampling_modes & sample.get_mode()))
        return false;

    incoming = sample.m_incoming;
    value = sample.m_value;
    pdf = wo_pdf;

    return true;
}

bool PathGuidedSampler::sample(
    SamplingContext&                sampling_context,
    BSDFSample&                     bsdf_sample,
    float&                          wo_pdf,
    float&                          bsdf_pdf,
    float&                          d_tree_pdf) const
{
    bool is_path_guided = guide_path_extension(
        sampling_context,
        bsdf_sample,
        wo_pdf,
        bsdf_pdf,
        d_tree_pdf
    );

    return is_path_guided;
}

float PathGuidedSampler::evaluate(
    const Vector3f&                 outgoing,
    const Vector3f&                 incoming,
    const int                       light_sampling_modes,
    DirectShadingComponents&        value) const
{
    BSDFSample sample(&m_shading_point, Dual3f(outgoing));
    sample.m_incoming = foundation::Dual3f(incoming);
    float wo_pdf, bsdf_pdf, d_tree_pdf;

    guided_bsdf_evaluation(
        sample,
        wo_pdf,
        bsdf_pdf,
        d_tree_pdf,
        value
    );

    return wo_pdf;
}

// This was taken from the Mitsuba implementation of "Practical Path Guiding for
// Efficient Light-Transport Simulation" [Müller et al. 2017] and adapted to work
// with Appleseed.

bool PathGuidedSampler::guide_path_extension(
    SamplingContext&                sampling_context,
    BSDFSample&                     bsdf_sample,
    float&                          wo_pdf,
    float&                          bsdf_pdf,
    float&                          d_tree_pdf) const
{
    sampling_context.split_in_place(2, 1);
    foundation::Vector2f sample = sampling_context.next2<foundation::Vector2f>();
    bool is_path_guided = false;

    if (!m_sd_tree_is_built || m_bsdf.is_purely_specular())
    {
        m_bsdf.sample(
            sampling_context,
            m_bsdf_data,
            false,
            true, // multiply by |cos(incoming, normal)|
            m_bsdf_sampling_modes,
            bsdf_sample);
        wo_pdf = bsdf_sample.get_probability();
        d_tree_pdf = 0;
        return is_path_guided;
    }

    if (sample.x < m_bsdf_sampling_fraction)
    {
        sample.x /= m_bsdf_sampling_fraction;
        m_bsdf.sample(
            sampling_context,
            m_bsdf_data,
            false,
            true, // multiply by |cos(incoming, normal)|
            m_bsdf_sampling_modes,
            bsdf_sample);

        if(bsdf_sample.get_mode() == ScatteringMode::None)
        {
            wo_pdf = bsdf_pdf = d_tree_pdf = 0;
            return is_path_guided;
        }
        
        bsdf_pdf = bsdf_sample.get_probability();

        // If we sampled a delta component, then we have a 0 probability
        // of sampling that direction via guiding, thus we can return early.
        if (bsdf_sample.get_mode() == ScatteringMode::Specular)
        {
            d_tree_pdf = 0;
            wo_pdf = m_bsdf_sampling_fraction;
            return is_path_guided;
        }

        guided_path_extension_pdf(bsdf_sample, wo_pdf, bsdf_pdf, d_tree_pdf, false);
    }
    else
    {
        is_path_guided = true;
        sample.x = (sample.x - m_bsdf_sampling_fraction) / (1 - m_bsdf_sampling_fraction);
        DTreeSample d_tree_sample;
        m_d_tree->sample(sampling_context, d_tree_sample);
        bsdf_sample.m_incoming = foundation::Dual3f(d_tree_sample.direction);
        d_tree_pdf = d_tree_sample.pdf;

        bsdf_pdf = m_bsdf.evaluate(
            m_bsdf_data,
            false, // not adjoint
            true,  // multiply by |cos(incoming, normal)|
            foundation::Vector3f(m_geometric_normal),
            foundation::Basis3f(m_shading_basis),
            foundation::Vector3f(bsdf_sample.m_outgoing.get_value()),
            bsdf_sample.m_incoming.get_value(),
            m_bsdf_sampling_modes,
            bsdf_sample.m_value);
        bsdf_sample.set_to_scattering(
            ScatteringMode::has_diffuse(m_bsdf.get_modes()) ? ScatteringMode::Diffuse : ScatteringMode::Glossy,
            bsdf_pdf);
        guided_path_extension_pdf(bsdf_sample, wo_pdf, bsdf_pdf, d_tree_pdf, true);
    }

    return is_path_guided;
}

void PathGuidedSampler::guided_path_extension_pdf(
    const BSDFSample&               bsdf_sample,
    float&                          wo_pdf,
    float&                          bsdf_pdf,
    float&                          d_tree_pdf,
    const bool                      d_tree_pdf_is_set) const
{
    if(!d_tree_pdf_is_set)
        d_tree_pdf = m_d_tree->pdf(bsdf_sample.m_incoming.get_value());
        
    wo_pdf = m_bsdf_sampling_fraction * bsdf_pdf + (1 - m_bsdf_sampling_fraction) * d_tree_pdf;
}

void PathGuidedSampler::guided_bsdf_evaluation(
const BSDFSample&                   bsdf_sample,
float&                              wo_pdf,
float&                              bsdf_pdf,
float&                              d_tree_pdf,
DirectShadingComponents&            value) const
{
    d_tree_pdf = 0;
    bsdf_pdf = m_bsdf.evaluate(
        m_bsdf_data,
        false, // not adjoint
        true,
        foundation::Vector3f(m_geometric_normal),
        foundation::Basis3f(m_shading_basis),
        foundation::Vector3f(bsdf_sample.m_outgoing.get_value()),
        bsdf_sample.m_incoming.get_value(),
        m_bsdf_sampling_modes,
        value);

    if (!m_sd_tree_is_built || m_bsdf.is_purely_specular())
    {
        wo_pdf = bsdf_pdf;
        return;
    }

    d_tree_pdf = m_d_tree->pdf(bsdf_sample.m_incoming.get_value());
    wo_pdf = m_bsdf_sampling_fraction * bsdf_pdf + (1 - m_bsdf_sampling_fraction) * d_tree_pdf;
}

}    //namespace renderer