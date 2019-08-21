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

// appleseed.foundation headers.
#include "foundation/math/scalar.h"

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
    assert(m_bsdf_sampling_fraction >= 0.0f && m_bsdf_sampling_fraction <= 1.0f);
}

bool PathGuidedSampler::sample(
    SamplingContext&                sampling_context,
    const Dual3d&                   outgoing,
    Dual3f&                         incoming,
    DirectShadingComponents&        value,
    float&                          pdf) const
{
    BSDFSample bsdf_sample(&m_shading_point, Dual3f(outgoing));
    float d_tree_pdf;

    sample(
        sampling_context,
        bsdf_sample,
        pdf,
        d_tree_pdf);

    // Filter scattering modes.
    if (!(m_bsdf_sampling_modes & bsdf_sample.get_mode()))
        return false;

    incoming = bsdf_sample.m_incoming;
    value = bsdf_sample.m_value;

    return true;
}

float PathGuidedSampler::evaluate(
    const Vector3f&                 outgoing,
    const Vector3f&                 incoming,
    const int                       light_sampling_modes,
    DirectShadingComponents&        value) const
{
    const float bsdf_pdf = m_bsdf.evaluate(
        m_bsdf_data,
        false, // not adjoint
        true,
        foundation::Vector3f(m_geometric_normal),
        foundation::Basis3f(m_shading_basis),
        outgoing,
        incoming,
        m_bsdf_sampling_modes,
        value);

    float d_tree_pdf;
    return guided_path_extension_pdf(incoming, bsdf_pdf, d_tree_pdf, false);
}

bool PathGuidedSampler::sample(
    SamplingContext&                sampling_context,
    BSDFSample&                     bsdf_sample,
    float&                          wi_pdf,
    float&                          d_tree_pdf) const
{
    if (!m_sd_tree_is_built || m_bsdf.is_purely_specular())
    {
        m_bsdf.sample(
            sampling_context,
            m_bsdf_data,
            false,
            true, // multiply by |cos(incoming, normal)|
            m_bsdf_sampling_modes,
            bsdf_sample);
            
        wi_pdf = guided_path_extension_pdf(bsdf_sample.m_incoming.get_value(), bsdf_sample.get_probability(), d_tree_pdf, true);
        return false;
    }

    sampling_context.split_in_place(1, 1);
    const float s = sampling_context.next2<float>();
    
    if (s < m_bsdf_sampling_fraction)
    {
        m_bsdf.sample(
            sampling_context,
            m_bsdf_data,
            false,
            true, // multiply by |cos(incoming, normal)|
            m_bsdf_sampling_modes,
            bsdf_sample);

        if(bsdf_sample.get_mode() == ScatteringMode::None)
            return false;

        if (bsdf_sample.get_mode() == ScatteringMode::Specular)
        {
            d_tree_pdf = 0.0f;
            wi_pdf = m_bsdf_sampling_fraction;
            return false;
        }

        wi_pdf = guided_path_extension_pdf(bsdf_sample.m_incoming.get_value(), bsdf_sample.get_probability(), d_tree_pdf, false);
        
        return false;
    }
    else
    {
        DTreeSample d_tree_sample;
        m_d_tree->sample(sampling_context, d_tree_sample, m_bsdf_sampling_modes);

        if(d_tree_sample.scattering_mode == ScatteringMode::None)
        {
            // Terminate.
            bsdf_sample.set_to_scattering(d_tree_sample.scattering_mode, 0.0f);
            return true;
        }

        bsdf_sample.m_incoming = foundation::Dual3f(d_tree_sample.direction);
        d_tree_pdf = d_tree_sample.pdf;

        const float bsdf_pdf = m_bsdf.evaluate(
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
            ScatteringMode::has_diffuse(m_bsdf.get_modes() & m_bsdf_sampling_modes) ? ScatteringMode::Diffuse : ScatteringMode::Glossy, bsdf_pdf);

        wi_pdf = guided_path_extension_pdf(bsdf_sample.m_incoming.get_value(), bsdf_pdf, d_tree_pdf, true);

        return true;
    }
}

float PathGuidedSampler::guided_path_extension_pdf(
    const foundation::Vector3f&     incoming,
    const float&                    bsdf_pdf,
    float&                          d_tree_pdf,
    const bool                      d_tree_pdf_is_set) const
{
    if(!m_sd_tree_is_built || m_bsdf.is_purely_specular())
    {
        d_tree_pdf = 0.0f;
        return bsdf_pdf;
    }

    if(!d_tree_pdf_is_set)
        d_tree_pdf = m_d_tree->pdf(incoming);
        
    return lerp(d_tree_pdf, bsdf_pdf, m_bsdf_sampling_fraction);
}

}    //namespace renderer