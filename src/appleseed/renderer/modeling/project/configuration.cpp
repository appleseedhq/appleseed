
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//

// Interface header.
#include "configuration.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/pt/ptlightingengine.h"
#include "renderer/kernel/lighting/sppm/sppmlightingengine.h"
#include "renderer/kernel/rendering/final/adaptivepixelrenderer.h"
#include "renderer/kernel/rendering/final/uniformpixelrenderer.h"
#include "renderer/kernel/rendering/generic/genericframerenderer.h"
#include "renderer/kernel/rendering/progressive/progressiveframerenderer.h"
#include "renderer/kernel/texturing/texturestore.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/utility/containers/dictionary.h"

// Standard headers.
#include <cassert>
#include <cstring>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// Configuration class implementation.
//

namespace
{
    const UniqueID g_class_uid = new_guid();
}

UniqueID Configuration::get_class_uid()
{
    return g_class_uid;
}

Configuration::Configuration(const char* name)
  : Entity(g_class_uid)
  , m_base(0)
{
    set_name(name);
}

void Configuration::release()
{
    delete this;
}

void Configuration::set_base(const Configuration* base)
{
    m_base = base;
}

const Configuration* Configuration::get_base() const
{
    return m_base;
}

ParamArray Configuration::get_inherited_parameters() const
{
    if (m_base)
    {
        ParamArray params = m_base->m_params;
        params.merge(m_params);
        return params;
    }
    else
    {
        return m_params;
    }
}

Dictionary Configuration::get_metadata()
{
    Dictionary metadata;

    metadata.insert(
        "sampling_mode",
        Dictionary()
            .insert("type", "enum")
            .insert("values", "rng|qmc")
            .insert("default", "rng")
            .insert("label", "Sampler")
            .insert("help", "Sampler to use when generating samples")
            .insert(
                "options",
                Dictionary()
                    .insert(
                        "rng",
                        Dictionary()
                            .insert("label", "RNG")
                            .insert("help", "Random sampler"))
                    .insert(
                        "qmc",
                        Dictionary()
                            .insert("label", "QMC")
                            .insert("help", "Quasi Monte Carlo sampler"))));

    metadata.insert(
        "lighting_engine",
        Dictionary()
            .insert("type", "enum")
            .insert("values", "pt|sppm")
            .insert("default", "pt")
            .insert("label", "Lighting Engine")
            .insert("help", "Lighting engine used when rendering")
            .insert(
                "options",
                Dictionary()
                    .insert(
                        "pt",
                        Dictionary()
                            .insert("label", "Unidirectional Path Tracer")
                            .insert("help", "Unidirectional path tracing"))
                    .insert(
                        "sppm",
                        Dictionary()
                            .insert("label", "Progressive Photon Mapping")
                            .insert("help", "Stochastic progressive photon mapping"))));

    metadata.insert(
        "rendering_threads",
        Dictionary()
            .insert("type", "int")
            .insert("label", "Render Threads")
            .insert("help", "Number of threads to use for rendering"));

    metadata.dictionaries().insert(
        "texture_store",
        TextureStore::get_params_metadata());

    metadata.dictionaries().insert(
        "uniform_pixel_renderer",
        UniformPixelRendererFactory::get_params_metadata());

    metadata.dictionaries().insert(
        "adaptive_pixel_renderer",
        AdaptivePixelRendererFactory::get_params_metadata());

    metadata.dictionaries().insert(
        "generic_frame_renderer",
        GenericFrameRendererFactory::get_params_metadata());

    metadata.dictionaries().insert(
        "progressive_frame_renderer",
        ProgressiveFrameRendererFactory::get_params_metadata());

    metadata.dictionaries().insert("pt", PTLightingEngineFactory::get_params_metadata());
    metadata.dictionaries().insert("sppm", SPPMLightingEngineFactory::get_params_metadata());

    return metadata;
}


//
// ConfigurationFactory class implementation.
//

auto_release_ptr<Configuration> ConfigurationFactory::create(const char* name)
{
    assert(name);

    return auto_release_ptr<Configuration>(new Configuration(name));
}

auto_release_ptr<Configuration> ConfigurationFactory::create(
    const char*         name,
    const ParamArray&   params)
{
    assert(name);

    auto_release_ptr<Configuration> configuration(new Configuration(name));

    configuration->get_parameters().merge(params);

    return configuration;
}


//
// BaseConfigurationFactory class implementation.
//

auto_release_ptr<Configuration> BaseConfigurationFactory::create_base_final()
{
    auto_release_ptr<Configuration> configuration(new Configuration("base_final"));

    ParamArray& parameters = configuration->get_parameters();

    parameters.insert("sampling_mode", "rng");

    parameters.insert("frame_renderer", "generic");
    parameters.insert("tile_renderer", "generic");

    parameters.insert("pixel_renderer", "uniform");
    parameters.dictionaries().insert(
        "uniform_pixel_renderer",
        ParamArray()
            .insert("samples", "64"));

    parameters.insert("sample_renderer", "generic");
    parameters.insert("lighting_engine", "pt");

    return configuration;
}

auto_release_ptr<Configuration> BaseConfigurationFactory::create_base_interactive()
{
    auto_release_ptr<Configuration> configuration(new Configuration("base_interactive"));

    ParamArray& parameters = configuration->get_parameters();

    parameters.insert("sampling_mode", "rng");

    parameters.insert("frame_renderer", "progressive");
    parameters.insert("sample_generator", "generic");
    parameters.insert("sample_renderer", "generic");
    parameters.insert("lighting_engine", "pt");

    return configuration;
}

bool BaseConfigurationFactory::is_base_final_configuration(const char* name)
{
    assert(name);
    return strcmp(name, "base_final") == 0;
}

bool BaseConfigurationFactory::is_base_interactive_configuration(const char* name)
{
    assert(name);
    return strcmp(name, "base_interactive") == 0;
}

bool BaseConfigurationFactory::is_base_configuration(const char* name)
{
    return is_base_final_configuration(name) || is_base_interactive_configuration(name);
}

}   // namespace renderer
