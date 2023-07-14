
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "aovfactoryregistrar.h"

// appleseed.renderer headers.
#include "renderer/modeling/aov/albedoaov.h"
#include "renderer/modeling/aov/aovtraits.h"
#include "renderer/modeling/aov/cryptomatteaov.h"
#include "renderer/modeling/aov/depthaov.h"
#include "renderer/modeling/aov/diffuseaov.h"
#include "renderer/modeling/aov/emissionaov.h"
#include "renderer/modeling/aov/glossyaov.h"
#include "renderer/modeling/aov/invalidsamplesaov.h"
#include "renderer/modeling/aov/normalaov.h"
#include "renderer/modeling/aov/npraovs.h"
#include "renderer/modeling/aov/pixelerroraov.h"
#include "renderer/modeling/aov/pixelsamplecountaov.h"
#include "renderer/modeling/aov/pixeltimeaov.h"
#include "renderer/modeling/aov/pixelvariationaov.h"
#include "renderer/modeling/aov/positionaov.h"
#include "renderer/modeling/aov/screenspacevelocityaov.h"
#include "renderer/modeling/aov/uvaov.h"
#include "renderer/modeling/aov/lpeaov.h"
#include "renderer/modeling/entity/entityfactoryregistrar.h"

// appleseed.foundation headers.
#include "foundation/memory/autoreleaseptr.h"

using namespace foundation;

namespace renderer
{

APPLESEED_DEFINE_APIARRAY(AOVFactoryArray);

struct AOVFactoryRegistrar::Impl
  : public EntityFactoryRegistrarImpl<
        AOVFactoryRegistrar::EntityType,
        AOVFactoryRegistrar::FactoryType,
        AOVFactoryRegistrar::FactoryArrayType
    >
{
};

AOVFactoryRegistrar::AOVFactoryRegistrar(const SearchPaths& search_paths)
  : impl(new Impl())
{
    // Register built-in factories.
    impl->register_factory(auto_release_ptr<FactoryType>(new AlbedoAOVFactory()));
    impl->register_factory(auto_release_ptr<FactoryType>(new DepthAOVFactory()));
    impl->register_factory(auto_release_ptr<FactoryType>(new DiffuseAOVFactory()));
    impl->register_factory(auto_release_ptr<FactoryType>(new DirectDiffuseAOVFactory()));
    impl->register_factory(auto_release_ptr<FactoryType>(new DirectGlossyAOVFactory()));
    impl->register_factory(auto_release_ptr<FactoryType>(new EmissionAOVFactory()));
    impl->register_factory(auto_release_ptr<FactoryType>(new GlossyAOVFactory()));
    impl->register_factory(auto_release_ptr<FactoryType>(new IndirectDiffuseAOVFactory()));
    impl->register_factory(auto_release_ptr<FactoryType>(new IndirectGlossyAOVFactory()));
    impl->register_factory(auto_release_ptr<FactoryType>(new InvalidSamplesAOVFactory()));
    impl->register_factory(auto_release_ptr<FactoryType>(new NormalAOVFactory()));
    impl->register_factory(auto_release_ptr<FactoryType>(new NPRContourAOVFactory()));
    impl->register_factory(auto_release_ptr<FactoryType>(new NPRShadingAOVFactory()));
    impl->register_factory(auto_release_ptr<FactoryType>(new PixelErrorAOVFactory()));
    impl->register_factory(auto_release_ptr<FactoryType>(new PixelSampleCountAOVFactory()));
    impl->register_factory(auto_release_ptr<FactoryType>(new PixelTimeAOVFactory()));
    impl->register_factory(auto_release_ptr<FactoryType>(new PixelVariationAOVFactory()));
    impl->register_factory(auto_release_ptr<FactoryType>(new PositionAOVFactory()));
    impl->register_factory(auto_release_ptr<FactoryType>(new ScreenSpaceVelocityAOVFactory()));
    impl->register_factory(auto_release_ptr<FactoryType>(new UVAOVFactory()));
    impl->register_factory(auto_release_ptr<FactoryType>(new CryptomatteAOVFactory(CryptomatteAOV::CryptomatteType::ObjectNames)));
    impl->register_factory(auto_release_ptr<FactoryType>(new CryptomatteAOVFactory(CryptomatteAOV::CryptomatteType::MaterialNames)));
    impl->register_factory(auto_release_ptr<FactoryType>(new LPEAOVFactory()));
}

AOVFactoryRegistrar::~AOVFactoryRegistrar()
{
    delete impl;
}

void AOVFactoryRegistrar::register_factory_plugin(Plugin* plugin, void* plugin_entry_point)
{
    impl->register_factory_plugin(plugin, plugin_entry_point);
}

AOVFactoryArray AOVFactoryRegistrar::get_factories() const
{
    return impl->get_factories();
}

const AOVFactoryRegistrar::FactoryType* AOVFactoryRegistrar::lookup(const char* name) const
{
    return impl->lookup(name);
}

}   // namespace renderer
