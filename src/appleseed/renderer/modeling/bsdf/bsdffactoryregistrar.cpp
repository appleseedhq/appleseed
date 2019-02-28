
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "bsdffactoryregistrar.h"

// appleseed.renderer headers.
#include "renderer/modeling/bsdf/ashikhminbrdf.h"
#include "renderer/modeling/bsdf/blinnbrdf.h"
#include "renderer/modeling/bsdf/bsdfblend.h"
#include "renderer/modeling/bsdf/bsdfmix.h"
#include "renderer/modeling/bsdf/bsdftraits.h"
#include "renderer/modeling/bsdf/diffusebtdf.h"
#include "renderer/modeling/bsdf/disneybrdf.h"
#include "renderer/modeling/bsdf/glassbsdf.h"
#include "renderer/modeling/bsdf/glossybrdf.h"
#include "renderer/modeling/bsdf/kelemenbrdf.h"
#include "renderer/modeling/bsdf/lambertianbrdf.h"
#include "renderer/modeling/bsdf/metalbrdf.h"
#include "renderer/modeling/bsdf/orennayarbrdf.h"
#include "renderer/modeling/bsdf/plasticbrdf.h"
#include "renderer/modeling/bsdf/sheenbrdf.h"
#include "renderer/modeling/bsdf/specularbrdf.h"
#include "renderer/modeling/bsdf/specularbtdf.h"
#include "renderer/modeling/entity/entityfactoryregistrar.h"

// appleseed.foundation headers.
#include "foundation/utility/autoreleaseptr.h"

using namespace foundation;

namespace renderer
{

APPLESEED_DEFINE_APIARRAY(BSDFFactoryArray);

struct BSDFFactoryRegistrar::Impl
  : public EntityFactoryRegistrarImpl<
        BSDFFactoryRegistrar::EntityType,
        BSDFFactoryRegistrar::FactoryType,
        BSDFFactoryRegistrar::FactoryArrayType
    >
{
};

BSDFFactoryRegistrar::BSDFFactoryRegistrar(const SearchPaths& search_paths)
  : impl(new Impl())
{
    // Register built-in factories.
    impl->register_factory(auto_release_ptr<FactoryType>(new AshikhminBRDFFactory()));
    impl->register_factory(auto_release_ptr<FactoryType>(new BlinnBRDFFactory()));
    impl->register_factory(auto_release_ptr<FactoryType>(new BSDFBlendFactory()));
    impl->register_factory(auto_release_ptr<FactoryType>(new BSDFMixFactory()));
    impl->register_factory(auto_release_ptr<FactoryType>(new DiffuseBTDFFactory()));
    impl->register_factory(auto_release_ptr<FactoryType>(new DisneyBRDFFactory()));
    impl->register_factory(auto_release_ptr<FactoryType>(new GlassBSDFFactory()));
    impl->register_factory(auto_release_ptr<FactoryType>(new GlossyBRDFFactory()));
    impl->register_factory(auto_release_ptr<FactoryType>(new KelemenBRDFFactory()));
    impl->register_factory(auto_release_ptr<FactoryType>(new LambertianBRDFFactory()));
    impl->register_factory(auto_release_ptr<FactoryType>(new MetalBRDFFactory()));
    impl->register_factory(auto_release_ptr<FactoryType>(new OrenNayarBRDFFactory()));
    impl->register_factory(auto_release_ptr<FactoryType>(new PlasticBRDFFactory()));
    impl->register_factory(auto_release_ptr<FactoryType>(new SheenBRDFFactory()));
    impl->register_factory(auto_release_ptr<FactoryType>(new SpecularBRDFFactory()));
    impl->register_factory(auto_release_ptr<FactoryType>(new SpecularBTDFFactory()));
}

BSDFFactoryRegistrar::~BSDFFactoryRegistrar()
{
    delete impl;
}

void BSDFFactoryRegistrar::register_factory_plugin(Plugin* plugin, void* plugin_entry_point)
{
    impl->register_factory_plugin(plugin, plugin_entry_point);
}

BSDFFactoryArray BSDFFactoryRegistrar::get_factories() const
{
    return impl->get_factories();
}

const BSDFFactoryRegistrar::FactoryType* BSDFFactoryRegistrar::lookup(const char* name) const
{
    return impl->lookup(name);
}

}   // namespace renderer
