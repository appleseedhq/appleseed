
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
#include "renderer/modeling/entity/registerentityfactories.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"
#include "foundation/utility/registrar.h"

// Standard headers.
#include <cassert>
#include <string>
#include <utility>

using namespace foundation;
using namespace std;

namespace renderer
{

APPLESEED_DEFINE_APIARRAY(BSDFFactoryArray);

struct BSDFFactoryRegistrar::Impl
{
    Registrar<IBSDFFactory> m_registrar;
};

BSDFFactoryRegistrar::BSDFFactoryRegistrar(const SearchPaths& search_paths)
  : impl(new Impl())
{
    reinitialize(search_paths);
}

BSDFFactoryRegistrar::~BSDFFactoryRegistrar()
{
    delete impl;
}

void BSDFFactoryRegistrar::reinitialize(const SearchPaths& search_paths)
{
    // The registrar must be cleared before the plugins are unloaded.
    impl->m_registrar.clear();
    unload_all_plugins();

    // Register built-in factories.
    register_factory(auto_release_ptr<FactoryType>(new AshikhminBRDFFactory()));
    register_factory(auto_release_ptr<FactoryType>(new BlinnBRDFFactory()));
    register_factory(auto_release_ptr<FactoryType>(new BSDFBlendFactory()));
    register_factory(auto_release_ptr<FactoryType>(new BSDFMixFactory()));
    register_factory(auto_release_ptr<FactoryType>(new DiffuseBTDFFactory()));
    register_factory(auto_release_ptr<FactoryType>(new DisneyBRDFFactory()));
    register_factory(auto_release_ptr<FactoryType>(new GlassBSDFFactory()));
    register_factory(auto_release_ptr<FactoryType>(new GlossyBRDFFactory()));
    register_factory(auto_release_ptr<FactoryType>(new KelemenBRDFFactory()));
    register_factory(auto_release_ptr<FactoryType>(new LambertianBRDFFactory()));
    register_factory(auto_release_ptr<FactoryType>(new MetalBRDFFactory()));
    register_factory(auto_release_ptr<FactoryType>(new OrenNayarBRDFFactory()));
    register_factory(auto_release_ptr<FactoryType>(new PlasticBRDFFactory()));
    register_factory(auto_release_ptr<FactoryType>(new SheenBRDFFactory()));
    register_factory(auto_release_ptr<FactoryType>(new SpecularBRDFFactory()));
    register_factory(auto_release_ptr<FactoryType>(new SpecularBTDFFactory()));

    // Register factories defined in plugins.
    register_factories_from_plugins<BSDF>(
        search_paths,
        [this](void* plugin_entry_point)
        {
            auto create_fn = reinterpret_cast<IBSDFFactory* (*)()>(plugin_entry_point);
            register_factory(foundation::auto_release_ptr<IBSDFFactory>(create_fn()));
        });
}

BSDFFactoryArray BSDFFactoryRegistrar::get_factories() const
{
    FactoryArrayType factories;

    for (const_each<Registrar<FactoryType>::Items> i = impl->m_registrar.items(); i; ++i)
        factories.push_back(i->second);

    return factories;
}

const BSDFFactoryRegistrar::FactoryType* BSDFFactoryRegistrar::lookup(const char* name) const
{
    assert(name);
    return impl->m_registrar.lookup(name);
}

void BSDFFactoryRegistrar::register_factory(auto_release_ptr<FactoryType> factory)
{
    const string model = factory->get_model();
    impl->m_registrar.insert(model, move(factory));
}

}   // namespace renderer
