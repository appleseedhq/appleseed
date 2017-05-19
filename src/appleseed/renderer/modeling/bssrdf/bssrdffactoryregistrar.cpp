
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2017 Francois Beaune, The appleseedhq Organization
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
#include "bssrdffactoryregistrar.h"

// appleseed.renderer headers.
#include "renderer/modeling/bssrdf/betterdipolebssrdf.h"
#include "renderer/modeling/bssrdf/directionaldipolebssrdf.h"
#include "renderer/modeling/bssrdf/gaussianbssrdf.h"
#include "renderer/modeling/bssrdf/ibssrdffactory.h"
#include "renderer/modeling/bssrdf/normalizeddiffusionbssrdf.h"
#include "renderer/modeling/bssrdf/standarddipolebssrdf.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"
#include "foundation/utility/registrar.h"

// Standard headers.
#include <cassert>
#include <string>

using namespace foundation;
using namespace std;

namespace renderer
{

APPLESEED_DEFINE_APIARRAY(BSSRDFFactoryArray);

struct BSSRDFFactoryRegistrar::Impl
{
    Registrar<IBSSRDFFactory> m_registrar;
};

BSSRDFFactoryRegistrar::BSSRDFFactoryRegistrar()
  : impl(new Impl())
{
    register_factory(auto_ptr<FactoryType>(new BetterDipoleBSSRDFFactory()));
    register_factory(auto_ptr<FactoryType>(new DirectionalDipoleBSSRDFFactory()));
    register_factory(auto_ptr<FactoryType>(new GaussianBSSRDFFactory()));
    register_factory(auto_ptr<FactoryType>(new NormalizedDiffusionBSSRDFFactory()));
    register_factory(auto_ptr<FactoryType>(new StandardDipoleBSSRDFFactory()));
}

BSSRDFFactoryRegistrar::~BSSRDFFactoryRegistrar()
{
    delete impl;
}

void BSSRDFFactoryRegistrar::register_factory(auto_ptr<FactoryType> factory)
{
    const string model = factory->get_model();
    impl->m_registrar.insert(model, factory);
}

BSSRDFFactoryArray BSSRDFFactoryRegistrar::get_factories() const
{
    FactoryArrayType factories;

    for (const_each<Registrar<FactoryType>::Items> i = impl->m_registrar.items(); i; ++i)
        factories.push_back(i->second);

    return factories;
}

const BSSRDFFactoryRegistrar::FactoryType* BSSRDFFactoryRegistrar::lookup(const char* name) const
{
    assert(name);

    return impl->m_registrar.lookup(name);
}

}   // namespace renderer
