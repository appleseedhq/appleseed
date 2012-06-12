
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
#include "surfaceshaderfactoryregistrar.h"

// appleseed.renderer headers.
#include "renderer/modeling/surfaceshader/aosurfaceshader.h"
#include "renderer/modeling/surfaceshader/constantsurfaceshader.h"
#include "renderer/modeling/surfaceshader/diagnosticsurfaceshader.h"
#include "renderer/modeling/surfaceshader/fastsubsurfacescatteringsurfaceshader.h"
#include "renderer/modeling/surfaceshader/isurfaceshaderfactory.h"
#include "renderer/modeling/surfaceshader/leafsurfaceshader.h"
#include "renderer/modeling/surfaceshader/physicalsurfaceshader.h"
#include "renderer/modeling/surfaceshader/smokesurfaceshader.h"
#include "renderer/modeling/surfaceshader/voxelaosurfaceshader.h"

// appleseed.foundation headers.
#include "foundation/utility/registrar.h"

using namespace foundation;
using namespace std;

namespace renderer
{

DEFINE_ARRAY(SurfaceShaderFactoryArray);

struct SurfaceShaderFactoryRegistrar::Impl
{
    Registrar<ISurfaceShaderFactory> m_registrar;
};

SurfaceShaderFactoryRegistrar::SurfaceShaderFactoryRegistrar()
  : impl(new Impl())
{
    register_factory(auto_ptr<FactoryType>(new AOSurfaceShaderFactory()));
    register_factory(auto_ptr<FactoryType>(new ConstantSurfaceShaderFactory()));
    register_factory(auto_ptr<FactoryType>(new DiagnosticSurfaceShaderFactory()));
    register_factory(auto_ptr<FactoryType>(new FastSubSurfaceScatteringSurfaceShaderFactory()));
    register_factory(auto_ptr<FactoryType>(new LeafSurfaceShaderFactory()));
    register_factory(auto_ptr<FactoryType>(new PhysicalSurfaceShaderFactory()));
    register_factory(auto_ptr<FactoryType>(new SmokeSurfaceShaderFactory()));
    register_factory(auto_ptr<FactoryType>(new VoxelAOSurfaceShaderFactory()));
}

SurfaceShaderFactoryRegistrar::~SurfaceShaderFactoryRegistrar()
{
    delete impl;
}

void SurfaceShaderFactoryRegistrar::register_factory(auto_ptr<FactoryType> factory)
{
    const string model = factory->get_model();
    impl->m_registrar.insert(model, factory);
}

SurfaceShaderFactoryArray SurfaceShaderFactoryRegistrar::get_factories() const
{
    FactoryArrayType factories;

    for (const_each<Registrar<FactoryType>::Items> i = impl->m_registrar.items(); i; ++i)
        factories.push_back(i->second);

    return factories;
}

const SurfaceShaderFactoryRegistrar::FactoryType*
SurfaceShaderFactoryRegistrar::lookup(const char* name) const
{
    assert(name);

    return impl->m_registrar.lookup(name);
}

}   // namespace renderer
