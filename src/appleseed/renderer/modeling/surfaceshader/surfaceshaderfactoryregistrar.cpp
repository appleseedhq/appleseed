
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "renderer/modeling/surfaceshader/physicalsurfaceshader.h"
#include "renderer/modeling/surfaceshader/smokesurfaceshader.h"
#include "renderer/modeling/surfaceshader/voxelaosurfaceshader.h"

// appleseed.foundation headers.
#include "foundation/utility/registrar.h"

using namespace foundation;
using namespace std;

namespace renderer
{

//
// SurfaceShaderFactoryRegistrar class implementation.
//

struct SurfaceShaderFactoryRegistrar::Impl
{
    Registrar<ISurfaceShaderFactory> m_registrar;
};

SurfaceShaderFactoryRegistrar::SurfaceShaderFactoryRegistrar()
  : impl(new Impl())
{
    impl->m_registrar.insert(
        AOSurfaceShaderFactory::get_model(),
        auto_ptr<ISurfaceShaderFactory>(new AOSurfaceShaderFactory()));

    impl->m_registrar.insert(
        ConstantSurfaceShaderFactory::get_model(),
        auto_ptr<ISurfaceShaderFactory>(new ConstantSurfaceShaderFactory()));

    impl->m_registrar.insert(
        DiagnosticSurfaceShaderFactory::get_model(),
        auto_ptr<ISurfaceShaderFactory>(new DiagnosticSurfaceShaderFactory()));

    impl->m_registrar.insert(
        PhysicalSurfaceShaderFactory::get_model(),
        auto_ptr<ISurfaceShaderFactory>(new PhysicalSurfaceShaderFactory()));

    impl->m_registrar.insert(
        SmokeSurfaceShaderFactory::get_model(),
        auto_ptr<ISurfaceShaderFactory>(new SmokeSurfaceShaderFactory()));

    impl->m_registrar.insert(
        VoxelAOSurfaceShaderFactory::get_model(),
        auto_ptr<ISurfaceShaderFactory>(new VoxelAOSurfaceShaderFactory()));
}

const SurfaceShaderFactoryRegistrar::FactoryType*
SurfaceShaderFactoryRegistrar::lookup(const char* name) const
{
    assert(name);

    return impl->m_registrar.lookup(name);
}

}   // namespace renderer
