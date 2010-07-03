
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
#include "surfaceshaderfactorydispatcher.h"

// appleseed.renderer headers.
#include "renderer/modeling/surfaceshader/aosurfaceshader.h"
#include "renderer/modeling/surfaceshader/constantsurfaceshader.h"
#include "renderer/modeling/surfaceshader/diagnosticsurfaceshader.h"
#include "renderer/modeling/surfaceshader/physicalsurfaceshader.h"
#include "renderer/modeling/surfaceshader/smokesurfaceshader.h"
#include "renderer/modeling/surfaceshader/voxelaosurfaceshader.h"

// appleseed.foundation headers.
#include "foundation/utility/dispatcher.h"

using namespace foundation;

namespace renderer
{

//
// SurfaceShaderFactoryDispatcher class implementation.
//

struct SurfaceShaderFactoryDispatcher::Impl
{
    Dispatcher<CreateFunctionPtr> m_dispatcher;
};

// Constructor.
SurfaceShaderFactoryDispatcher::SurfaceShaderFactoryDispatcher()
  : impl(new Impl())
{
    // Declare the various factory functions.
    impl->m_dispatcher.declare(
        AOSurfaceShaderFactory::get_model(),
        &AOSurfaceShaderFactory::create);
    impl->m_dispatcher.declare(
        ConstantSurfaceShaderFactory::get_model(),
        &ConstantSurfaceShaderFactory::create);
    impl->m_dispatcher.declare(
        DiagnosticSurfaceShaderFactory::get_model(),
        &DiagnosticSurfaceShaderFactory::create);
    impl->m_dispatcher.declare(
        PhysicalSurfaceShaderFactory::get_model(),
        &PhysicalSurfaceShaderFactory::create);
    impl->m_dispatcher.declare(
        SmokeSurfaceShaderFactory::get_model(),
        &SmokeSurfaceShaderFactory::create);
    impl->m_dispatcher.declare(
        VoxelAOSurfaceShaderFactory::get_model(),
        &VoxelAOSurfaceShaderFactory::create);
}

// Destructor.
SurfaceShaderFactoryDispatcher::~SurfaceShaderFactoryDispatcher()
{
    delete impl;
}

// Lookup a factory function by name.
SurfaceShaderFactoryDispatcher::CreateFunctionPtr
SurfaceShaderFactoryDispatcher::lookup(const char* name) const
{
    assert(name);
    return impl->m_dispatcher.lookup(name);
}

}   // namespace renderer
