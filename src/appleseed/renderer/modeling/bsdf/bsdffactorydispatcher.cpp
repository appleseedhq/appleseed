
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
#include "bsdffactorydispatcher.h"

// appleseed.renderer headers.
#include "renderer/modeling/bsdf/ashikhminbrdf.h"
#include "renderer/modeling/bsdf/lambertianbrdf.h"
#include "renderer/modeling/bsdf/lambertianbtdf.h"
#include "renderer/modeling/bsdf/phongbrdf.h"
#include "renderer/modeling/bsdf/phongbtdf.h"
#include "renderer/modeling/bsdf/specularbrdf.h"
#include "renderer/modeling/bsdf/specularbtdf.h"

// appleseed.foundation headers.
#include "foundation/utility/dispatcher.h"

using namespace foundation;

namespace renderer
{

//
// BSDFFactoryDispatcher class implementation.
//

struct BSDFFactoryDispatcher::Impl
{
    Dispatcher<CreateFunctionPtr> m_dispatcher;
};

// Constructor.
BSDFFactoryDispatcher::BSDFFactoryDispatcher()
  : impl(new Impl())
{
    // Declare the various factory functions.
    impl->m_dispatcher.declare(
        AshikhminBRDFFactory::get_model(),
        &AshikhminBRDFFactory::create);
    impl->m_dispatcher.declare(
        LambertianBRDFFactory::get_model(),
        &LambertianBRDFFactory::create);
//     impl->m_dispatcher.declare(
//         LambertianBTDFFactory::get_model(),
//         &LambertianBTDFFactory::create);
    impl->m_dispatcher.declare(
        PhongBRDFFactory::get_model(),
        &PhongBRDFFactory::create);
//     impl->m_dispatcher.declare(
//         PhongBTDFFactory::get_model(),
//         &PhongBTDFFactory::create);
    impl->m_dispatcher.declare(
        SpecularBRDFFactory::get_model(),
        &SpecularBRDFFactory::create);
    impl->m_dispatcher.declare(
        SpecularBTDFFactory::get_model(),
        &SpecularBTDFFactory::create);
}

// Destructor.
BSDFFactoryDispatcher::~BSDFFactoryDispatcher()
{
    delete impl;
}

// Lookup a factory function by name.
BSDFFactoryDispatcher::CreateFunctionPtr
BSDFFactoryDispatcher::lookup(const char* name) const
{
    assert(name);
    return impl->m_dispatcher.lookup(name);
}

}   // namespace renderer
