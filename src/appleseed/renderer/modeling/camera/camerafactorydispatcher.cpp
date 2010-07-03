
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
#include "camerafactorydispatcher.h"

// appleseed.renderer headers.
#include "renderer/modeling/camera/pinholecamera.h"
#include "renderer/modeling/camera/thinlenscamera.h"

// appleseed.foundation headers.
#include "foundation/utility/dispatcher.h"

using namespace foundation;

namespace renderer
{

//
// CameraFactoryDispatcher class implementation.
//

struct CameraFactoryDispatcher::Impl
{
    Dispatcher<CreateFunctionPtr> m_dispatcher;
};

// Constructor.
CameraFactoryDispatcher::CameraFactoryDispatcher()
  : impl(new Impl())
{
    // Declare the various factory functions.
    impl->m_dispatcher.declare(
        PinholeCameraFactory::get_model(),
        &PinholeCameraFactory::create);
    impl->m_dispatcher.declare(
        ThinLensCameraFactory::get_model(),
        &ThinLensCameraFactory::create);
}

// Destructor.
CameraFactoryDispatcher::~CameraFactoryDispatcher()
{
    delete impl;
}

// Lookup a factory function by name.
CameraFactoryDispatcher::CreateFunctionPtr
CameraFactoryDispatcher::lookup(const char* name) const
{
    assert(name);
    return impl->m_dispatcher.lookup(name);
}

}   // namespace renderer
