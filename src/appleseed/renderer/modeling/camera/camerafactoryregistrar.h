
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

#ifndef APPLESEED_RENDERER_MODELING_CAMERA_CAMERAFACTORYREGISTRAR_H
#define APPLESEED_RENDERER_MODELING_CAMERA_CAMERAFACTORYREGISTRAR_H

// appleseed.main headers.
#include "main/dllsymbol.h"

// appleseed.renderer headers.
#include "renderer/global/global.h"

// appleseed.foundation headers.
#include "foundation/math/transform.h"
#include "foundation/utility/containers/array.h"

// Forward declarations.
namespace renderer      { class ICameraFactory; }

namespace renderer
{

//
// An array of camera factories.
//

DECLARE_ARRAY(CameraFactoryArray, ICameraFactory*);


//
// Camera factory registrar.
//

class DLLSYMBOL CameraFactoryRegistrar
  : public foundation::NonCopyable
{
  public:
    typedef ICameraFactory FactoryType;
    typedef CameraFactoryArray FactoryArrayType;

    // Constructor.
    CameraFactoryRegistrar();

    // Destructor.
    ~CameraFactoryRegistrar();

    // Register a camera factory.
    void register_factory(std::auto_ptr<FactoryType> factory);

    // Retrieve the registered factories.
    FactoryArrayType get_factories() const;

    // Lookup a factory by name.
    const FactoryType* lookup(const char* name) const;

  private:
    struct Impl;
    Impl* impl;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_CAMERA_CAMERAFACTORYREGISTRAR_H
