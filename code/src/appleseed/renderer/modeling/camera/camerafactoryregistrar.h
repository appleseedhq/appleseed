
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

#ifndef APPLESEED_RENDERER_MODELING_CAMERA_CAMERAFACTORYREGISTRAR_H
#define APPLESEED_RENDERER_MODELING_CAMERA_CAMERAFACTORYREGISTRAR_H

// appleseed.renderer headers.
#include "renderer/modeling/entity/entityfactoryregistrar.h"

// appleseed.foundation headers.
#include "foundation/utility/api/apiarray.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/searchpaths.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace renderer      { class ICameraFactory; }

namespace renderer
{

//
// An array of camera factories.
//

APPLESEED_DECLARE_APIARRAY(CameraFactoryArray, ICameraFactory*);


//
// Camera factory registrar.
//

class APPLESEED_DLLSYMBOL CameraFactoryRegistrar
  : public EntityFactoryRegistrar
{
  public:
    typedef ICameraFactory FactoryType;
    typedef CameraFactoryArray FactoryArrayType;

    // Constructor.
    explicit CameraFactoryRegistrar(
        const foundation::SearchPaths& search_paths = foundation::SearchPaths());

    // Destructor.
    ~CameraFactoryRegistrar();

    // Reinitialize the registrar; load plugins found in provided search paths.
    void reinitialize(const foundation::SearchPaths& search_paths);

    // Retrieve the registered factories.
    FactoryArrayType get_factories() const;

    // Lookup a factory by name.
    const FactoryType* lookup(const char* name) const;

  private:
    struct Impl;
    Impl* impl;

    // Register a factory.
    void register_factory(foundation::auto_release_ptr<FactoryType> factory);
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_CAMERA_CAMERAFACTORYREGISTRAR_H
