
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Esteban Tovagliari, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_MODELING_SCENE_PLUGINASSEMBLY_H
#define APPLESEED_RENDERER_MODELING_SCENE_PLUGINASSEMBLY_H

// appleseed.renderer headers.
#include "renderer/modeling/scene/basegroup.h"
#include "renderer/modeling/scene/iassemblyfactory.h"
#include "renderer/modeling/scene/proceduralassembly.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/utility/autoreleaseptr.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Project; }

namespace renderer
{

//
// An assembly that generates its contents procedurally
// by loading and executing the code in a plugin.
//

class APPLESEED_DLLSYMBOL PluginAssembly
  : public ProceduralAssembly
{
  public:
    // Destructor.
    virtual ~PluginAssembly() override;

    // Return a string identifying the model of this entity.
    virtual const char* get_model() const override;

    // Delete this instance.
    virtual void release() override;

    virtual bool expand_contents(
        const Project&              project,
        const Assembly*             parent,
        foundation::IAbortSwitch*   abort_switch = nullptr) override;

  protected:
    friend class PluginAssemblyFactory;

    // Constructor.
    PluginAssembly(
        const char*                 name,
        const ParamArray&           params);

  private:
    Assembly* m_plugin_assembly;
};


//
// PluginAssembly factory.
//

class APPLESEED_DLLSYMBOL PluginAssemblyFactory
  : public IAssemblyFactory
{
  public:
    // Delete this instance.
    virtual void release() override;

    // Return a string identifying this assembly model.
    virtual const char* get_model() const override;

    // Create a new assembly.
    virtual foundation::auto_release_ptr<Assembly> create(
        const char*         name,
        const ParamArray&   params = ParamArray()) const override;

    // Static variant of the create() method above.
    static foundation::auto_release_ptr<Assembly> static_create(
        const char*         name,
        const ParamArray&   params = ParamArray());
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_SCENE_PLUGINASSEMBLY_H
