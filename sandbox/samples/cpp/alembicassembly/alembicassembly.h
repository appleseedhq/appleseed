
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
#ifndef APPLESEED_ALEMBIC_ASSEMBLY_H
#define APPLESEED_ALEMBIC_ASSEMBLY_H

#define APPLESEED_ENABLE_IMATH_INTEROP

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/entity/connectableentity.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/math/dual.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/uid.h"

#include <Alembic/Abc/All.h>

// appleseed.main headers.
#include "main/dllsymbol.h"

namespace asf = foundation;
namespace asr = renderer;

namespace
{

//
// Alembic Assembly
//

class AlembicAssembly
  : public asr::ProceduralAssembly
{
  public:
    // Constructor.
    AlembicAssembly(
        const char*                     name,
        const asr::ParamArray&          params);

    // Return a string identifying the model of this entity.
    const char* get_model() const override
    {
        return Model;
    };

    // Delete this instance.
    void release() override
    {
        delete this;
    }

    // This method is called once before rendering each frame.
    // Returns true on success, false otherwise.
    /*bool on_frame_begin(
        const Project&                  project,
        const BaseGroup*                parent,
        OnFrameBeginRecorder&           recorder,
        foundation::IAbortSwitch*       abort_switch = nullptr) override;*/

    // Expand the contents of the assembly.
    bool do_expand_contents(
        const asr::Project&     project,
        const asr::Assembly*    parent,
        asf::IAbortSwitch*      abort_switch = 0) override;

  private:

    // retrieve and store assembly parameters
    void retrieve_params(const asr::Project& project);

    void foo(const asr::Assembly* assembly, Alembic::Abc::IObject o);

    static const char* Model;

    std::string m_file_path;
    float m_shutter_open_time;
    float m_shutter_close_time;
    std::vector<asf::Matrix4d> m_mtx_stack;
    std::vector<asr::TransformSequence> m_xform_seq_stack;
};


//
// Factory for the new assembly model.
//

class AlembicAssemblyFactory
  : public asr::IAssemblyFactory
{
  public:
    // Delete this instance.
    void release() override
    {
        delete this;
    }

    // Return a string identifying this assembly model.
    const char* get_model() const override
    {
        return Model;
    }

    // Create a new assembly.
    asf::auto_release_ptr<asr::Assembly> create(
        const char*             name,
        const asr::ParamArray&  params) const override
    {
        return asf::auto_release_ptr<asr::Assembly>(
            new AlembicAssembly(name, params));
    }

  private:

    static const char* Model;

};

}       // namespace

#endif  // !APPLESEED_RENDERER_MODELING_CAMERA_CAMERA_H
