
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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

#pragma once

// appleseed.renderer headers.
#include "renderer/modeling/entity/connectableentity.h"

// appleseed.foundation headers.
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/uid.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace foundation    { class DictionaryArray; }
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class BaseGroup; }
namespace renderer      { class EnvironmentEDF; }
namespace renderer      { class EnvironmentShader; }
namespace renderer      { class OnFrameBeginRecorder; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Project; }

namespace renderer
{

//
// Environment.
//

class APPLESEED_DLLSYMBOL Environment
  : public ConnectableEntity
{
  public:
    // Return the unique ID of this class of entities.
    static foundation::UniqueID get_class_uid();

    // Delete this instance.
    void release() override;

    // Return a string identifying the model of this environment.
    const char* get_model() const;

    bool on_frame_begin(
        const Project&              project,
        const BaseGroup*            parent,
        OnFrameBeginRecorder&       recorder,
        foundation::IAbortSwitch*   abort_switch = nullptr) override;

    void on_frame_end(
        const Project&              project,
        const BaseGroup*            parent) override;

    //
    // The get_*() methods below retrieve entities that were cached by on_frame_begin().
    // To retrieve the entities before on_frame_begin() or after on_frame_end() is called,
    // use the get_uncached_*() variants.
    //

    // Return the EDF of this environment, or 0 if the environment doesn't have one.
    EnvironmentEDF* get_environment_edf() const;
    EnvironmentEDF* get_uncached_environment_edf() const;

    // Return the shader of this environment, or 0 if the environment doesn't have one.
    EnvironmentShader* get_environment_shader() const;
    EnvironmentShader* get_uncached_environment_shader() const;

  private:
    friend class EnvironmentFactory;

    EnvironmentEDF*     m_environment_edf;
    EnvironmentShader*  m_environment_shader;

    // Constructor.
    Environment(
        const char*                 name,
        const ParamArray&           params);
};


//
// Environment factory.
//

class APPLESEED_DLLSYMBOL EnvironmentFactory
{
  public:
    // Return a string identifying this environment model.
    static const char* get_model();

    // Return a set of input metadata for this environment model.
    static foundation::DictionaryArray get_input_metadata();

    // Create a new environment.
    static foundation::auto_release_ptr<Environment> create(
        const char*         name,
        const ParamArray&   params);
};


//
// Environment class implementation.
//

inline EnvironmentEDF* Environment::get_environment_edf() const
{
    return m_environment_edf;
}

inline EnvironmentShader* Environment::get_environment_shader() const
{
    return m_environment_shader;
}

}   // namespace renderer
