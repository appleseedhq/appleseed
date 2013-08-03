
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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
#include "environment.h"

// appleseed.renderer headers.
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/environmentshader/environmentshader.h"
#include "renderer/modeling/input/inputarray.h"

// appleseed.foundation headers.
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"
#include "foundation/utility/uid.h"

using namespace foundation;
using namespace std;

namespace renderer
{

//
// Environment class implementation.
//

namespace
{
    const UniqueID g_class_uid = new_guid();
}

Environment::Environment(
    const char*         name,
    const ParamArray&   params)
  : ConnectableEntity(g_class_uid, params)
  , m_environment_edf(0)
  , m_environment_shader(0)
{
    set_name(name);

    m_inputs.declare("environment_edf", InputFormatEntity, "");
    m_inputs.declare("environment_shader", InputFormatEntity, "");
}

void Environment::release()
{
    delete this;
}

bool Environment::on_frame_begin(const Project& project)
{
    m_environment_edf = static_cast<EnvironmentEDF*>(m_inputs.get_entity("environment_edf"));
    m_environment_shader = static_cast<EnvironmentShader*>(m_inputs.get_entity("environment_shader"));

    return true;
}

void Environment::on_frame_end(const Project& project)
{
    m_environment_edf = 0;
    m_environment_shader = 0;
}

const char* Environment::get_model() const
{
    return EnvironmentFactory::get_model();
}


//
// EnvironmentFactory class implementation.
//

const char* EnvironmentFactory::get_model()
{
    return "generic_environment";
}

DictionaryArray EnvironmentFactory::get_input_metadata()
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "environment_edf")
            .insert("label", "Environment EDF")
            .insert("type", "entity")
            .insert("entity_types",
                Dictionary().insert("environment_edf", "Environment EDFs"))
            .insert("use", "optional"));

    metadata.push_back(
        Dictionary()
            .insert("name", "environment_shader")
            .insert("label", "Environment Shader")
            .insert("type", "entity")
            .insert("entity_types",
                Dictionary().insert("environment_shader", "Environment Shaders"))
            .insert("use", "optional"));

    return metadata;
}

auto_release_ptr<Environment> EnvironmentFactory::create(
    const char*         name,
    const ParamArray&   params)
{
    return auto_release_ptr<Environment>(new Environment(name, params));
}

}   // namespace renderer
