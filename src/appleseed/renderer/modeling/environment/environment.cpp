
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
#include "environment.h"

using namespace foundation;
using namespace std;

namespace renderer
{

//
// Environment class implementation.
//

struct Environment::Impl
{
    string  m_name;
};

// Constructors.
Environment::Environment(
    const char*                         name)
{
    construct(name, 0, 0);
}
Environment::Environment(
    const char*                         name,
    const EnvironmentEDF*               environment_edf)
{
    construct(name, environment_edf, 0);
}
Environment::Environment(
    const char*                         name,
    const EnvironmentShader*            environment_shader)
{
    construct(name, 0, environment_shader);
}
Environment::Environment(
    const char*                         name,
    const EnvironmentEDF*               environment_edf,
    const EnvironmentShader*            environment_shader)
{
    construct(name, environment_edf, environment_shader);
}
Environment::Environment(
    const char*                         name,
    const ParamArray&                   params,
    const EnvironmentEDFContainer&      environment_edfs,
    const EnvironmentShaderContainer&   environment_shaders)
  : Entity(params)
{
    const EnvironmentEDF* environment_edf =
        get_optional_entity<EnvironmentEDF>(
            environment_edfs,
            params,
            "environment_edf");

    const EnvironmentShader* environment_shader =
        get_optional_entity<EnvironmentShader>(
            environment_shaders,
            params,
            "environment_shader");

    construct(name, environment_edf, environment_shader);
}

void Environment::construct(
    const char*                         name,
    const EnvironmentEDF*               environment_edf,
    const EnvironmentShader*            environment_shader)
{
    impl = new Impl();

    assert(name);
    impl->m_name = name;

    m_environment_edf = environment_edf;
    m_environment_shader = environment_shader;
}

// Destructor.
Environment::~Environment()
{
    delete impl;
}

// Delete this instance.
void Environment::release()
{
    delete this;
}

// Return the name of this environment.
const char* Environment::get_name() const
{
    return impl->m_name.c_str();
}

// Return a string identifying the model of this environment.
const char* Environment::get_model() const
{
    return EnvironmentFactory::get_model();
}


//
// EnvironmentFactory class implementation.
//

// Return a string identifying this environment model.
const char* EnvironmentFactory::get_model()
{
    return "generic_environment";
}

// Create a new environment.
auto_release_ptr<Environment> EnvironmentFactory::create(
    const char*                         name)
{
    return auto_release_ptr<Environment>(new Environment(name));
}
auto_release_ptr<Environment> EnvironmentFactory::create(
    const char*                         name,
    const EnvironmentEDF*               environment_edf)
{
    return
        auto_release_ptr<Environment>(
            new Environment(
                name,
                environment_edf));
}
auto_release_ptr<Environment> EnvironmentFactory::create(
    const char*                         name,
    const EnvironmentShader*            environment_shader)
{
    return
        auto_release_ptr<Environment>(
            new Environment(
                name,
                environment_shader));
}
auto_release_ptr<Environment> EnvironmentFactory::create(
    const char*                         name,
    const EnvironmentEDF*               environment_edf,
    const EnvironmentShader*            environment_shader)
{
    return
        auto_release_ptr<Environment>(
            new Environment(
                name,
                environment_edf,
                environment_shader));
}
auto_release_ptr<Environment> EnvironmentFactory::create(
    const char*                         name,
    const ParamArray&                   params,
    const EnvironmentEDFContainer&      environment_edfs,
    const EnvironmentShaderContainer&   environment_shaders)
{
    return
        auto_release_ptr<Environment>(
            new Environment(
                name,
                params,
                environment_edfs,
                environment_shaders));
}

}   // namespace renderer
