
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

#ifndef APPLESEED_RENDERER_MODELING_ENVIRONMENT_ENVIRONMENT_H
#define APPLESEED_RENDERER_MODELING_ENVIRONMENT_ENVIRONMENT_H

// appleseed.renderer headers.
#include "renderer/global/global.h"
#include "renderer/modeling/entity/entity.h"
#include "renderer/modeling/scene/containers.h"

// Forward declarations.
namespace renderer      { class EnvironmentEDF; }
namespace renderer      { class EnvironmentShader; }

namespace renderer
{

//
// Environment.
//

class RENDERERDLL Environment
  : public Entity
{
  public:
    // Delete this instance.
    virtual void release();

    // Return a string identifying the model of this environment.
    const char* get_model() const;

    // Return the EDF of this environment, or 0 if the environment doesn't have one.
    const EnvironmentEDF* get_environment_edf() const;

    // Return the shader of this environment, or 0 if the environment doesn't have one.
    const EnvironmentShader* get_environment_shader() const;

  private:
    friend class EnvironmentFactory;

    const EnvironmentEDF*       m_environment_edf;
    const EnvironmentShader*    m_environment_shader;

    // Constructors.
    Environment(
        const char*                         name);
    Environment(
        const char*                         name,
        const EnvironmentEDF*               environment_edf);
    Environment(
        const char*                         name,
        const EnvironmentShader*            environment_shader);
    Environment(
        const char*                         name,
        const EnvironmentEDF*               environment_edf,
        const EnvironmentShader*            environment_shader);
    Environment(
        const char*                         name,
        const ParamArray&                   params,
        const EnvironmentEDFContainer&      environment_edfs,
        const EnvironmentShaderContainer&   environment_shaders);
};


//
// Environment factory.
//

class RENDERERDLL EnvironmentFactory
{
  public:
    // Return a string identifying this environment model.
    static const char* get_model();

    // Create a new environment.
    static foundation::auto_release_ptr<Environment> create(
        const char*                         name);
    static foundation::auto_release_ptr<Environment> create(
        const char*                         name,
        const EnvironmentEDF*               environment_edf);
    static foundation::auto_release_ptr<Environment> create(
        const char*                         name,
        const EnvironmentShader*            environment_shader);
    static foundation::auto_release_ptr<Environment> create(
        const char*                         name,
        const EnvironmentEDF*               environment_edf,
        const EnvironmentShader*            environment_shader);
    static foundation::auto_release_ptr<Environment> create(
        const char*                         name,
        const ParamArray&                   params,
        const EnvironmentEDFContainer&      environment_edfs,
        const EnvironmentShaderContainer&   environment_shaders);
};


//
// Environment class implementation.
//

// Return the EDF of this environment, or 0 if the environment doesn't have one.
inline const EnvironmentEDF* Environment::get_environment_edf() const
{
    return m_environment_edf;
}

// Return the shader of this environment, or 0 if the environment doesn't have one.
inline const EnvironmentShader* Environment::get_environment_shader() const
{
    return m_environment_shader;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_ENVIRONMENT_ENVIRONMENT_H
