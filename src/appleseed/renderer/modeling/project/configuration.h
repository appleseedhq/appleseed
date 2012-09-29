
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

#ifndef APPLESEED_RENDERER_MODELING_PROJECT_CONFIGURATION_H
#define APPLESEED_RENDERER_MODELING_PROJECT_CONFIGURATION_H

// appleseed.main headers.
#include "main/dllsymbol.h"

// appleseed.renderer headers.
#include "renderer/global/global.h"
#include "renderer/modeling/entity/entity.h"

namespace renderer
{

//
// A configuration is a named set of parameters, possibly inheriting
// parameters from a base configuration.
//

class DLLSYMBOL Configuration
  : public Entity
{
  public:
    // Delete this instance.
    virtual void release();

    // Set the base configuration.
    // A null pointer is a valid value.
    void set_base(const Configuration* base);

    // Get the base configuration.
    // Returns a null pointer if there is no base configuration.
    const Configuration* get_base() const;

    // Construct a set of parameters from the parameters inherited
    // from the base configuration and the ones of this configuration.
    ParamArray get_inherited_parameters() const;

  private:
    friend class ConfigurationFactory;
    friend class BaseConfigurationFactory;

    const Configuration* m_base;

    // Constructor.
    explicit Configuration(const char* name);
};


//
// Configuration factory.
//

class DLLSYMBOL ConfigurationFactory
{
  public:
    // Create a new empty configuration.
    static foundation::auto_release_ptr<Configuration> create(const char* name);
    static foundation::auto_release_ptr<Configuration> create(
        const char*         name,
        const ParamArray&   params);
};


//
// Base configuration factory.
//

class DLLSYMBOL BaseConfigurationFactory
{
  public:
    // Create a new "base_final" configuration.
    static foundation::auto_release_ptr<Configuration> create_base_final();

    // Create a new "base_interactive" configuration.
    static foundation::auto_release_ptr<Configuration> create_base_interactive();

    // Return true if the string passed in argument is the name of a base configuration.
    static bool is_base_configuration(const char* name);
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_PROJECT_CONFIGURATION_H
