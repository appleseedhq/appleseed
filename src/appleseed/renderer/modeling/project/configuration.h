
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
#include "renderer/modeling/entity/entity.h"

// appleseed.foundation headers.
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/uid.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace foundation    { class Dictionary; }
namespace renderer      { class ParamArray; }

namespace renderer
{

//
// A configuration is a named set of parameters, possibly inheriting
// parameters from a base configuration.
//

class APPLESEED_DLLSYMBOL Configuration
  : public Entity
{
  public:
    // Return the unique ID of this class of entities.
    static foundation::UniqueID get_class_uid();

    // Return configuration metadata.
    static foundation::Dictionary get_metadata();

    // Delete this instance.
    void release() override;

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
    friend class BaseConfigurationFactory;
    friend class ConfigurationFactory;

    const Configuration* m_base;

    // Constructor.
    explicit Configuration(const char* name);
};


//
// Configuration factory.
//

class APPLESEED_DLLSYMBOL ConfigurationFactory
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

class APPLESEED_DLLSYMBOL BaseConfigurationFactory
{
  public:
    // Instantiate the built-in "base_final" configuration.
    static foundation::auto_release_ptr<Configuration> create_base_final();

    // Instantiate the built-in "base_interactive" configuration.
    static foundation::auto_release_ptr<Configuration> create_base_interactive();

    // Return true if the string passed in argument is the name of a base configuration.
    static bool is_base_final_configuration(const char* name);
    static bool is_base_interactive_configuration(const char* name);
    static bool is_base_configuration(const char* name);
};

}   // namespace renderer
