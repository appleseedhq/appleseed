
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
#include "renderer/modeling/input/inputarray.h"

// appleseed.foundation headers.
#include "foundation/utility/uid.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace renderer  { class ParamArray; }
namespace renderer  { class Source; }

namespace renderer
{

//
// Base class for all entities with inputs.
//

class APPLESEED_DLLSYMBOL ConnectableEntity
  : public Entity
{
  public:
    // Constructors.
    explicit ConnectableEntity(
        const foundation::UniqueID  class_uid);
    ConnectableEntity(
        const foundation::UniqueID  class_uid,
        const ParamArray&           params);

    // Return the inputs of this instance.
    InputArray& get_inputs();
    const InputArray& get_inputs() const;

  protected:
    InputArray m_inputs;

    static bool is_uniform_zero_scalar(const Source* source);
    static bool is_uniform_zero_spectrum(const Source* source);

    bool is_uniform_zero_scalar(const char* input_name) const;
    bool is_uniform_zero_spectrum(const char* input_name) const;

    static bool is_uniform_zero(const Source* source, const Source* multiplier);
    bool is_uniform_zero(const char* input_name, const char* multiplier_name) const;

    bool check_uniform(const char* input_name) const;

    void check_non_zero_emission(const Source* source) const;
    void check_non_zero_emission(const char* input_name) const;

    void check_non_zero_emission(const Source* source, const Source* multiplier) const;
    void check_non_zero_emission(const char* input_name, const char* multiplier_name) const;

    void warn_zero_emission() const;
};


//
// ConnectableEntity class implementation.
//

inline ConnectableEntity::ConnectableEntity(
    const foundation::UniqueID      class_uid)
  : Entity(class_uid)
{
}

inline ConnectableEntity::ConnectableEntity(
    const foundation::UniqueID      class_uid,
    const ParamArray&               params)
  : Entity(class_uid, params)
{
}

inline InputArray& ConnectableEntity::get_inputs()
{
    return m_inputs;
}

inline const InputArray& ConnectableEntity::get_inputs() const
{
    return m_inputs;
}

}   // namespace renderer
