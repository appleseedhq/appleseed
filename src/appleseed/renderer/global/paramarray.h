
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

#ifndef APPLESEED_RENDERER_GLOBAL_PARAMARRAY_H
#define APPLESEED_RENDERER_GLOBAL_PARAMARRAY_H

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"

// appleseed.foundation headers.
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <cassert>
#include <string>

namespace renderer
{

//
// A collection of parameters.
//

class RENDERERDLL ParamArray
  : public foundation::Dictionary
{
  public:
    // Insert an item into the dictionary.
    template <typename T> void insert(const char* key, const T& value);
    void insert_path(const char* path, const char* value);
    template <typename T> void insert_path(const char* path, const T& value);
    template <typename T> void insert_path(const std::string& path, const T& value);

    //
    // Retrieve the value of a required parameter.
    //
    // If the parameter is present but its value is invalid, an error message
    // is emitted and the default value is returned.
    //
    // If the parameter is missing, an error message is emitted and the default
    // value is returned.
    //

    template <typename T>
    T get_required(const char* name, const T& default_value) const;

    //
    // Retrieve the value of an optional parameter.
    //
    // If the parameter is present but its value is invalid, an error message
    // is emitted and the default value is returned.
    //
    // If the parameter is missing, the default value is silently returned.
    //

    template <typename T>
    T get_optional(const char* name, const T& default_value) const;

    // Return a child set of parameters, or create it if it doesn't exist.
    ParamArray& push(const char* name);

    // Retrieve a child set of parameters.
    // Returns an empty parameter set if the specified set cannot be found.
    const ParamArray& child(const char* name) const;

    // Merge another set of parameters into this one.
    void merge(const ParamArray& rhs);

  private:
    template <typename T>
    T get_helper(
        const char*     name,
        const T&        default_value,
        const bool      required) const;
};


//
// ParamArray class implementation.
//

template <typename T>
inline void ParamArray::insert(const char* key, const T& value)
{
    Dictionary::insert(key, value);
}

template <>
inline void ParamArray::insert(const char* key, const ParamArray& value)
{
    dictionaries().insert(key, value);
}

template <typename T>
inline void ParamArray::insert_path(const char* path, const T& value)
{
    insert_path(path, foundation::to_string(value).c_str());
}

template <typename T>
inline void ParamArray::insert_path(const std::string& path, const T& value)
{
    insert_path(path.c_str(), value);
}

template <typename T>
inline T ParamArray::get_required(const char* name, const T& default_value) const
{
    return get_helper(name, default_value, true);
}

template <typename T>
inline T ParamArray::get_optional(const char* name, const T& default_value) const
{
    return get_helper(name, default_value, false);
}

template <typename T>
T ParamArray::get_helper(
    const char*     name,
    const T&        default_value,
    const bool      required) const
{
    assert(name);

    try
    {
        return get<T>(name);
    }
    catch (const foundation::ExceptionDictionaryItemNotFound&)
    {
        if (required)
        {
            RENDERER_LOG_ERROR(
                "parameter \"%s\" not found, using default value \"%s\"",
                name,
                foundation::to_string(default_value).c_str());
        }
    }
    catch (const foundation::ExceptionStringConversionError&)
    {
        RENDERER_LOG_ERROR(
            "invalid value \"%s\" for parameter \"%s\", using default value \"%s\"",
            get(name),
            name,
            foundation::to_string(default_value).c_str());
    }

    return default_value;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_GLOBAL_PARAMARRAY_H
