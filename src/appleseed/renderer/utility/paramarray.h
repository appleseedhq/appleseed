
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
#include "renderer/global/globallogger.h"
#include "renderer/utility/messagecontext.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/string/string.h"
#include "foundation/utility/foreach.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cassert>
#include <string>
#include <vector>

namespace renderer
{

//
// A collection of parameters.
//

class APPLESEED_DLLSYMBOL ParamArray
  : public foundation::Dictionary
{
  public:
    typedef std::vector<std::string> StringVec;

    // Constructors.
    ParamArray();
    ParamArray(const ParamArray& rhs);
    ParamArray(const foundation::Dictionary& dictionary);

    // Assignment operator.
    ParamArray& operator=(const ParamArray& rhs);

    // Insert an item into the dictionary.
    template <typename T> ParamArray& insert(const char* key, const T& value);

    //
    // Retrieve the value of a required parameter.
    //
    // If the parameter is missing, an error message is emitted and the default
    // value is returned.
    //
    // If the parameter is present but its value is invalid, an error message
    // is emitted and the default value is returned.
    //
    // If the parameter is present and a list of allowed values is given, but
    // the value of the parameter does not appear to be allowed, an error
    // message is emitted and the default value is returned.
    //

    template <typename T>
    T get_required(
        const char*             name,
        const T&                default_value,
        const StringVec&        allowed_values,
        const MessageContext&   message_context) const;

    template <typename T>
    T get_required(
        const char*             name,
        const T&                default_value,
        const StringVec&        allowed_values) const;

    template <typename T>
    T get_required(
        const char*             name,
        const T&                default_value,
        const MessageContext&   message_context) const;

    template <typename T>
    T get_required(
        const char*             name,
        const T&                default_value = T()) const;

    //
    // Retrieve the value of an optional parameter.
    //
    // If the parameter is missing, the default value is silently returned.
    //
    // If the parameter is present but its value is invalid, an error message
    // is emitted and the default value is returned.
    //
    // If the parameter is present and a list of allowed values is given, but
    // the value of the parameter does not appear to be allowed, an error
    // message is emitted and the default value is returned.
    //

    template <typename T>
    T get_optional(
        const char*             name,
        const T&                default_value,
        const StringVec&        allowed_values,
        const MessageContext&   message_context) const;

    template <typename T>
    T get_optional(
        const char*             name,
        const T&                default_value,
        const StringVec&        allowed_values) const;

    template <typename T>
    T get_optional(
        const char*             name,
        const T&                default_value,
        const MessageContext&   message_context) const;

    template <typename T>
    T get_optional(
        const char*             name,
        const T&                default_value = T()) const;

    //
    // Insert or set an item with a given path, creating branches as needed.
    //
    // For instance, insert_path("a.b.c", 12) will insert the value 12 with key 'c'
    // inside a dictionary named 'b' itself contained inside a dictionary named 'a'.
    //

    ParamArray& insert_path(const char* path, const char* value);
    template <typename T> ParamArray& insert_path(const char* path, const T& value);

    // Return true if a parameter with a given path exists.
    bool exist_path(const char* path) const;

    // Retrieve a parameter at a given path.
    const char* get_path(const char* path) const;

    // Like get_required() but given a path instead of a key.
    template <typename T>
    T get_path_required(
        const char*             path,
        const T&                default_value,
        const StringVec&        allowed_values,
        const MessageContext&   message_context) const;
    template <typename T>
    T get_path_required(
        const char*             path,
        const T&                default_value,
        const StringVec&        allowed_values) const;
    template <typename T>
    T get_path_required(
        const char*             path,
        const T&                default_value) const;

    // Like get_optional() but given a path instead of a key.
    template <typename T>
    T get_path_optional(
        const char*             path,
        const T&                default_value,
        const StringVec&        allowed_values,
        const MessageContext&   message_context) const;
    template <typename T>
    T get_path_optional(
        const char*             path,
        const T&                default_value,
        const StringVec&        allowed_values) const;
    template <typename T>
    T get_path_optional(
        const char*             path,
        const T&                default_value = T()) const;

    // Remove a parameter at a given path, if it exists.
    ParamArray& remove_path(const char* path);

    // Return a child set of parameters, or create it if it doesn't exist.
    ParamArray& push(const char* name);

    // Retrieve a child set of parameters.
    // Returns an empty parameter set if the specified set cannot be found.
    const ParamArray& child(const char* name) const;

  private:
    template <typename T>
    T get_helper(
        const char*             name,
        const bool              is_path,
        const bool              is_required,
        const T&                default_value,
        const StringVec&        allowed_values,
        const MessageContext&   message_context) const;

    template <typename T>
    T get_helper(
        const char*             name,
        const bool              is_path,
        const bool              is_required,
        const T&                default_value,
        const StringVec&        allowed_values) const;

    template <typename T>
    T get_helper(
        const char*             name,
        const bool              is_path,
        const bool              is_required,
        const T&                default_value,
        const MessageContext&   message_context) const;

    template <typename T>
    T get_helper(
        const char*             name,
        const bool              is_path,
        const bool              is_required,
        const T&                default_value) const;

    template <typename T>
    static bool contains(
        const StringVec&        allowed_values,
        const T&                value);
};


//
// ParamArray class implementation.
//

template <typename T>
inline ParamArray& ParamArray::insert(const char* key, const T& value)
{
    foundation::Dictionary::insert(key, value);
    return *this;
}

template <>
inline ParamArray& ParamArray::insert(const char* key, const ParamArray& value)
{
    dictionaries().insert(key, value);
    return *this;
}

template <typename T>
inline T ParamArray::get_required(
    const char*                 name,
    const T&                    default_value,
    const StringVec&            allowed_values,
    const MessageContext&       message_context) const
{
    return get_helper(name, false, true, default_value, allowed_values, message_context);
}

template <typename T>
inline T ParamArray::get_required(
    const char*                 name,
    const T&                    default_value,
    const StringVec&            allowed_values) const
{
    return get_helper(name, false, true, default_value, allowed_values);
}

template <typename T>
inline T ParamArray::get_required(
    const char*                 name,
    const T&                    default_value,
    const MessageContext&       message_context) const
{
    return get_helper(name, false, true, default_value, message_context);
}

template <typename T>
inline T ParamArray::get_required(
    const char*                 name,
    const T&                    default_value) const
{
    return get_helper(name, false, true, default_value);
}

template <typename T>
inline T ParamArray::get_optional(
    const char*                 name,
    const T&                    default_value,
    const StringVec&            allowed_values,
    const MessageContext&       message_context) const
{
    return get_helper(name, false, false, default_value, allowed_values, message_context);
}

template <typename T>
inline T ParamArray::get_optional(
    const char*                 name,
    const T&                    default_value,
    const StringVec&            allowed_values) const
{
    return get_helper(name, false, false, default_value, allowed_values);
}

template <typename T>
inline T ParamArray::get_optional(
    const char*                 name,
    const T&                    default_value,
    const MessageContext&       message_context) const
{
    return get_helper(name, false, false, default_value, message_context);
}

template <typename T>
inline T ParamArray::get_optional(
    const char*                 name,
    const T&                    default_value) const
{
    return get_helper(name, false, false, default_value);
}

template <typename T>
inline ParamArray& ParamArray::insert_path(const char* path, const T& value)
{
    return insert_path(path, foundation::to_string(value).c_str());
}

template <typename T>
inline T ParamArray::get_path_required(
    const char*                 path,
    const T&                    default_value,
    const StringVec&            allowed_values,
    const MessageContext&       message_context) const
{
    return get_helper(path, true, true, default_value, allowed_values, message_context);
}

template <typename T>
inline T ParamArray::get_path_required(
    const char*                 path,
    const T&                    default_value,
    const StringVec&            allowed_values) const
{
    return get_helper(path, true, true, default_value, allowed_values);
}

template <typename T>
inline T ParamArray::get_path_required(
    const char*                 path,
    const T&                    default_value) const
{
    return get_helper(path, true, true, default_value);
}

template <typename T>
inline T ParamArray::get_path_optional(
    const char*                 path,
    const T&                    default_value,
    const StringVec&            allowed_values,
    const MessageContext&       message_context) const
{
    return get_helper(path, true, false, default_value, allowed_values, message_context);
}

template <typename T>
inline T ParamArray::get_path_optional(
    const char*                 path,
    const T&                    default_value,
    const StringVec&            allowed_values) const
{
    return get_helper(path, true, false, default_value, allowed_values);
}

template <typename T>
inline T ParamArray::get_path_optional(
    const char*                 path,
    const T&                    default_value) const
{
    return get_helper(path, true, false, default_value);
}

template <typename T>
T ParamArray::get_helper(
    const char*                 name,
    const bool                  is_path,
    const bool                  is_required,
    const T&                    default_value,
    const StringVec&            allowed_values,
    const MessageContext&       message_context) const
{
    assert(name);

    try
    {
        if (is_path ? exist_path(name) : strings().exist(name))
        {
            const T value = is_path ? foundation::from_string<T>(get_path(name)) : get<T>(name);

            if (allowed_values.empty() || contains(allowed_values, value))
                return value;
        }
        else
        {
            if (is_required)
            {
                RENDERER_LOG_ERROR(
                    "%srequired parameter \"%s\" not found; continuing using value \"%s\".",
                    message_context.get(),
                    name,
                    foundation::to_string(default_value).c_str());
            }

            return default_value;
        }
    }
    catch (const foundation::ExceptionStringConversionError&)
    {
    }

    RENDERER_LOG_ERROR(
        "%sinvalid value \"%s\" for parameter \"%s\"; continuing using value \"%s\".",
        message_context.get(),
        is_path ? get_path(name) : get(name),
        name,
        foundation::to_string(default_value).c_str());

    return default_value;
}

template <typename T>
T ParamArray::get_helper(
    const char*                 name,
    const bool                  is_path,
    const bool                  is_required,
    const T&                    default_value,
    const StringVec&            allowed_values) const
{
    assert(name);

    try
    {
        if (is_path ? exist_path(name) : strings().exist(name))
        {
            const T value = is_path ? foundation::from_string<T>(get_path(name)) : get<T>(name);

            if (allowed_values.empty() || contains(allowed_values, value))
                return value;
        }
        else
        {
            if (is_required)
            {
                RENDERER_LOG_ERROR(
                    "required parameter \"%s\" not found; continuing using value \"%s\".",
                    name,
                    foundation::to_string(default_value).c_str());
            }

            return default_value;
        }
    }
    catch (const foundation::ExceptionStringConversionError&)
    {
    }

    RENDERER_LOG_ERROR(
        "invalid value \"%s\" for parameter \"%s\"; continuing using value \"%s\".",
        is_path ? get_path(name) : get(name),
        name,
        foundation::to_string(default_value).c_str());

    return default_value;
}

template <typename T>
T ParamArray::get_helper(
    const char*                 name,
    const bool                  is_path,
    const bool                  is_required,
    const T&                    default_value,
    const MessageContext&       message_context) const
{
    assert(name);

    try
    {
        if (is_path ? exist_path(name) : strings().exist(name))
        {
            return is_path ? foundation::from_string<T>(get_path(name)) : get<T>(name);
        }
        else
        {
            if (is_required)
            {
                RENDERER_LOG_ERROR(
                    "%srequired parameter \"%s\" not found; continuing using value \"%s\".",
                    message_context.get(),
                    name,
                    foundation::to_string(default_value).c_str());
            }

            return default_value;
        }
    }
    catch (const foundation::ExceptionStringConversionError&)
    {
    }

    RENDERER_LOG_ERROR(
        "%sinvalid value \"%s\" for parameter \"%s\"; continuing using value \"%s\".",
        message_context.get(),
        is_path ? get_path(name) : get(name),
        name,
        foundation::to_string(default_value).c_str());

    return default_value;
}

template <typename T>
T ParamArray::get_helper(
    const char*                 name,
    const bool                  is_path,
    const bool                  is_required,
    const T&                    default_value) const
{
    assert(name);

    try
    {
        if (is_path ? exist_path(name) : strings().exist(name))
        {
            return is_path ? foundation::from_string<T>(get_path(name)) : get<T>(name);
        }
        else
        {
            if (is_required)
            {
                RENDERER_LOG_ERROR(
                    "required parameter \"%s\" not found; continuing using value \"%s\".",
                    name,
                    foundation::to_string(default_value).c_str());
            }

            return default_value;
        }
    }
    catch (const foundation::ExceptionStringConversionError&)
    {
    }

    RENDERER_LOG_ERROR(
        "invalid value \"%s\" for parameter \"%s\"; continuing using value \"%s\".",
        is_path ? get_path(name) : get(name),
        name,
        foundation::to_string(default_value).c_str());

    return default_value;
}

template <typename T>
bool ParamArray::contains(
    const StringVec&            allowed_values,
    const T&                    value)
{
    for (foundation::const_each<StringVec> i = allowed_values; i; ++i)
    {
        if (value == foundation::from_string<T>(*i))
            return true;
    }

    return false;
}

}   // namespace renderer
