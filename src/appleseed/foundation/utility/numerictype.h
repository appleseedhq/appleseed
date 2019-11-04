
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

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstdint>

namespace foundation
{

//
// Enumeration of supported numeric types.
//

enum NumericTypeID
{
    NumericTypeInt8,            // 8-bit signed integer
    NumericTypeInt16,           // 16-bit signed integer
    NumericTypeInt32,           // 32-bit signed integer
    NumericTypeInt64,           // 64-bit signed integer
    NumericTypeUInt8,           // 8-bit unsigned integer
    NumericTypeUInt16,          // 16-bit unsigned integer
    NumericTypeUInt32,          // 32-bit unsigned integer
    NumericTypeUInt64,          // 64-bit unsigned integer
    NumericTypeFloat,           // 32-bit float
    NumericTypeDouble           // 64-bit double
};


//
// The Type class provides a type to type ID mapping and defines
// various operations on types (e.g.  retrieving the size or the
// name of a type, given its type ID).
//

class NumericType
{
  public:
    // Return the ID of the type T. Attempting to get the ID of
    // an unsupported type will result in a compilation error.
    template <typename T> static NumericTypeID id();

    // Return the size in byte of a numeric type.
    // Return 0 if ID is not a valid numeric type.
    static size_t size(const NumericTypeID id);

    // Return the name of a type.
    // Return an empty string if ID is not a valid type.
    static const char* name(const NumericTypeID id);
};


//
// NumericType class implementation.
//

// Return the ID of the type T.
template <> inline NumericTypeID NumericType::id<std::int8_t>()     { return NumericTypeInt8;   }
template <> inline NumericTypeID NumericType::id<std::int16_t>()    { return NumericTypeInt16;  }
template <> inline NumericTypeID NumericType::id<std::int32_t>()    { return NumericTypeInt32;  }
template <> inline NumericTypeID NumericType::id<std::int64_t>()    { return NumericTypeInt64;  }
template <> inline NumericTypeID NumericType::id<std::uint8_t>()    { return NumericTypeUInt8;  }
template <> inline NumericTypeID NumericType::id<std::uint16_t>()   { return NumericTypeUInt16; }
template <> inline NumericTypeID NumericType::id<std::uint32_t>()   { return NumericTypeUInt32; }
template <> inline NumericTypeID NumericType::id<std::uint64_t>()   { return NumericTypeUInt64; }
template <> inline NumericTypeID NumericType::id<float>()           { return NumericTypeFloat;  }
template <> inline NumericTypeID NumericType::id<double>()          { return NumericTypeDouble; }

// Return the size in byte of a numeric type.
inline size_t NumericType::size(const NumericTypeID id)
{
    switch (id)
    {
      case NumericTypeInt8:    return 1;
      case NumericTypeInt16:   return 2;
      case NumericTypeInt32:   return 4;
      case NumericTypeInt64:   return 8;
      case NumericTypeUInt8:   return 1;
      case NumericTypeUInt16:  return 2;
      case NumericTypeUInt32:  return 4;
      case NumericTypeUInt64:  return 8;
      case NumericTypeFloat:   return 4;
      case NumericTypeDouble:  return 8;
      default:
        assert(false);
        return 0;
    }
}

// Return the name of a type.
inline const char* NumericType::name(const NumericTypeID id)
{
    switch (id)
    {
      case NumericTypeInt8:    return "int8";
      case NumericTypeInt16:   return "int16";
      case NumericTypeInt32:   return "int32_t";
      case NumericTypeInt64:   return "int64";
      case NumericTypeUInt8:   return "uint8";
      case NumericTypeUInt16:  return "uint16";
      case NumericTypeUInt32:  return "uint32";
      case NumericTypeUInt64:  return "uint64";
      case NumericTypeFloat:   return "float";
      case NumericTypeDouble:  return "double";
      default:
        assert(false);
        return "";
    }
}

}   // namespace foundation
