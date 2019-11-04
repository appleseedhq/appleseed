
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Esteban Tovagliari, The appleseedhq Organization
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

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/math/compressedunitvector.h"
#include "foundation/math/vector.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstdint>

namespace foundation
{

enum ArrayType
{
    UInt8Type = 0,
    UInt16Type,
    UInt32Type,
    FloatType,
    Vector2fType,
    Vector3fType,
    CompressedUnitVectorType,
    Color3fType,
    ArrayTypeCount,
    InvalidArrayType = ArrayTypeCount
};

APPLESEED_DLLSYMBOL const char* array_type_to_string(const ArrayType type);
APPLESEED_DLLSYMBOL ArrayType string_to_array_type(const char* str);

template <typename T>
struct ArrayTraits
{
};

#define APPLESEED_ARRAY_TYPE_TRAITS_SPECIALIZATION(type_enum, type_name) \
    template <> \
    struct ArrayTraits<type_name> \
    { \
        static ArrayType array_type() {return type_enum;} \
    }

APPLESEED_ARRAY_TYPE_TRAITS_SPECIALIZATION(UInt8Type, std::uint8_t);
APPLESEED_ARRAY_TYPE_TRAITS_SPECIALIZATION(UInt16Type, std::uint16_t);
APPLESEED_ARRAY_TYPE_TRAITS_SPECIALIZATION(UInt32Type, std::uint32_t);
APPLESEED_ARRAY_TYPE_TRAITS_SPECIALIZATION(FloatType, float);
APPLESEED_ARRAY_TYPE_TRAITS_SPECIALIZATION(Vector2fType, Vector2f);
APPLESEED_ARRAY_TYPE_TRAITS_SPECIALIZATION(Vector3fType, Vector3f);
APPLESEED_ARRAY_TYPE_TRAITS_SPECIALIZATION(CompressedUnitVectorType, CompressedUnitVector);
APPLESEED_ARRAY_TYPE_TRAITS_SPECIALIZATION(Color3fType, Color3f);

#undef APPLESEED_ARRAY_TYPE_TRAITS_SPECIALIZATION

}       // namespace foundation
