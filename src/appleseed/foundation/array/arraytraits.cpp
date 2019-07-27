
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Esteban Tovagliari, The appleseedhq Organization
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
#include "arraytraits.h"

// Standard headers.
#include <cstring>

namespace foundation
{

const char* array_type_to_string(const ArrayType type)
{
    switch (type)
    {
        case UInt8Type: return "uint8";
        case UInt16Type: return "uint16";
        case UInt32Type: return "uint32";
        case FloatType: return "float";
        case Vector2fType: return "vector2f";
        case Vector3fType: return "vector3f";
        case CompressedUnitVectorType: return "unit_vector";
        case Color3fType: return "color3f";
        default: return nullptr;
    }
}

ArrayType string_to_array_type(const char* str)
{
    if (std::strcmp(str, "uint8") == 0)
        return UInt8Type;
    else if (std::strcmp(str, "uint16") == 0)
        return UInt16Type;
    else if (std::strcmp(str, "uint32") == 0)
        return UInt32Type;
    else if (std::strcmp(str, "float") == 0)
        return FloatType;
    else if (std::strcmp(str, "vector2f") == 0)
        return Vector2fType;
    else if (std::strcmp(str, "vector3f") == 0)
        return Vector3fType;
    else if (std::strcmp(str, "unit_vector") == 0)
        return CompressedUnitVectorType;
    else if (std::strcmp(str, "color3f") == 0)
        return Color3fType;

    return InvalidArrayType;
}

}   // namespace foundation
