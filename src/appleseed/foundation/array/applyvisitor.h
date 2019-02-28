
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
#include "foundation/array/array.h"
#include "foundation/array/arrayref.h"
#include "foundation/array/arrayview.h"
#include "foundation/image/color.h"
#include "foundation/math/compressedunitvector.h"
#include "foundation/math/vector.h"
#include "foundation/platform/types.h"
#include "foundation/utility/otherwise.h"

// Standard headers.
#include <utility>

namespace foundation
{
namespace detail
{

template <typename T, typename Visitor>
void apply_visitor(Array& array, Visitor&& v)
{
    ArrayRef<T> array_ref(array);
    v(array_ref);
}

template <typename T, typename Visitor>
void apply_visitor(const Array& array, Visitor&& v)
{
    v(ArrayView<T>(array));
}

#define APPLESEED_ARRAY_APPLY_VISITOR_CASE(type_enum, type_name) \
    case type_enum: \
        detail::apply_visitor<type_name>(array, std::forward<Visitor>(v)); \
        break;

}       // namespace detail

template <typename Visitor>
void apply_visitor(Array& array, Visitor&& v)
{
    switch (array.type())
    {
      APPLESEED_ARRAY_APPLY_VISITOR_CASE(UInt8Type, uint8)
      APPLESEED_ARRAY_APPLY_VISITOR_CASE(UInt16Type, uint16)
      APPLESEED_ARRAY_APPLY_VISITOR_CASE(UInt32Type, uint32)
      APPLESEED_ARRAY_APPLY_VISITOR_CASE(FloatType, float)
      APPLESEED_ARRAY_APPLY_VISITOR_CASE(Vector2fType, foundation::Vector2f)
      APPLESEED_ARRAY_APPLY_VISITOR_CASE(Vector3fType, foundation::Vector3f)
      APPLESEED_ARRAY_APPLY_VISITOR_CASE(CompressedUnitVectorType, foundation::CompressedUnitVector)
      APPLESEED_ARRAY_APPLY_VISITOR_CASE(Color3fType, foundation::Color3f)

      assert_otherwise;
    }
}

template <typename Visitor>
void apply_visitor(const Array& array, Visitor&& v)
{
    switch (array.type())
    {
      APPLESEED_ARRAY_APPLY_VISITOR_CASE(UInt8Type, uint8)
      APPLESEED_ARRAY_APPLY_VISITOR_CASE(UInt16Type, uint16)
      APPLESEED_ARRAY_APPLY_VISITOR_CASE(UInt32Type, uint32)
      APPLESEED_ARRAY_APPLY_VISITOR_CASE(FloatType, float)
      APPLESEED_ARRAY_APPLY_VISITOR_CASE(Vector2fType, foundation::Vector2f)
      APPLESEED_ARRAY_APPLY_VISITOR_CASE(Vector3fType, foundation::Vector3f)
      APPLESEED_ARRAY_APPLY_VISITOR_CASE(CompressedUnitVectorType, foundation::CompressedUnitVector)
      APPLESEED_ARRAY_APPLY_VISITOR_CASE(Color3fType, foundation::Color3f)

      assert_otherwise;
    }
}

#undef APPLESEED_ARRAY_APPLY_VISITOR_CASE

}       // namespace foundation
