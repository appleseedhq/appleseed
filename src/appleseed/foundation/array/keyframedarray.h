
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

#pragma once

// appleseed.foundation headers.
#include "foundation/array/array.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/test.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

DECLARE_TEST_CASE(Foundation_Array_KeyframedArray, MoveConstruct);

namespace foundation
{

//
// A time varying array that can hold items of a predetermined set of types.
//

class APPLESEED_DLLSYMBOL KeyFramedArray
{
  public:
    // Constructor.
    KeyFramedArray();

    // Constructor.
    explicit KeyFramedArray(const ArrayType type, const size_t size = 0, const size_t num_keys = 1);

    // Destructor.
    ~KeyFramedArray();

    // Copy constructor
    KeyFramedArray(const KeyFramedArray& rhs);

    // Move constructor.
    KeyFramedArray(KeyFramedArray&& rhs) APPLESEED_NOEXCEPT;

    // Assignment.
    KeyFramedArray& operator=(const KeyFramedArray& rhs);

    // Move assignment.
    KeyFramedArray& operator=(KeyFramedArray&& rhs) APPLESEED_NOEXCEPT;

    // Return the type of the array elements.
    ArrayType type() const;

    // Resize the array. The previous contents of the array is lost.
    void resize(const size_t size, const size_t keys = 1);

    // Const iterators.
    const Array* begin() const;
    const Array* end() const;

    // Iterators.
    Array* begin();
    Array* end();

    // Get the number of keys.
    size_t get_key_count() const;

    // Set the number of keys.
    void set_key_count(const size_t keys);

    // Get the ith keyframe.
    const Array& get_key(const size_t i) const;
    Array& get_key(const size_t i);

    // Remove excess capacity from the arrays.
    void shrink_to_fit();

    // Check that all arrays have the same type and size.
    bool check_consistency() const;

    // Return true if all keyframes are equal.
    bool all_keyframes_equal() const;

    // Equality and inequality tests.
    bool operator==(const KeyFramedArray& rhs) const;
    bool operator!=(const KeyFramedArray& rhs) const;

  private:
    Array* m_keys;
    size_t m_key_count;

    KeyFramedArray(Array&& first_key, const size_t num_keys);

    GRANT_ACCESS_TO_TEST_CASE(Foundation_Array_KeyframedArray, MoveConstruct);

    bool is_moved() const;
};

}       // namespace foundation
