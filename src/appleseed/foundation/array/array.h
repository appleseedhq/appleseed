
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
#include "foundation/array/arrayallocator.h"
#include "foundation/array/arraytraits.h"
#include "foundation/memory/alignedallocator.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/test.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <algorithm>
#include <cstddef>
#include <vector>

DECLARE_TEST_CASE(Foundation_Array_Array, MoveConstruct);

namespace foundation
{

//
// An array that can hold items of a predetermined set of types.
//

class APPLESEED_DLLSYMBOL Array
{
  public:
    // Constructor
    Array();

    // Constructor.
    explicit Array(const ArrayType type, const size_t size = 0);

    // Destructor.
    ~Array();

    // Copy constructor
    Array(const Array& rhs);

    // Move constructor.
    Array(Array&& rhs) APPLESEED_NOEXCEPT;

    // Assignment.
    Array& operator=(const Array& rhs);

    // Move assignment.
    Array& operator=(Array&& rhs) APPLESEED_NOEXCEPT;

    // Return the type of the array elements.
    ArrayType type() const;

    // Return true if the array is empty.
    bool empty() const;

    // Return the number of items in the array.
    size_t size() const;

    // Return the size in bytes of an item in the array.
    size_t item_size() const;

    // Return the capacity of the array.
    size_t capacity() const;

    // Remove all items from the array.
    void clear();

    // Reserve memory for n items.
    void reserve(const size_t n);

    // Resize the array to n items.
    void resize(const size_t n);

    // Return a pointer to the elements.
    const void* data() const;
    void* data();

    // Remove excess capacity from the array.
    void shrink_to_fit();

    // Equality and inequality tests.
    bool operator==(const Array& rhs) const;
    bool operator!=(const Array& rhs) const;

  private:
    template <typename T>
    friend class ArrayRef;

    template <typename T>
    friend class ArrayView;

    GRANT_ACCESS_TO_TEST_CASE(Foundation_Array_Array, MoveConstruct);

    bool is_moved() const;

    const void* begin() const;
    const void* end() const;

    void* begin();
    void* end();

    void push_back(const void* p);

    struct Concept
    {
        virtual ~Concept() = default;
        virtual Concept* copy() const = 0;

        virtual ArrayType type() const = 0;

        virtual size_t item_size() const = 0;

        virtual bool empty() const = 0;
        virtual size_t size() const = 0;
        virtual size_t capacity() const = 0;

        virtual void clear() = 0;

        virtual void reserve(const size_t n) = 0;
        virtual void resize(const size_t n) = 0;
        virtual void shrink_to_fit() = 0;

        virtual void push_back(const void* p) = 0;

        virtual const void* begin() const = 0;
        virtual const void* end() const = 0;

        virtual void* begin() = 0;
        virtual void* end() = 0;

        virtual bool equals(const Concept* rhs) const = 0;
    };

    template <typename T>
    struct Model
      : public Concept
    {
        Model() = default;

        Model(const Model& rhs) = default;

        Concept* copy() const override;

        ArrayType type() const override;

        size_t item_size() const override;

        bool empty() const override;

        size_t size() const override;

        size_t capacity() const override;

        void clear() override;

        void reserve(const size_t n) override;

        void resize(const size_t n) override;

        void shrink_to_fit() override;

        void push_back(const void* p) override;

        const void* begin() const override;
        const void* end() const override;

        void* begin() override;
        void* end() override;

        bool equals(const Concept* rhs) const override;

        std::vector<T, ArrayAllocator<T>> m_items;
    };

    Concept* m_self;
};

}   // namespace foundation
