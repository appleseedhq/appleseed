
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

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/utility/api/apiarray.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

namespace foundation
{

//
// Predefined array types.
//

APPLESEED_DECLARE_APIARRAY(FloatArray, float);
APPLESEED_DECLARE_APIARRAY(DoubleArray, double);
APPLESEED_DECLARE_APIARRAY(DictionaryArray, Dictionary);


//
// An array of strings that can be passed safely across DLL boundaries.
//
// The interface and implementation of this class both differ slightly
// from what APPLESEED_DECLARE_APIARRAY and APPLESEED_DEFINE_APIARRAY offer.
//

class APPLESEED_DLLSYMBOL StringArray
{
  public:
    // Types.
    typedef const char* value_type;
    typedef size_t size_type;

    // Constructors.
    StringArray();
    StringArray(const StringArray& rhs);
    StringArray(
        const size_type     size,
        const value_type*   values);

    // Destructor.
    ~StringArray();

    // Assignment operator.
    StringArray& operator=(const StringArray& rhs);

    // Returns the size of the vector.
    size_type size() const;

    // Tests if the vector is empty.
    bool empty() const;

    // Clears the vector.
    void clear();

    // Reserves memory for a given number of elements.
    void reserve(const size_type count);

    // Specifies a new size for a vector.
    void resize(const size_type new_size);

    // Adds an element to the end of the vector.
    void push_back(const value_type val);

    // Set the vector element at a specified position.
    void set(const size_type pos, const value_type val);

    // Returns the vector element at a specified position.
    value_type operator[](const size_type pos) const;

  private:
    struct Impl;
    Impl* impl;
};

}   // namespace foundation
