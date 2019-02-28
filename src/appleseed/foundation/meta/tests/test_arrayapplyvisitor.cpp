
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

// appleseed.foundation headers.
#include "foundation/array/applyvisitor.h"
#include "foundation/array/array.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <algorithm>
#include <vector>

using namespace foundation;
using namespace std;

TEST_SUITE(Foundation_Array_ApplyVisitor)
{
    class CountAppliesVisitor
    {
      public:
        CountAppliesVisitor()
          : m_applies(ArrayType::ArrayTypeCount, 0)
        {
        }

        template <typename ArrayRefType>
        void operator()(ArrayRefType ref)
        {
            typedef typename ArrayRefType::value_type T;

            const size_t type_index = static_cast<size_t>(ArrayTraits<T>::array_type());
            m_applies[type_index]++;
        }

        bool all_types_applied() const
        {
            return std::all_of(
                m_applies.begin(),
                m_applies.end(),
                [](size_t x) { return x == 1; });
        }

      private:
        vector<size_t> m_applies;
    };

    TEST_CASE(ApplyConst)
    {
        CountAppliesVisitor v;

        for (int i = 0; i < ArrayType::ArrayTypeCount; ++i)
        {
            const Array array(static_cast<ArrayType>(i));
            apply_visitor(array, v);
        }

        EXPECT_TRUE(v.all_types_applied());
    }

    TEST_CASE(ApplyNonConst)
    {
        CountAppliesVisitor v;

        for (int i = 0; i < ArrayType::ArrayTypeCount; ++i)
        {
            Array array(static_cast<ArrayType>(i));
            apply_visitor(array, v);
        }

        EXPECT_TRUE(v.all_types_applied());
    }
}
