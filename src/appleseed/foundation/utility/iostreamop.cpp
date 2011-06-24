
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "iostreamop.h"

// appleseed.foundation headers.
#include "foundation/utility/containers/specializedarrays.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <string>

using namespace std;

namespace foundation
{

namespace
{
    template <typename ArrayType>
    istream& read_array(istream& s, ArrayType& array)
    {
        string token;
        s >> token;

        while (!token.empty())
        {
            array.push_back(from_string<typename ArrayType::value_type>(token));
            token.clear();
            s >> token;
        }

        return s;
    }
}

ostream& operator<<(ostream& s, const FloatArray& array)
{
    return iostreamop_impl::write_sequence(s, array, array.size());
}

istream& operator>>(istream& s, FloatArray& array)
{
    return read_array(s, array);
}

ostream& operator<<(ostream& s, const DoubleArray& array)
{
    return iostreamop_impl::write_sequence(s, array, array.size());
}

istream& operator>>(istream& s, DoubleArray& array)
{
    return read_array(s, array);
}

}   // namespace foundation
