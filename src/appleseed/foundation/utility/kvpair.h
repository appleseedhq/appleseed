
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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

#ifndef APPLESEED_FOUNDATION_UTILITY_KVPAIR_H
#define APPLESEED_FOUNDATION_UTILITY_KVPAIR_H

// appleseed.foundation headers.
#include "foundation/utility/countof.h"

// Standard headers.
#include <cstddef>

//
// On Windows, define FOUNDATIONDLL to __declspec(dllexport) when building the DLL
// and to __declspec(dllimport) when building an application using the DLL.
// Other platforms don't use this export mechanism and the symbol FOUNDATIONDLL is
// defined to evaluate to nothing.
//

#ifndef FOUNDATIONDLL
#ifdef _WIN32
#ifdef APPLESEED_FOUNDATION_EXPORTS
#define FOUNDATIONDLL __declspec(dllexport)
#else
#define FOUNDATIONDLL __declspec(dllimport)
#endif
#else
#define FOUNDATIONDLL
#endif
#endif

namespace foundation
{

//
// Utilities to create and lookup arrays of (key, value) pairs.
//

template <typename KeyType, typename ValueType>
struct FOUNDATIONDLL KeyValuePair
{
    const KeyType   m_key;
    const ValueType m_value;
};

template <typename KeyValuePairType, typename LookupKeyType>
const KeyValuePairType* lookup_kvpair_array(
    const KeyValuePairType  kvpairs[],
    const size_t            size,
    const LookupKeyType     key)
{
    for (size_t i = 0; i < size; ++i)
    {
        if (kvpairs[i].m_key == key)
            return &kvpairs[i];
    }

    return 0;
}

#define FOUNDATION_LOOKUP_KVPAIR_ARRAY(kvpairs, key) \
    foundation::lookup_kvpair_array(kvpairs, COUNT_OF(kvpairs), key)

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_KVPAIR_H
