
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
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/platform/types.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstdint>

namespace foundation
{

//
// A unique identifier.
//

typedef std::uint64_t UniqueID;


//
// Format string for printing unique identifiers to strings.
//

#define FMT_UNIQUE_ID FMT_UINT64


//
// Base class for uniquely identifiable objects.
//

class APPLESEED_DLLSYMBOL Identifiable
  : public NonCopyable
{
  public:
    // Constructor, assigns a unique ID to this instance.
    Identifiable();

    // Return the unique ID of this object.
    UniqueID get_uid() const;

  private:
    const UniqueID m_unique_id;
};


//
// This function generates identifiers that are unique within the process.
//
// Thread-safe.
//

APPLESEED_DLLSYMBOL UniqueID new_guid();


//
// Identifiable class implementation.
//

inline Identifiable::Identifiable()
  : m_unique_id(new_guid())
{
}

inline UniqueID Identifiable::get_uid() const
{
    return m_unique_id;
}

}   // namespace foundation
