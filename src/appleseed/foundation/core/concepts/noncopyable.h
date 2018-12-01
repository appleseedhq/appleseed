
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
#include "foundation/utility/typetraits.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

namespace foundation
{

//
// Private copy constructor and copy assignment ensure classes derived
// from class NonCopyable cannot be copied or constructed by copy.
// Taken from boost (http://www.boost.org/boost/noncopyable.hpp).
//

class APPLESEED_DLLSYMBOL NonCopyable
{
  protected:
    // Class NonCopyable has protected constructor and destructor members
    // to emphasize that it is to be used only as a base class.
    NonCopyable() {}
    ~NonCopyable() {}

  private:
    // Prevent NonCopyable class and any class inheriting it from being
    // copied or constructed by copy.
    NonCopyable(const NonCopyable&);
    NonCopyable& operator=(const NonCopyable&);
};


//
// Check whether a class inherits from foundation::NonCopyable or not.
//

template <typename T>
struct IsCopyable
  : public IsNotBase<NonCopyable, T>
{
};

}   // namespace foundation
