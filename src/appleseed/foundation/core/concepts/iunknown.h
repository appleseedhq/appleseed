
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

// appleseed.main headers.
#include "main/dllsymbol.h"

namespace foundation
{

//
// A COM-like IUnknown interface.
//
// At the moment, only the Release method is implemented, and it behaves
// differently than the COM IUnknown::Release in that it simply deletes
// the instance, whereas the COM one decrements the reference count by 1.
// The other two methods, IUnknown::AddRef and IUnknown::QueryInterface,
// are not yet supported.
//

class APPLESEED_DLLSYMBOL IUnknown
  : public NonCopyable
{
  public:
    // Delete this instance.
    // Use this method to delete an instance which was created by a
    // factory. This ensures that an instance is created and deleted
    // using the same memory subsystem, for example if the factory
    // resides inside a DLL. Do not use this method to delete an
    // instance which was created using the new operator. In this case,
    // simply use the delete operator.
    virtual void release() = 0;

  protected:
    // Declare the destructor as protected to prevent the user from
    // calling delete on an IUnknown pointer, but not on a pointer
    // type derived from IUnknown.
    // For convenience, also declare the destructor as virtual to
    // make it safe to call delete on a derived type without having
    // to add virtual destructors to all derived types.
    virtual ~IUnknown() {}
};

}   // namespace foundation
