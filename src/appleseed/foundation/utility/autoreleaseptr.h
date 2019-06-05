
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
#include "foundation/platform/compilerfeatures.h"

// Standard headers.
#include <cassert>

namespace foundation
{

//
// Similarly to std::auto_ptr<>, foundation::auto_release_ptr<> is a smart pointer
// providing strict ownership semantics. The only difference with std::auto_ptr<>
// is that the pointed object is deallocated using foundation::IUnknown::release()
// instead of the delete operator.
//

// A wrapper class to provide auto_release_ptr with reference semantics.
template <typename T>
struct auto_release_ptr_ref
{
    T* m_ptr;

    explicit auto_release_ptr_ref(T* ptr) APPLESEED_NOEXCEPT
      : m_ptr(ptr)
    {
    }
};

template <typename T>
class auto_release_ptr
{
  public:
    typedef T element_type;

    // Construct from a raw pointer.
    explicit auto_release_ptr(T* ptr = nullptr) APPLESEED_NOEXCEPT
      : m_ptr(ptr)
    {
    }

    // Construct from another auto_release_ptr of the same type.
    auto_release_ptr(auto_release_ptr& rhs) APPLESEED_NOEXCEPT
      : m_ptr(rhs.release())
    {
    }

    // Construct from another auto_release_ptr of a different but related type.
    template <typename U>
    auto_release_ptr(auto_release_ptr<U>& rhs) APPLESEED_NOEXCEPT
      : m_ptr(rhs.release())
    {
    }

    // Construct from an auto_release_ptr_ref.
    auto_release_ptr(auto_release_ptr_ref<T> ref) APPLESEED_NOEXCEPT
      : m_ptr(ref.m_ptr)
    {
    }

    // Assign from another auto_release_ptr of the same type.
    auto_release_ptr& operator=(auto_release_ptr& rhs) APPLESEED_NOEXCEPT
    {
        reset(rhs.release());
        return *this;
    }

    // Assign from another auto_release_ptr of a different but related type.
    template <typename U>
    auto_release_ptr& operator=(auto_release_ptr<U>& rhs) APPLESEED_NOEXCEPT
    {
        reset(rhs.release());
        return *this;
    }

    // Assign from an auto_release_ptr_ref.
    auto_release_ptr<T>& operator=(auto_release_ptr_ref<T> ref)
    {
        if (m_ptr != ref.m_ptr)
        {
            if (m_ptr)
                m_ptr->release();

            m_ptr = ref.m_ptr;
        }

        return *this;
    }

    // Delete the wrapped pointer.
    ~auto_release_ptr()
    {
        if (m_ptr)
            m_ptr->release();
    }

    // Automatic conversion to an auto_release_ptr_ref.
    template <typename U>
    operator auto_release_ptr_ref<U>() APPLESEED_NOEXCEPT
    {
        return auto_release_ptr_ref<U>(static_cast<U*>(release()));
    }

    // Automatic conversion to an auto_release_ptr of a different type.
    template <typename U>
    operator auto_release_ptr<U>() APPLESEED_NOEXCEPT
    {
        return auto_release_ptr<U>(static_cast<U*>(release()));
    }

    // Dereference the wrapped pointer.
    T& operator*() const APPLESEED_NOEXCEPT
    {
        assert(m_ptr);
        return *m_ptr;
    }
    T* operator->() const APPLESEED_NOEXCEPT
    {
        assert(m_ptr);
        return m_ptr;
    }

    // Bool conversion of the wrapped pointer.
#if !defined(_MSC_VER) || _MSC_VER >= 1800
    explicit
#endif
    operator bool() const APPLESEED_NOEXCEPT
    {
        return m_ptr != nullptr;
    }

    // Return the wrapped pointer.
    T* get() const APPLESEED_NOEXCEPT
    {
        return m_ptr;
    }
    T& ref() const APPLESEED_NOEXCEPT
    {
        assert(m_ptr);
        return *m_ptr;
    }

    // Return the wrapped pointer and give up ownership.
    T* release() APPLESEED_NOEXCEPT
    {
        T* rv = m_ptr;
        m_ptr = nullptr;
        return rv;
    }

    // Delete the owned pointer and replace it with the provided raw pointer.
    void reset(T* ptr = nullptr)
    {
        if (m_ptr != ptr)
        {
            if (m_ptr)
                m_ptr->release();

            m_ptr = ptr;
        }
    }

  private:
    T* m_ptr;
};

}   // namespace foundation
