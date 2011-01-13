
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

#ifndef APPLESEED_FOUNDATION_UTILITY_AUTORELEASEPTR_H
#define APPLESEED_FOUNDATION_UTILITY_AUTORELEASEPTR_H

// Standard headers.
#include <cassert>

namespace foundation
{

//
// Similarly to std::auto_ptr, foundation::auto_release_ptr allows
// to wrap a pointer to ensure automatic deletion of the object
// pointer when falling out of scope. The object pointer is deleted
// using the foundation::IUnknown::release() method, instead of the
// plain delete operator.
//

// Proxy reference for auto_release_ptr<> copying.
template <class T>
struct auto_release_ptr_ref
{
    // Construct from generic pointer.
    explicit auto_release_ptr_ref(void* rhs)
      : m_ref(rhs)
    {
    }

    void* m_ref;    // generic pointer to auto_release_ptr pointer
};

template <class T>
class auto_release_ptr
{
  public:
    typedef T element_type;

    // Construct from pointer.
    explicit auto_release_ptr(T* ptr = 0) throw()
      : m_ptr(ptr)
    {
    }

    // Copy constructors.
    auto_release_ptr(auto_release_ptr<T>& rhs) throw()
      : m_ptr(rhs.release())
    {
    }
    template <class U>
    auto_release_ptr(auto_release_ptr<U>& rhs) throw()
      : m_ptr(rhs.release())
    {
    }
    auto_release_ptr(auto_release_ptr_ref<T> rhs) throw()
    {
        T** pptr = (T**)rhs.m_ref;
        T* ptr = *pptr;
        *pptr = 0;          // release old
        m_ptr = ptr;        // reset this
    }

    // Delete the pointer using IUnknown::release().
    ~auto_release_ptr()
    {
        if (m_ptr)
            m_ptr->release();
    }

    // Assignment operators.
    auto_release_ptr<T>& operator=(auto_release_ptr<T>& rhs) throw()
    {
        reset(rhs.release());
        return *this;
    }
    template <class U>
    auto_release_ptr<T>& operator=(auto_release_ptr<U>& rhs) throw()
    {
        reset(rhs.release());
        return *this;
    }
    auto_release_ptr<T>& operator=(auto_release_ptr_ref<T> rhs) throw()
    {
        T** pptr = (T**)rhs.m_ref;
        T* ptr = *pptr;
        *pptr = 0;          // release old
        reset(ptr);         // set new
        return *this;
    }

    // Convert to compatible auto_release_ptr<>.
    template <class U>
    operator auto_release_ptr<U>() throw()
    {
        return auto_release_ptr<U>(*this);
    }

    // Convert to compatible auto_release_ptr_ref<>.
    template <class U>
    operator auto_release_ptr_ref<U>() throw()
    {
        U* test_ptr = m_ptr;    // test implicit conversion
        auto_release_ptr_ref<U> ans(&m_ptr);
        return test_ptr != 0 ? ans : ans;
    }

    // Pointer dereference.
    T& operator*() const throw()
    {
        return *m_ptr;
    }
    T* operator->() const throw()
    {
        return &**this;
    }

    // Return wrapped pointer.
    T* get() const throw()
    {
        return m_ptr;
    }

    T& ref() const throw()
    {
        assert(m_ptr);
        return *m_ptr;
    }

    // Return wrapped pointer and give up ownership.
    T* release() throw()
    {
        T* tmp = m_ptr;
        m_ptr = 0;
        return tmp;
    }

    // Destroy designated object and store new pointer.
    void reset(T* ptr = 0)
    {
        if (ptr != m_ptr)
        {
            if (m_ptr)
                m_ptr->release();
            m_ptr = ptr;
        }
    }

  private:
    T* m_ptr;       // the wrapped pointer
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_AUTORELEASEPTR_H
