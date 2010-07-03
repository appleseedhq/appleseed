
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

#ifndef APPLESEED_FOUNDATION_UTILITY_IMPLPTR_H
#define APPLESEED_FOUNDATION_UTILITY_IMPLPTR_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"

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
// A generic implementation of the 'Pimpl' idiom, based on the work of Alan Griffiths
// (http://www.octopull.demon.co.uk/arglib/).
//
// References:
//
//   http://en.wikipedia.org/wiki/Opaque_pointer
//   http://www.octopull.demon.co.uk/arglib/TheGrin.html
//   http://www.drdobbs.com/tools/205918714
//
// Basic usage:
//
//   --- foo.h ---
//
//       #include "foundation/utility/implptr.h"
//
//       class Foo
//       {
//         public:
//           Foo(const int x);
//           int get() const;
//
//         private:
//           struct Impl;
//           foundation::impl_ptr<Impl> impl;
//       };
//
//
//   --- foo.cpp ---
//
//       #include "foo.h"
//
//       struct Foo::Impl
//       {
//           int x;
//
//           explicit Impl(int x_) : x(x_) {}
//       };
//
//       Foo::Foo(const int x) : impl(new Impl(x)) {}
//
//       int Foo::get() const
//       {
//          return impl->x;
//       }
//
// By default, foundation::impl_ptr<> provides a copy constructor and an assignment operator.
//
// If the implementation class is not copyable, false should be passed as a second argument
// to foundation::impl_ptr<>, as illustrated below:
//
//       class NonCopyableFoo
//       {
//         public:
//           ...
//
//         private:
//           struct Impl;
//           foundation::impl_ptr<Impl, false> impl;
//       };
//
// A helper macro is provided to facilitate the declaration of the private implementation.
// It selects the adequate specialization of foundation::impl_ptr<> based on whether the
// class inherits from foundation::NonCopyable or not:
//
//       class CopyableFoo                              class NonCopyableFoo
//       {                                                : public foundation::NonCopyable
//         public:                                      {
//           ...                                          public:
//                                                          ...
//         private:
//           FOUNDATION_PIMPL(CopyableFoo);               private:
//       };                                                 FOUNDATION_PIMPL(NonCopyableFoo);
//                                                      };
//

template <typename Impl, bool IsCopyable = true>
class FOUNDATIONDLL impl_ptr;

// This variant provides a copy constructor, an assignment operator, etc.
template <typename Impl>
class FOUNDATIONDLL impl_ptr<Impl, true>
{
  public:
    // Constructor.
    explicit impl_ptr(Impl* impl)
      : m_copy(do_copy)
      , m_assign(do_assign)
      , m_delete(do_delete)
      , m_impl(impl)
    {
        sizeof(Impl);   // at this point, Impl must be completely defined
    }

    // Copy constructor.
    impl_ptr(const impl_ptr& rhs)
      // In the copy constructor, we must copy the function pointers from rhs,
      // as taking the address of the functions require Impl to be fully defined.
      : m_copy(rhs.m_copy)
      , m_assign(rhs.m_assign)
      , m_delete(rhs.m_delete)
      , m_impl(m_copy(rhs.m_impl))
    {
    }

    // Destructor.
    ~impl_ptr()
        throw ()
    {
        m_delete(m_impl);
    }

    // Assignment operator.
    impl_ptr& operator=(const impl_ptr& rhs)
    {
        m_assign(m_impl, rhs.m_impl);
        return *this;
    }

    // Dereference operators.
    const Impl* operator->() const  { return m_impl;  }
    Impl* operator->()              { return m_impl;  }
    const Impl& operator*() const   { return *m_impl; }
    Impl& operator*()               { return *m_impl; }

  private:
    typedef Impl* (*copy_func)(const Impl* p);
    typedef void (*assign_func)(Impl* lhs, const Impl* rhs);
    typedef void (*delete_func)(Impl* p);

    copy_func       m_copy;
    assign_func     m_assign;
    delete_func     m_delete;
    Impl*           m_impl;         // keep last

    static Impl* do_copy(const Impl* p)
    {
        return p ? new Impl(*p) : 0;
    }

    static void do_assign(Impl* lhs, const Impl* rhs)
    {
        *lhs = *rhs;
    }

    static void do_delete(Impl* p)
    {
        delete p;
    }
};

// This variant is to be used with non-copyable classes.
template <typename Impl>
class FOUNDATIONDLL impl_ptr<Impl, false>
{
  public:
    // Constructor.
    explicit impl_ptr(Impl* impl)
      : m_delete(do_delete)
      , m_impl(impl)
    {
        sizeof(Impl);   // at this point, Impl must be completely defined
    }

    // Destructor.
    ~impl_ptr()
        throw ()
    {
        m_delete(m_impl);
    }

    // Dereference operators.
    const Impl* operator->() const  { return m_impl;  }
    Impl* operator->()              { return m_impl;  }
    const Impl& operator*() const   { return *m_impl; }
    Impl& operator*()               { return *m_impl; }

  private:
    typedef void (*delete_func)(Impl* p);

    delete_func     m_delete;
    Impl*           m_impl;         // keep last

    static void do_delete(Impl* p)
    {
        delete p;
    }
};

#define FOUNDATION_IMPL_PTR(T, Impl)                                        \
    foundation::impl_ptr<Impl, foundation::IsCopyable<T>::value> impl

#define FOUNDATION_PIMPL(T)                                                 \
    struct Impl;                                                            \
    FOUNDATION_IMPL_PTR(T, Impl)

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_IMPLPTR_H
