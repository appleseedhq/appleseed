// Copyright David Abrahams and Aleksey Gurtovoy
// 2002-2004. Distributed under the Boost Software License, Version
// 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

// run-time test for "boost/ref.hpp" header content
// see 'ref_ct_test.cpp' for compile-time part

#if defined(_MSC_VER) && !defined(__ICL)
# pragma warning(disable: 4786)  // identifier truncated in debug info
# pragma warning(disable: 4710)  // function not inlined
# pragma warning(disable: 4711)  // function selected for automatic inline expansion
# pragma warning(disable: 4514)  // unreferenced inline removed
#endif

#include <boost/ref.hpp>

#if defined(BOOST_MSVC) && (BOOST_MSVC < 1300)
# pragma warning(push, 3)
#endif

#include <iostream>

#if defined(BOOST_MSVC) && (BOOST_MSVC < 1300)
# pragma warning(pop)
#endif


#define BOOST_INCLUDE_MAIN
#include <boost/test/test_tools.hpp>

namespace {
using namespace boost;

template <class T>
struct ref_wrapper
{
    // Used to verify implicit conversion
    static T* get_pointer(T& x)
    {
        return &x;
    }

    static T const* get_const_pointer(T const& x)
    {
        return &x;
    }

    template <class Arg>
    static T* passthru(Arg x)
    {
        return get_pointer(x);
    }

    template <class Arg>
    static T const* cref_passthru(Arg x)
    {
        return get_const_pointer(x);
    }

    static void test(T x)
    {
        BOOST_CHECK(passthru(ref(x)) == &x);
        BOOST_CHECK(&ref(x).get() == &x);

        BOOST_CHECK(cref_passthru(cref(x)) == &x);
        BOOST_CHECK(&cref(x).get() == &x);
    }
};

} // namespace unnamed

int test_main(int, char * [])
{
    ref_wrapper<int>::test(1);
    ref_wrapper<int const>::test(1);
    return 0;
}
