// Copyright Alexander Nasonov 2007-2008
//
// Distributed under the Boost Software License, Version 1.0. 
// (See accompanying file LICENSE_1_0.txt or copy at 
// http://www.boost.org/LICENSE_1_0.txt)

#include <boost/scope_exit.hpp>

int tu1();
int tu2();

inline int inline_f()
{
    int i = 99;
    {
        BOOST_SCOPE_EXIT( (&i) ) { i = -1; } BOOST_SCOPE_EXIT_END
    }
    return i;
}

#if !defined(BOOST_SCOPE_EXIT_AUX_GCC)
#error "BOOST_SCOPE_EXIT_AUX_GCC undefined!"
#elif BOOST_SCOPE_EXIT_AUX_GCC == 0 || BOOST_SCOPE_EXIT_AUX_GCC >= 304
template<class Int>
Int template_f(Int i)
{
    {
        BOOST_SCOPE_EXIT_TPL( (&i) ) { ++i; } BOOST_SCOPE_EXIT_END
    }
    return i;
}
#else
inline int template_f(int i)
{
    {
        BOOST_SCOPE_EXIT( (&i) ) { ++i; } BOOST_SCOPE_EXIT_END
    }
    return i;
}
#endif

