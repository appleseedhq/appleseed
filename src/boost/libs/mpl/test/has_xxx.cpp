
// Copyright Aleksey Gurtovoy 2000-2004
//
// Distributed under the Boost Software License, Version 1.0. 
// (See accompanying file LICENSE_1_0.txt or copy at 
// http://www.boost.org/LICENSE_1_0.txt)
//
// See http://www.boost.org/libs/mpl for documentation.

// $Id: has_xxx.cpp 49268 2008-10-11 06:26:17Z agurtovoy $
// $Date: 2008-10-11 02:26:17 -0400 (Sat, 11 Oct 2008) $
// $Revision: 49268 $

#include <boost/mpl/has_xxx.hpp>
#include <boost/mpl/aux_/config/workaround.hpp>
#include <boost/mpl/aux_/test.hpp>

BOOST_MPL_HAS_XXX_TRAIT_DEF(xxx)

struct a1 {};
struct a2 { void xxx(); };
struct a3 { int xxx; };
struct a4 { static int xxx(); };
struct a5 { template< typename T > struct xxx {}; };

struct b1 { typedef int xxx; };
struct b2 { struct xxx; };
struct b3 { typedef int& xxx; };
struct b4 { typedef int* xxx; };
struct b5 { typedef int xxx[10]; };
struct b6 { typedef void (*xxx)(); };
struct b7 { typedef void (xxx)(); };

template< typename T > struct outer;
template< typename T > struct inner { typedef typename T::type type; };

// agurt, 15/aug/04: make sure MWCW passes the test in presence of the following
// template
template< typename T > struct xxx;


MPL_TEST_CASE()
{
    MPL_ASSERT_NOT(( has_xxx<int> ));
#if !BOOST_WORKAROUND(BOOST_MSVC, <= 1300)
    MPL_ASSERT_NOT(( has_xxx<int&> ));
    MPL_ASSERT_NOT(( has_xxx<int*> ));
    MPL_ASSERT_NOT(( has_xxx<int[]> ));
    MPL_ASSERT_NOT(( has_xxx<int (*)()> ));

    MPL_ASSERT_NOT(( has_xxx<a2> ));
    MPL_ASSERT_NOT(( has_xxx<a3> ));
    MPL_ASSERT_NOT(( has_xxx<a4> ));
#if !BOOST_WORKAROUND(__MWERKS__, BOOST_TESTED_AT(0x3202))
    MPL_ASSERT_NOT(( has_xxx<a5> ));
#endif
    MPL_ASSERT_NOT(( has_xxx< enum_ > ));
#endif
    MPL_ASSERT_NOT(( has_xxx<a1> ));
    MPL_ASSERT_NOT(( has_xxx< outer< inner<int> > > ));
    MPL_ASSERT_NOT(( has_xxx< incomplete > ));
    MPL_ASSERT_NOT(( has_xxx< abstract > ));
    MPL_ASSERT_NOT(( has_xxx< noncopyable > ));

    MPL_ASSERT(( has_xxx<b1,true_> ));
    MPL_ASSERT(( has_xxx<b2,true_> ));
    MPL_ASSERT(( has_xxx<b3,true_> ));
    MPL_ASSERT(( has_xxx<b4,true_> ));
    MPL_ASSERT(( has_xxx<b5,true_> ));
    MPL_ASSERT(( has_xxx<b6,true_> ));
    MPL_ASSERT(( has_xxx<b7,true_> ));

#if !defined(HAS_XXX_ASSERT)
#   define HAS_XXX_ASSERT(x) MPL_ASSERT(x)
#endif

    HAS_XXX_ASSERT(( has_xxx<b1> ));
    HAS_XXX_ASSERT(( has_xxx<b2> ));
    HAS_XXX_ASSERT(( has_xxx<b3> ));
    HAS_XXX_ASSERT(( has_xxx<b4> ));
    HAS_XXX_ASSERT(( has_xxx<b5> ));
    HAS_XXX_ASSERT(( has_xxx<b6> ));
    HAS_XXX_ASSERT(( has_xxx<b7> ));
}
