
// Copyright Aleksey Gurtovoy 2000-2004
//
// Distributed under the Boost Software License, Version 1.0. 
// (See accompanying file LICENSE_1_0.txt or copy at 
// http://www.boost.org/LICENSE_1_0.txt)
//
// See http://www.boost.org/libs/mpl for documentation.

// $Id: push_front.cpp 49268 2008-10-11 06:26:17Z agurtovoy $
// $Date: 2008-10-11 02:26:17 -0400 (Sat, 11 Oct 2008) $
// $Revision: 49268 $

#include <boost/mpl/push_front.hpp>
#include <boost/mpl/push_back.hpp>
#include <boost/mpl/list/list10.hpp>
#include <boost/mpl/size.hpp>
#include <boost/mpl/front.hpp>

#include <boost/mpl/aux_/test.hpp>

MPL_TEST_CASE()
{
    typedef push_front<list0<>,long>::type res1;
    typedef push_front<list1<long>,int>::type res2;
    typedef push_front<list2<int,long>,char>::type res3;
    
    MPL_ASSERT_RELATION( size<res1>::value, ==, 1 );
    MPL_ASSERT_RELATION( size<res2>::value, ==, 2 );
    MPL_ASSERT_RELATION( size<res3>::value, ==, 3 );
    
    MPL_ASSERT(( is_same< front<res1>::type, long > ));
    MPL_ASSERT(( is_same< front<res2>::type, int > ));
    MPL_ASSERT(( is_same< front<res3>::type, char > ));
    
    MPL_ASSERT(( has_push_front< list0<> > ));
    MPL_ASSERT(( has_push_front< list1<long> > ));

    MPL_ASSERT_NOT(( has_push_back< list0<> > ));
}
