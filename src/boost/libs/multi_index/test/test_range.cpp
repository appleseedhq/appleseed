/* Boost.MultiIndex test for range().
 *
 * Copyright 2003-2008 Joaquin M Lopez Munoz.
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *
 * See http://www.boost.org/libs/multi_index for library home page.
 */

#include "test_range.hpp"

#include <boost/config.hpp> /* keep it first to prevent nasty warns in MSVC */
#include <algorithm>
#include <functional>
#include "pre_multi_index.hpp"
#include <boost/multi_index_container.hpp>
#include <boost/multi_index/identity.hpp>
#include <boost/multi_index/ordered_index.hpp>
#include <boost/test/test_tools.hpp>

using namespace boost::multi_index;

typedef multi_index_container<int>  int_set;
typedef int_set::iterator int_set_iterator;

#undef _
#define _ ,

#undef CHECK_RANGE
#define CHECK_RANGE(p,check_range) \
{\
  int v[]=check_range;\
  std::size_t size_v=sizeof(v)/sizeof(int);\
  BOOST_CHECK(std::size_t(std::distance((p).first,(p).second))==size_v);\
  BOOST_CHECK(std::equal((p).first,(p).second,&v[0]));\
}

#undef CHECK_VOID_RANGE
#define CHECK_VOID_RANGE(p) BOOST_CHECK((p).first==(p).second)

void test_range()
{
  int_set is;

  for(int i=1;i<=10;++i)is.insert(i);

  std::pair<int_set::iterator,int_set::iterator> p;

  p=is.range(unbounded,unbounded);
  CHECK_RANGE(p,{1 _ 2 _ 3 _ 4 _ 5 _ 6 _ 7 _ 8 _ 9 _ 10});

  p=is.range(
    std::bind1st(std::less<int>(),5), /* 5 < x */
    unbounded);
  CHECK_RANGE(p,{6 _ 7 _ 8 _ 9 _ 10});

  p=is.range(
    std::bind1st(std::less_equal<int>(),8), /* 8 <= x */
    unbounded);
  CHECK_RANGE(p,{8 _ 9 _ 10});

  p=is.range(
    std::bind1st(std::less_equal<int>(),11), /* 11 <= x */
    unbounded);
  CHECK_VOID_RANGE(p);

  p=is.range(
    unbounded,
    std::bind2nd(std::less<int>(),8)); /* x < 8 */
  CHECK_RANGE(p,{1 _ 2 _ 3 _ 4 _ 5 _ 6 _ 7});

  p=is.range(
    unbounded,
    std::bind2nd(std::less_equal<int>(),4)); /* x <= 4 */
  CHECK_RANGE(p,{1 _ 2 _ 3 _ 4});

  p=is.range(
    unbounded,
    std::bind2nd(std::less_equal<int>(),0)); /* x <= 0 */
  CHECK_VOID_RANGE(p);

  p=is.range(
    std::bind1st(std::less<int>(),6),        /* 6 <  x */
    std::bind2nd(std::less_equal<int>(),9)); /* x <= 9 */
  CHECK_RANGE(p,{7 _ 8 _ 9});

  p=is.range(
    std::bind1st(std::less_equal<int>(),4), /* 4 <= x */
    std::bind2nd(std::less<int>(),5));      /* x <  5 */
  CHECK_RANGE(p,{4});

  p=is.range(
    std::bind1st(std::less_equal<int>(),10),  /* 10 <=  x */
    std::bind2nd(std::less_equal<int>(),10)); /*  x <= 10 */
  CHECK_RANGE(p,{10});

  p=is.range(
    std::bind1st(std::less<int>(),0),   /* 0 <  x */
    std::bind2nd(std::less<int>(),11)); /* x < 11 */
  CHECK_RANGE(p,{1 _ 2 _ 3 _ 4 _ 5 _ 6 _ 7 _ 8 _ 9 _ 10});

  p=is.range(
    std::bind1st(std::less<int>(),7),        /* 7 <  x */
    std::bind2nd(std::less_equal<int>(),7)); /* x <= 7 */
  CHECK_VOID_RANGE(p);
  BOOST_CHECK(p.first==is.upper_bound(7));

  p=is.range(
    std::bind1st(std::less_equal<int>(),8), /* 8 <= x */
    std::bind2nd(std::less<int>(),2));      /* x <  2 */
  CHECK_VOID_RANGE(p);
  BOOST_CHECK(p.first==is.lower_bound(8));

  p=is.range(
    std::bind1st(std::less<int>(),4),  /* 4 < x */
    std::bind2nd(std::less<int>(),5)); /* x < 5 */
  CHECK_VOID_RANGE(p);
  BOOST_CHECK(p.first!=is.end());
}
