/*=============================================================================
    Copyright (c) 2006-2007 Tobias Schwinger
  
    Use modification and distribution are subject to the Boost Software 
    License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
    http://www.boost.org/LICENSE_1_0.txt).
==============================================================================*/

#include <boost/fusion/functional/adapter/unfused_generic.hpp>
#include <boost/detail/lightweight_test.hpp>

#include <boost/type_traits/is_same.hpp>

#include <boost/noncopyable.hpp>
#include <boost/blank.hpp>

#include <boost/mpl/identity.hpp>

#include <boost/utility/result_of.hpp>

#include <boost/fusion/algorithm/iteration/fold.hpp>

namespace fusion = boost::fusion;
namespace mpl = boost::mpl;

using boost::noncopyable;
typedef mpl::true_ no_nullary_call;

template <class Base = boost::blank>
struct test_func
    : Base
{
    template <typename Sig>
    struct result;

    template <class Self, class Seq>
    struct result< Self (Seq) >
        : mpl::identity<long>
    { };

    template <typename Seq>
    long operator()(Seq const & seq) const
    {
        long state = 0;
        return fusion::fold(seq, state, fold_op());
    }

    template <typename Seq>
    long operator()(Seq const & seq) 
    {
        long state = 100;
        return fusion::fold(seq, state, fold_op());
    }

  private:

    struct fold_op
    {
        template <typename T>
        long operator()(T const & elem, long value) const
        {
          return value + sizeof(T) * elem;
        }

        template <typename T>
        long operator()(T & elem, long value) const
        {
          elem += sizeof(T);
          return value;
        }

        template <typename Sig>
        struct result;

        template <class Self, typename T0, typename T1> struct result< Self(T0,T1) >
            : mpl::identity<long>
        { };
    };
};

void result_type_tests()
{
    using boost::is_same;

    typedef fusion::unfused_generic< test_func<> > t;
    BOOST_TEST(( is_same< boost::result_of< t () >::type, long >::value ));
    BOOST_TEST(( is_same< boost::result_of< t (int) >::type, long >::value ));
}

int main()
{
    result_type_tests();

    test_func<noncopyable> f;
    fusion::unfused_generic< test_func<> > unfused_func;
    fusion::unfused_generic< test_func<noncopyable> & > unfused_func_ref(f);
    fusion::unfused_generic< test_func<> const > unfused_func_c;
    fusion::unfused_generic< test_func<> > const unfused_func_c2;
    fusion::unfused_generic< test_func<noncopyable> const & > unfused_func_c_ref(f);

    BOOST_TEST(unfused_func() == 100);
    BOOST_TEST(unfused_func_ref() == 100);
    BOOST_TEST(unfused_func_c() == 0);
    BOOST_TEST(unfused_func_c2() == 0);
    BOOST_TEST(unfused_func_c_ref() == 0);

    long lvalue = 12;
    // also test const lvalues to pick up compiler deficiencies in that area
    int const clvalue_1 = 1;
    long const clvalue_2 = 2; 

    static const long expected = 1*sizeof(int) + 2*sizeof(long) + 7*sizeof(char);
    BOOST_TEST(unfused_func(lvalue,lvalue,clvalue_1,clvalue_2,'\007') == 100 + expected); 
    BOOST_TEST(lvalue == 12 + 2*sizeof(long));
    BOOST_TEST(unfused_func_ref(lvalue,lvalue,1,2l,'\007') == 100 + expected); 
    BOOST_TEST(lvalue == 12 + 4*sizeof(long));
    BOOST_TEST(unfused_func_c(lvalue,lvalue,1,2l,'\007') == 0 + expected); 
    BOOST_TEST(lvalue == 12 + 6*sizeof(long));
    BOOST_TEST(unfused_func_c2(lvalue,lvalue,1,2l,'\007') == 0 + expected); 
    BOOST_TEST(lvalue == 12 + 8*sizeof(long));
    BOOST_TEST(unfused_func_c_ref(lvalue,lvalue,1,2l,'\007') == 0 + expected); 
    BOOST_TEST(lvalue == 12 + 10*sizeof(long));

    return boost::report_errors();
}

