/*=============================================================================
    Copyright (c) 2006-2007 Tobias Schwinger
  
    Use modification and distribution are subject to the Boost Software 
    License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
    http://www.boost.org/LICENSE_1_0.txt).
==============================================================================*/

#include <boost/fusion/functional/adapter/unfused_rvalue_args.hpp>
#include <boost/detail/lightweight_test.hpp>

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
    struct result< Self(Seq) >
        : mpl::identity<long> 
    { };

    template <typename Seq>
    long operator()(Seq const & seq) const
    {
        long state = 0;
        return fusion::fold(seq, state, fold_op());
    }

    template < typename Seq >
    long operator()(Seq const & seq) 
    {
        long state = 100;
        return fusion::fold(seq, state, fold_op());
    }

  private:

    struct fold_op
    {
        typedef long result_type;

        template <typename T>
        long operator()(T const & elem, long value) const
        {
          return value + sizeof(T) * elem;
        }
    };
};

void result_type_tests()
{
    using boost::is_same;

    typedef fusion::unfused_rvalue_args< test_func<> > t;
    BOOST_TEST(( is_same< boost::result_of< t () >::type, long >::value ));
    BOOST_TEST(( is_same< boost::result_of< t (int) >::type, long >::value ));
}

int main()
{
    result_type_tests();

    test_func<noncopyable> f;
    fusion::unfused_rvalue_args< test_func<> > unfused_func;
    fusion::unfused_rvalue_args< test_func<noncopyable> & > unfused_func_ref(f);
    fusion::unfused_rvalue_args< test_func<> const > unfused_func_c;
    fusion::unfused_rvalue_args< test_func<> > const unfused_func_c2;
    fusion::unfused_rvalue_args< test_func<noncopyable> const & > unfused_func_c_ref(f);

    BOOST_TEST(unfused_func() == 100);
    BOOST_TEST(unfused_func_ref() == 100);
    BOOST_TEST(unfused_func_c() == 0);
    BOOST_TEST(unfused_func_c2() == 0);
    BOOST_TEST(unfused_func_c_ref() == 0);

    static const long expected = 1*sizeof(int) + 2*sizeof(long) + 7*sizeof(char);
    BOOST_TEST(unfused_func(1,2l,'\007') == 100 + expected); 
    BOOST_TEST(unfused_func_ref(1,2l,'\007') == 100 + expected); 
    BOOST_TEST(unfused_func_c(1,2l,'\007') == 0 + expected); 
    BOOST_TEST(unfused_func_c2(1,2l,'\007') == 0 + expected); 
    BOOST_TEST(unfused_func_c_ref(1,2l,'\007') == 0 + expected); 

    return boost::report_errors();
}

