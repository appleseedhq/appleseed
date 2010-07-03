/*=============================================================================
    Copyright (c) 2006-2007 Tobias Schwinger
  
    Use modification and distribution are subject to the Boost Software 
    License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
    http://www.boost.org/LICENSE_1_0.txt).
==============================================================================*/

#include <boost/fusion/functional/generation/make_unfused_rvalue_args.hpp>
#include <boost/detail/lightweight_test.hpp>

#include <boost/noncopyable.hpp>
#include <boost/blank.hpp>

#include <boost/mpl/if.hpp>
#include <boost/mpl/and.hpp>
#include <boost/mpl/bool.hpp>
#include <boost/mpl/identity.hpp>

#include <boost/fusion/sequence/intrinsic/empty.hpp>
#include <boost/fusion/algorithm/iteration/fold.hpp>

#include <boost/ref.hpp>

namespace fusion = boost::fusion;
namespace mpl = boost::mpl;

using boost::noncopyable;
typedef mpl::true_ no_nullary_call;

using boost::ref;
using boost::cref;

template <class Base = boost::blank, class RemoveNullary = mpl::false_>
struct test_func
    : Base
{
    template <typename Sig>
    struct result;

    template <class Self, class Seq> 
    struct result< Self(Seq &) >
        : mpl::if_< mpl::and_< fusion::result_of::empty<Seq>, RemoveNullary >, 
                    boost::blank, mpl::identity<long> >::type
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

template <typename T>
inline T const & const_(T const & t)
{
    return t;
}

int main()
{
    test_func<> f;
    test_func<noncopyable> f_nc;

    fusion::result_of::make_unfused_rvalue_args< test_func<> >::type unfused_func =
        fusion::make_unfused_rvalue_args(f);

    fusion::result_of::make_unfused_rvalue_args< boost::reference_wrapper< 
        test_func<noncopyable> > >::type unfused_func_ref = 
            fusion::make_unfused_rvalue_args(ref(f_nc));

    fusion::result_of::make_unfused_rvalue_args< boost::reference_wrapper< 
        test_func<noncopyable> const> >::type unfused_func_c_ref = 
            fusion::make_unfused_rvalue_args(cref(f_nc));

    BOOST_TEST(unfused_func() == 100);
    BOOST_TEST(const_(unfused_func)() == 0);
    BOOST_TEST(unfused_func_ref() == 100);
    BOOST_TEST(unfused_func_c_ref() == 0);

    static const long expected = 1*sizeof(int) + 2*sizeof(long) + 7*sizeof(char);
    BOOST_TEST(unfused_func(1,2l,'\007') == 100 + expected); 
    BOOST_TEST(const_(unfused_func)(1,2l,'\007') == 0 + expected); 
    BOOST_TEST(unfused_func_ref(1,2l,'\007') == 100 + expected); 
    BOOST_TEST(unfused_func_c_ref(1,2l,'\007') == 0 + expected); 

    return boost::report_errors();
}

