/*=============================================================================
    Copyright (c) 2001-2006 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying 
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
==============================================================================*/
#include <boost/detail/lightweight_test.hpp>
#include <boost/fusion/container/vector/vector.hpp>
#include <boost/fusion/adapted/mpl.hpp>
#include <boost/fusion/container/vector/vector_iterator.hpp>
#include <boost/fusion/sequence/io/out.hpp>
#include <boost/fusion/sequence/comparison/equal_to.hpp>
#include <boost/fusion/view/filter_view/filter_view.hpp>
#include <boost/fusion/container/generation/make_vector.hpp>
#include <boost/fusion/sequence/intrinsic/size.hpp>
#include <boost/type_traits/is_class.hpp>
#include <boost/type_traits/is_same.hpp>
#include <boost/mpl/arg.hpp>
#include <boost/mpl/not.hpp>
#include <boost/mpl/vector_c.hpp>
#include <boost/mpl/less.hpp>
#include <boost/mpl/bool.hpp>
#include <boost/mpl/assert.hpp>

struct X
{
    operator char const*() const
    {
        return "<X-object>";
    }
};

struct Y
{
    operator char const*() const
    {
        return "<Y-object>";
    }
};

struct reject_all
{
    template<typename T>
    struct apply : boost::mpl::false_
    {};
};

int
main()
{
    using namespace boost::fusion;

    using boost::mpl::int_;
    using boost::mpl::_;
    using boost::mpl::not_;
    using boost::mpl::less;
    using boost::mpl::vector_c;
    using boost::is_class;
    using boost::is_same;

    std::cout << tuple_open('[');
    std::cout << tuple_close(']');
    std::cout << tuple_delimiter(", ");

    { // Testing the static find_if (internal function)

        typedef vector<int, char, long, X> vector_type;

        vector_type v(1, 'x', 987654, X());
        typedef vector_iterator<vector_type, 0> begin;
        typedef vector_iterator<vector_type, 4> end;
        typedef detail::static_find_if<begin, end, is_same<_, long> > filter;
        typedef filter::type type;

        BOOST_TEST(*type(v) == 987654);
        std::cout << *type(v) << std::endl;
        std::cout << *filter::call(begin(v)) << std::endl;
        BOOST_TEST(*type(v) == *filter::call(begin(v)));
    }

    {
        typedef vector<Y, char, long, X, bool, double> vector_type;

        X x; Y y;
        vector_type v(y, '@', 987654, x, true, 6.6);
        typedef filter_view<vector_type const, not_<is_class<_> > > filter_view_type;
        filter_view_type view(v);
        std::cout << view << std::endl;
        BOOST_TEST((view == make_vector('@', 987654, true, 6.6)));
        BOOST_STATIC_ASSERT(result_of::size<filter_view_type>::value == 4);
    }

    {
        // $$$ JDG $$$ For some obscure reason, EDG based compilers
        // (e.g. comeau 4.3.3, intel) have problems with this.
        // vc7.1 and g++ are ok. The errors from comeau are useless.
        
#ifndef __EDG_VERSION__
        typedef vector_c<int, 5, 1, 2, 3, 6, 0, -1> vector_type;
        typedef filter_view<vector_type const, less<_, int_<3> > > filter_view_type;
        vector_type v;
        filter_view_type view(v);
        std::cout << view << std::endl;
        BOOST_TEST((view == make_vector(1, 2, 0, -1)));
        BOOST_STATIC_ASSERT(result_of::size<filter_view_type>::value == 4);
#endif
    }

    {
        // Previous filtering out all values caused problems as begin<seq> was not equal to end<seq>
        // Picked up by Andreas Pokorny
        typedef vector<int> vec;
        typedef filter_view<vec, reject_all> filter_view_type;

        BOOST_MPL_ASSERT((result_of::equal_to<result_of::begin<filter_view_type>::type, result_of::end<filter_view_type>::type>));
    }

    return boost::report_errors();
}

