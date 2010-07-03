/*=============================================================================
    Copyright (c) 2001-2006 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying 
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
==============================================================================*/
#include <boost/detail/lightweight_test.hpp>
#include <boost/fusion/sequence/io/out.hpp>
#include <boost/fusion/view/single_view/single_view.hpp>
#include <boost/fusion/sequence/intrinsic/begin.hpp>
#include <boost/fusion/iterator/deref.hpp>

struct X {};

template <typename OS>
OS& operator<<(OS& os, X const&)
{
    os << "<X-object>";
    return os;
}

void foo() {}

int
main()
{
    using namespace boost::fusion;

    std::cout << tuple_open('[');
    std::cout << tuple_close(']');
    std::cout << tuple_delimiter(", ");

    {
        single_view<int> view1(3);
        std::cout << view1 << std::endl;
        
#ifdef FUSION_TEST_FAIL
        // single_view is immutable
        *begin(view1) += 4;
#endif
        std::cout << view1 << std::endl;
        BOOST_TEST(*begin(view1) == 3);
        BOOST_TEST(view1.val == 3);

        single_view<X> view2;
        std::cout << view2 << std::endl;
    }
    
    {
        std::cout << make_single_view(1) << std::endl;
        std::cout << make_single_view("Hello, World") << std::endl;
        std::cout << make_single_view(&foo) << std::endl;
    }

    return boost::report_errors();
}

