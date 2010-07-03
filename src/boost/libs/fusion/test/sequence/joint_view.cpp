/*=============================================================================
    Copyright (c) 2001-2006 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying 
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
==============================================================================*/
#include <boost/detail/lightweight_test.hpp>
#include <boost/fusion/container/vector/vector.hpp>
#include <boost/fusion/view/joint_view/joint_view.hpp>
#include <boost/fusion/sequence/io/out.hpp>
#include <boost/fusion/sequence/comparison/equal_to.hpp>
#include <boost/fusion/container/generation/make_vector.hpp>
#include <boost/fusion/sequence/intrinsic/at.hpp>
#include <boost/mpl/vector_c.hpp>

struct X
{
    operator char const*() const
    {
        return "<X-object>";
    }
};

int
main()
{
    using namespace boost::fusion;

    std::cout << tuple_open('[');
    std::cout << tuple_close(']');
    std::cout << tuple_delimiter(", ");

/// Testing joint_view

    {
        vector<int> t1(3);
        vector<X> t2;
        typedef joint_view<vector<int>, vector<X> > view_type;
        view_type view(t1, t2);

        std::cout << view << std::endl;
        BOOST_TEST((view == make_vector(3, X())));
    }

    {
        vector<int, char> t1(3, 'x');
        vector<X> t2;
        typedef joint_view<vector<int, char>, vector<X> > view_type;
        view_type view(t1, t2);
        std::cout << view << std::endl;
        BOOST_TEST((view == make_vector(3, 'x', X())));

        *begin(view) = 4;
        BOOST_TEST(at_c<0>(t1) == 4);
    }

    {
        vector<int, char> t1(3, 'x');
        vector<X, int> t2;
        typedef joint_view<vector<int, char>, vector<X, int> > view_type;
        view_type view(t1, t2);
        std::cout << view << std::endl;
        BOOST_TEST((view == make_vector(3, 'x', X(), 0)));
    }

    {
        typedef vector<int> t1_type;
        t1_type t1(777);
        typedef vector<int, char, double> t2_type;
        t2_type t2(1, 'x', 3.3);

        {
            typedef joint_view<t1_type, t2_type> view_type;
            view_type view(t1, t2);
            std::cout << view << std::endl;
            BOOST_TEST((view == make_vector(777, 1, 'x', 3.3)));
        }

        {
            typedef joint_view<t2_type, t1_type> view_type;
            view_type view(t2, t1);
            std::cout << view << std::endl;
            BOOST_TEST((view == make_vector(1, 'x', 3.3, 777)));
        }

        {
            typedef joint_view<t2_type, t1_type> jv_type;
            typedef joint_view<jv_type, jv_type> jv2_type;

            jv_type jv(t2, t1);
            jv2_type jv2(jv, jv);

            std::cout << jv << std::endl;
            std::cout << jv2 << std::endl;

            BOOST_TEST(jv2
                == make_vector(1, 'x', 3.3, 777, 1, 'x', 3.3, 777));
        }

        {
            typedef joint_view<t2_type, t1_type> jt_type;
            typedef joint_view<t1_type, t2_type> jv2_type;
            typedef joint_view<jt_type, jv2_type> jv3_type;

            jt_type jt(t2, t1);
            jv2_type jv2(t1, t2);
            jv3_type jv3(jt, jv2);

            std::cout << jt << std::endl;
            std::cout << jv2 << std::endl;
            std::cout << jv3 << std::endl;

            BOOST_TEST(jv3
                == make_vector(1, 'x', 3.3, 777, 777, 1, 'x', 3.3));
        }

        {
            typedef joint_view<vector<>, t1_type> jt_type;
            vector<> empty;
            jt_type jt(empty, t1);
            std::cout << jt << std::endl;
            BOOST_TEST(jt == make_vector(777));
        }

        {
            typedef joint_view<t1_type, vector<> > jt_type;
            vector<> empty;
            jt_type jt(t1, empty);
            std::cout << jt << std::endl;
            BOOST_TEST(jt == make_vector(777));
        }

        {
            typedef joint_view<vector<>, vector<> > jt_type;
            vector<> empty;
            jt_type jt(empty, empty);
            std::cout << jt << std::endl;
            BOOST_TEST(jt == make_vector());
        }
    }

    return boost::report_errors();
}

