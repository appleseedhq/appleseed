/*=============================================================================
    Copyright (c) 2001-2006 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying 
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
==============================================================================*/
#include <boost/detail/lightweight_test.hpp>
#include <boost/fusion/container/vector/vector.hpp>
#include <boost/fusion/adapted/mpl.hpp>
#include <boost/fusion/sequence/io/out.hpp>
#include <boost/fusion/algorithm/query/ext_/find_if_s.hpp>
#include <boost/fusion/container/ext_/tree.hpp>
#include <boost/fusion/container/generation/make_vector.hpp>
#include <boost/mpl/vector.hpp>
#include <boost/mpl/vector_c.hpp>
#include <boost/mpl/less.hpp>
#include <boost/type_traits/is_same.hpp>

struct X
{
    operator int() const
    {
        return 12345;
    }
};

template<typename Tree>
void 
process_tree(Tree const &tree)
{
    using namespace boost;
    using mpl::_;

    typedef typename fusion::result_of::find_if_s<Tree const, is_same<_,short> >::type short_iter;
    typedef typename fusion::result_of::find_if_s<Tree const, is_same<_,float> >::type float_iter;

    // find_if_s of a segmented data structure returns generic
    // segmented iterators
    short_iter si = fusion::find_if_s<is_same<_,short> >(tree);
    float_iter fi = fusion::find_if_s<is_same<_,float> >(tree);

    // they behave like ordinary Fusion iterators ...
    BOOST_TEST((*si == short('d')));
    BOOST_TEST((*fi == float(1)));
}

int
main()
{
    using namespace boost::fusion;

    {
        using boost::is_same;
        using boost::mpl::_;

        typedef vector<int, char, int, double> vector_type;
        vector_type v(12345, 'x', 678910, 3.36);

        std::cout << *find_if_s<is_same<_, char> >(v) << std::endl;
        BOOST_TEST((*find_if_s<is_same<_, char> >(v) == 'x'));

        std::cout << *find_if_s<is_same<_, int> >(v) << std::endl;
        BOOST_TEST((*find_if_s<is_same<_, int> >(v) == 12345));

        std::cout << *find_if_s<is_same<_, double> >(v) << std::endl;
        BOOST_TEST((*find_if_s<is_same<_, double> >(v) == 3.36));
    }

    {
        using boost::mpl::vector;
        using boost::is_same;
        using boost::mpl::_;

        typedef vector<int, char, X, double> mpl_vec;
        BOOST_TEST((*find_if_s<is_same<_, X> >(mpl_vec()) == 12345));
    }

    {
        using boost::mpl::vector_c;
        using boost::mpl::less;
        using boost::mpl::int_;
        using boost::is_same;
        using boost::mpl::_;

        typedef vector_c<int, 1, 2, 3, 4> mpl_vec;
        BOOST_TEST((*find_if_s<less<_, int_<3> > >(mpl_vec()) == 1));
    }

    {
        process_tree(
            make_tree(
                make_vector(double(0),'B')
              , make_tree(
                    make_vector(1,2,long(3))
                  , make_tree(make_vector('a','b','c'))
                  , make_tree(make_vector(short('d'),'e','f'))
                )
              , make_tree(
                    make_vector(4,5,6)
                  , make_tree(make_vector(float(1),'h','i'))
                  , make_tree(make_vector('j','k','l'))
                )
            )
        );
    }

    return boost::report_errors();
}

