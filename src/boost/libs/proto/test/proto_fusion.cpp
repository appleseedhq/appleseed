///////////////////////////////////////////////////////////////////////////////
// proto_fusion.cpp
//
//  Copyright 2008 Eric Niebler. Distributed under the Boost
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#include <boost/version.hpp>
#if BOOST_VERSION < 103500
# error This test only works on Boost v1.35
#endif

#include <boost/proto/core.hpp>
#include <boost/proto/fusion.hpp>
#include <boost/fusion/include/for_each.hpp>
#include <boost/test/unit_test.hpp>
#include <boost/utility/addressof.hpp>
#include <sstream>

boost::proto::terminal<char>::type a_ = {'a'};
boost::proto::terminal<char>::type b_ = {'b'};
boost::proto::terminal<char>::type c_ = {'c'};
boost::proto::terminal<char>::type d_ = {'d'};
boost::proto::terminal<char>::type e_ = {'e'};
boost::proto::terminal<char>::type f_ = {'f'};
boost::proto::terminal<char>::type g_ = {'g'};
boost::proto::terminal<char>::type h_ = {'h'};
boost::proto::terminal<char>::type i_ = {'i'};

std::ostream &operator <<(std::ostream &sout, boost::proto::tag::shift_right)
{
    return sout << ">>";
}

std::ostream &operator <<(std::ostream &sout, boost::proto::tag::bitwise_or)
{
    return sout << "|";
}

template<typename Args>
std::ostream &operator <<(std::ostream &sout, boost::proto::expr<boost::proto::tag::terminal, Args, 0> const *op)
{
    return sout << boost::proto::value(*op);
}

template<typename Tag, typename Args>
std::ostream &operator <<(std::ostream &sout, boost::proto::expr<Tag, Args, 1> const *op)
{
    return sout << Tag() << boost::addressof(boost::proto::child(*op));
}

template<typename Tag, typename Args>
std::ostream &operator <<(std::ostream &sout, boost::proto::expr<Tag, Args, 2> const *op)
{
    return sout << boost::addressof(boost::proto::left(*op)) << Tag() << boost::addressof(boost::proto::right(*op));
}

///////////////////////////////////////////////////////////////////////////////
// to_string
//
struct to_string
{
    to_string(std::ostream &sout)
      : sout_(sout)
    {}

    template<typename Op>
    void operator ()(Op const &op) const
    {
        this->sout_ << '(' << boost::addressof(op) << ')';
    }
private:
    std::ostream &sout_;
};

void test1()
{
    using boost::proto::flatten;

    std::stringstream sout;

    // Test for 1-way branching "tree"
    sout.str("");
    boost::fusion::for_each(flatten(!!!!(a_ >> b_)), to_string(sout));
    BOOST_CHECK_EQUAL("(a>>b)", sout.str());

    // Tests for 2-way branching trees
    sout.str("");
    boost::fusion::for_each(flatten(a_ >> b_ >> c_), to_string(sout));
    BOOST_CHECK_EQUAL("(a)(b)(c)", sout.str());

    sout.str("");
    boost::fusion::for_each(flatten(a_ | b_ | c_), to_string(sout));
    BOOST_CHECK_EQUAL("(a)(b)(c)", sout.str());

    sout.str("");
    boost::fusion::for_each(flatten(a_ >> b_ | c_ >> d_), to_string(sout));
    BOOST_CHECK_EQUAL("(a>>b)(c>>d)", sout.str());

    sout.str("");
    boost::fusion::for_each(flatten(a_ | b_ >> c_ | d_), to_string(sout));
    BOOST_CHECK_EQUAL("(a)(b>>c)(d)", sout.str());

    sout.str("");
    boost::fusion::for_each(flatten(a_ >> b_ | c_ >> d_ | e_ >> f_ >> g_), to_string(sout));
    BOOST_CHECK_EQUAL("(a>>b)(c>>d)(e>>f>>g)", sout.str());

    sout.str("");
    boost::fusion::for_each(flatten(a_ >> b_ | c_ >> d_ | e_ >> (f_ | g_) >> h_), to_string(sout));
    BOOST_CHECK_EQUAL("(a>>b)(c>>d)(e>>f|g>>h)", sout.str());

    // Test for n-way branching tree
    sout.str("");
    boost::fusion::for_each(flatten(a_(b_(c_ >> d_, e_ | f_), g_ >> h_)(i_)), to_string(sout));
    BOOST_CHECK_EQUAL("(a)(b)(c>>d)(e|f)(g>>h)(i)", sout.str());
}

using namespace boost::unit_test;
///////////////////////////////////////////////////////////////////////////////
// init_unit_test_suite
//
test_suite* init_unit_test_suite( int argc, char* argv[] )
{
    test_suite *test = BOOST_TEST_SUITE("test proto and fusion integration");

    test->add(BOOST_TEST_CASE(&test1));

    return test;
}
