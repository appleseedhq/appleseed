//-----------------------------------------------------------------------------
// boost-libs variant/test/recursive_variant_test.cpp source file
// See http://www.boost.org for updates, documentation, and revision history.
//-----------------------------------------------------------------------------
//
// Copyright (c) 2003
// Eric Friedman, Itay Maman
//
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#include "boost/test/minimal.hpp"
#include "boost/variant.hpp"

#include <iostream>
#include <sstream>
#include <vector>

struct vector_printer
    : boost::static_visitor<std::string>
{
    template <typename T>
    std::string operator()(const std::vector<T>& vec) const
    {
        std::ostringstream ost;

        ost << "( ";

        typename std::vector<T>::const_iterator it = vec.begin();
        for (; it != vec.end(); ++it)
            ost << boost::apply_visitor( vector_printer(), *it );

        ost << ") ";

        return ost.str();
    }

    template <typename T>
    std::string operator()(const T& operand) const
    {
        std::ostringstream ost;
        ost << operand << ' ';
        return ost.str();
    }
};

int test_main(int , char* [])
{
    typedef boost::make_recursive_variant<
          int
        , std::vector<boost::recursive_variant_>
        >::type var_t;

    std::vector<var_t> vec;
    vec.push_back(3);
    vec.push_back(5);
    vec.push_back(vec);
    vec.push_back(7);

    var_t var(vec);
    std::string result( boost::apply_visitor( vector_printer(), var ) );

    std::cout << "result: " << result << '\n';
    BOOST_CHECK(result == "( 3 5 ( 3 5 ) 7 ) ");

    return boost::exit_success;
}
