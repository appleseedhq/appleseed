/*=============================================================================
    Copyright (c) 2001-2009 Hartmut Kaiser
    Copyright (c) 2001-2008 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/

#include <boost/config/warning_disable.hpp>
#include <boost/detail/lightweight_test.hpp>

#include <boost/spirit/include/karma.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/bind.hpp>
#include <boost/function_output_iterator.hpp>

#include <sstream>
#include "test.hpp"

using namespace spirit_test;
using namespace boost::spirit;

void read1(int& i)
{
    i = 42;
}

void read2(int& i, unused_type)
{
    i = 42;
}

void read3(int& i, unused_type, bool&)
{
    i = 42;
}

struct read_action
{
    void operator()(int& i, unused_type, unused_type) const
    {
        i = 42;
    }
};

///////////////////////////////////////////////////////////////////////////////
int main()
{
    using namespace boost::spirit::ascii;

    {
        BOOST_TEST(test("{42}", '{' << int_[&read1] << '}'));
        BOOST_TEST(test_delimited("{ 42 } ", '{' << int_[&read1] << '}', space));
        BOOST_TEST(test("{42}", '{' << int_[&read2] << '}'));
        BOOST_TEST(test_delimited("{ 42 } ", '{' << int_[&read2] << '}', space));
        BOOST_TEST(test("{42}", '{' << int_[&read3] << '}'));
        BOOST_TEST(test_delimited("{ 42 } ", '{' << int_[&read3] << '}', space));
    }

    {
        BOOST_TEST(test("{42}", '{' << int_[read_action()] << '}'));
        BOOST_TEST(test_delimited("{ 42 } ", '{' << int_[read_action()] << '}', space));
    }

    {
        BOOST_TEST(test("{42}", '{' << int_[boost::bind(&read1, _1)] << '}'));
        BOOST_TEST(test_delimited("{ 42 } ", 
            '{' << int_[boost::bind(&read1, _1)] << '}', space));
        BOOST_TEST(test("{42}", '{' << int_[boost::bind(&read2, _1, _2)] << '}'));
        BOOST_TEST(test_delimited("{ 42 } ", 
            '{' << int_[boost::bind(&read2, _1, _2)] << '}', space));
        BOOST_TEST(test("{42}", '{' << int_[boost::bind(&read3, _1, _2, _3)] << '}'));
        BOOST_TEST(test_delimited("{ 42 } ", 
            '{' << int_[boost::bind(&read3, _1, _2, _3)] << '}', space));
    }

    {
        namespace lambda = boost::lambda;
        {
            std::stringstream strm("42");
            BOOST_TEST(test("{42}", '{' << int_[strm >> lambda::_1] << '}'));
        }
        {
            std::stringstream strm("42");
            BOOST_TEST(test_delimited("{ 42 } ", 
                '{' << int_[strm >> lambda::_1] << '}', space));
        }
    }

    return boost::report_errors();
}

