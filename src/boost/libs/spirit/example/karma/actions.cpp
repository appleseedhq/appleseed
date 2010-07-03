/*=============================================================================
    Copyright (c) 2001-2008 Hartmut Kaiser
    Copyright (c) 2001-2008 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/

#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/karma.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/bind.hpp>

#include <iostream>
#include <sstream>

// Presented are various ways to attach semantic actions
//  * Using plain function pointer
//  * Using simple function object
//  * Using boost.bind
//  * Using boost.lambda

using namespace boost::spirit;

void read(int& i)
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
    { // example using plain functions
        std::string generated;
        bool result = karma::generate(std::back_inserter(generated), 
            '{' << int_[&read] << '}');

        if (result)
            std::cout << "Simple function: " << generated << std::endl;
    }

    { // example using simple function objects
        std::string generated;
        bool result = karma::generate(std::back_inserter(generated), 
            '{' << int_[read_action()] << '}');

        if (result)
            std::cout << "Simple function object: " << generated << std::endl;
    }

    { // example using boost.bind
        std::string generated;
        bool result = karma::generate(std::back_inserter(generated), 
            '{' << int_[boost::bind(&read, _1)] << '}');

        if (result)
            std::cout << "Boost.Bind: " << generated << std::endl;
    }

    { // example using boost.lambda
        namespace lambda = boost::lambda;

        std::string generated;
        std::stringstream strm("42");

        bool result = karma::generate(std::back_inserter(generated),
            '{' << int_[strm >> lambda::_1] << '}');

        if (result)
            std::cout << "Boost.Lambda: " << generated << std::endl;
    }

    return 0;
}

