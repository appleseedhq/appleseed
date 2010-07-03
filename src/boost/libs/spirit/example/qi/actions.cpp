/*=============================================================================
    Copyright (c) 2001-2008 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/

#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/bind.hpp>

#include <iostream>

// Presented are various ways to attach semantic actions
//  * Using plain function pointer
//  * Using simple function object
//  * Using boost.bind with a plain function
//  * Using boost.bind with a member function
//  * Using boost.lambda

using namespace boost::spirit;

//[tutorial_semantic_action_functions
// A plain function
void write(int const& i)
{
    std::cout << i << std::endl;
}

// A member function
struct writer
{
    void print(int const& i) const
    {
        std::cout << i << std::endl;
    }
};

// A function object
struct write_action
{
    void operator()(int const& i, unused_type, unused_type) const
    {
        std::cout << i << std::endl;
    }
};
//]

int main()
{
    { // example using plain function

        char const *first = "{42}", *last = first + std::strlen(first);
        //[tutorial_attach_actions1
        qi::parse(first, last, '{' >> int_[&write] >> '}');
        //]
    }

    { // example using simple function object

        char const *first = "{43}", *last = first + std::strlen(first);
        //[tutorial_attach_actions2
        qi::parse(first, last, '{' >> int_[write_action()] >> '}');
        //]
    }

    { // example using boost.bind with a plain function

        char const *first = "{44}", *last = first + std::strlen(first);
        //[tutorial_attach_actions3
        qi::parse(first, last, '{' >> int_[boost::bind(&write, _1)] >> '}');
        //]
    }

    { // example using boost.bind with a member function

        char const *first = "{44}", *last = first + std::strlen(first);
        //[tutorial_attach_actions4
        writer w;
        qi::parse(first, last, '{' >> int_[boost::bind(&writer::print, &w, _1)] >> '}');
        //]
    }

    { // example using boost.lambda

        namespace lambda = boost::lambda;
        char const *first = "{45}", *last = first + std::strlen(first);
        using lambda::_1;
        //[tutorial_attach_actions5
        qi::parse(first, last, '{' >> int_[std::cout << _1 << '\n'] >> '}');
        //]
    }

    return 0;
}




