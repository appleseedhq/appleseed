//Copyright (c) 2006-2009 Emil Dotchevski and Reverge Studios, Inc.

//Distributed under the Boost Software License, Version 1.0. (See accompanying
//file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#include <boost/exception/get_error_info.hpp>
#include <boost/exception/info.hpp>
#include <boost/detail/lightweight_test.hpp>
#include <boost/detail/workaround.hpp>
#include <errno.h>

#if BOOST_WORKAROUND(__CODEGEARC__, BOOST_TESTED_AT(0x610))
struct tag_errno {};
#endif

typedef boost::error_info<struct tag_errno,int> info_errno;

class
my_exception:
    public boost::exception
    {
    };

int
main()
    {
    try
        {
        errno=1;
        throw my_exception() << info_errno(errno);
        BOOST_TEST(false);
        }
    catch(
    my_exception & x )
        {
        BOOST_TEST(1==*boost::get_error_info<info_errno>(x));
        }
    return boost::report_errors();
    }
