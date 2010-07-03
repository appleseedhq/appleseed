/*=============================================================================
    Copyright (c) 2009 Daniel James

    Use, modification and distribution is subject to the Boost Software
    License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
    http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/

#include <boost/program_options.hpp>
#include "./input_path.hpp"

#if defined(__cygwin__) || defined(__CYGWIN__)
#include <boost/filesystem/config.hpp>
#include <windows.h>
#include <sys/cygwin.h>
#endif


namespace quickbook { namespace detail
{
    void validate(boost::any& v,
            const std::vector<std::string>& values,
            input_path*, int)
    {
        std::string path
            = boost::program_options::validators::get_single_string(values);

#if !(defined(__cygwin__) || defined(__CYGWIN__))
        v = input_path(path);
#elif defined(BOOST_WINDOWS_PATH)
        char result[MAX_PATH + 1];
        cygwin_conv_to_win32_path(path.c_str(), result);
        v = input_path(result);
#elif defined(BOOST_POSIX_PATH)
        char result[MAX_PATH + 1];
        cygwin_conv_to_posix_path(path.c_str(), result);
        v = input_path(result);
#else
#    error "Bosot filesystem path type doesn't seem to be set."
#endif
    }
}}