/*=============================================================================
    Copyright (c) 2005 2006 Joel de Guzman
    http://spirit.sourceforge.net/

    Use, modification and distribution is subject to the Boost Software
    License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
    http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#if !defined(BOOST_SPIRIT_QUICKBOOK_POST_PROCESS_HPP)
#define BOOST_SPIRIT_QUICKBOOK_POST_PROCESS_HPP

#include <iostream>
#include <string>

namespace quickbook
{
    int post_process(
        std::string const& in
      , std::ostream& out
      , int indent
      , int linewidth);
}

#endif // BOOST_SPIRIT_QUICKBOOK_POST_PROCESS_HPP

