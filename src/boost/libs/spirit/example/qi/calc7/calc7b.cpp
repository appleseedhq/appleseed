/*=============================================================================
    Copyright (c) 2001-2007 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include "calc7b.hpp"

// This is not really called. Its only purpose is to
// instantiate the constructor of the grammar.
void instantiate_expression()
{
    typedef std::string::const_iterator iterator_type;
    symbols<char, int> vars;
    std::vector<int> code;
    expression<iterator_type> g(code, vars);
}
