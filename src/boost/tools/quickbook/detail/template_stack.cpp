/*=============================================================================
    Copyright (c) 2002 2004 2006 Joel de Guzman
    http://spirit.sourceforge.net/

    Use, modification and distribution is subject to the Boost Software
    License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
    http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include "./template_stack.hpp"

#ifdef BOOST_MSVC
#pragma warning(disable : 4355)
#endif

namespace quickbook
{
    template_stack::template_stack()
        : scope(template_stack::parser(*this))
        , scopes()
    {
        scopes.push_front(template_symbols());
    }
    
    template_symbol* template_stack::find(std::string const& symbol) const
    {
        for (deque::const_iterator i = scopes.begin(); i != scopes.end(); ++i)
        {
            if (template_symbol* ts = boost::spirit::classic::find(*i, symbol.c_str()))
                return ts;
        }
        return 0;
    }

    template_symbol* template_stack::find_top_scope(std::string const& symbol) const
    {
        return boost::spirit::classic::find(scopes.front(), symbol.c_str());
    }

    template_symbols const& template_stack::top() const
    {
        BOOST_ASSERT(!scopes.empty());
        return scopes.front();
    }
    
    void template_stack::add(std::string const& symbol, template_symbol const& ts)
    {
        BOOST_ASSERT(!scopes.empty());
        boost::spirit::classic::add(scopes.front(), symbol.c_str(), ts);
    }
    
    void template_stack::push()
    {
        scopes.push_front(template_symbols());
    }

    void template_stack::pop()
    {
        scopes.pop_front();
    }
}


