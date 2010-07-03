/* Copyright 2006-2008 Joaquin M Lopez Munoz.
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *
 * See http://www.boost.org/libs/flyweight for library home page.
 */

#ifndef BOOST_FLYWEIGHT_DETAIL_HANDLE_FACTORY_ADAPTOR_HPP
#define BOOST_FLYWEIGHT_DETAIL_HANDLE_FACTORY_ADAPTOR_HPP

#if defined(_MSC_VER)&&(_MSC_VER>=1200)
#pragma once
#endif

/* Given a Factory and a Handle type constructible from and implicitly
 * convertible to Factory::handle_type, handle_factory_adaptor
 * adapts Factory to present Handle as the associated handle_type.
 */
 
namespace boost{

namespace flyweights{

namespace detail{

template<typename Factory,typename Handle,typename Entry>
struct handle_factory_adaptor:Factory
{
public:
  typedef Handle handle_type;

  handle_type insert(const Entry& x)
  {
    return static_cast<handle_type>(base().insert(x));
  }

private:
  Factory& base(){return *this;}
};

} /* namespace flyweights::detail */

} /* namespace flyweights */

} /* namespace boost */

#endif
