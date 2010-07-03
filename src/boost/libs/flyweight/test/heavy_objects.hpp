/* Boost.Flyweight basic test template.
 *
 * Copyright 2006-2008 Joaquin M Lopez Munoz.
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *
 * See http://www.boost.org/libs/flyweight for library home page.
 */

#ifndef BOOST_FLYWEIGHT_TEST_HEAVY_OBJECTS_HPP
#define BOOST_FLYWEIGHT_TEST_HEAVY_OBJECTS_HPP

#if defined(_MSC_VER)&&(_MSC_VER>=1200)
#pragma once
#endif

#include <boost/noncopyable.hpp>
#include <iosfwd>
#include <string>

struct texture
{
  texture(const std::string& str=""):str(str){}

  friend bool operator==(
    const texture& x,const texture& y){return x.str==y.str;}
  friend bool operator< (
    const texture& x,const texture& y){return x.str< y.str;}
  friend bool operator!=(
    const texture& x,const texture& y){return x.str!=y.str;}
  friend bool operator> (
    const texture& x,const texture& y){return x.str> y.str;}
  friend bool operator>=(
    const texture& x,const texture& y){return x.str>=y.str;}
  friend bool operator<=(
    const texture& x,const texture& y){return x.str<=y.str;}

  friend std::ostream& operator<<(std::ostream& os,const texture& x)
  {
    return os<<x.str;
  }

  friend std::istream& operator>>(std::istream& is,texture& x)
  {
    return is>>x.str;
  }

  std::string str;
};

struct from_texture_to_string
{
  const std::string& operator()(const texture& x)const{return x.str;}
};

struct factorization:private boost::noncopyable
{
  factorization(int n=0):n(n){}

  friend bool operator==(
    const factorization& x,const factorization& y){return x.n==y.n;}
  friend bool operator< (
    const factorization& x,const factorization& y){return x.n< y.n;}
  friend bool operator!=(
    const factorization& x,const factorization& y){return x.n!=y.n;}
  friend bool operator> (
    const factorization& x,const factorization& y){return x.n> y.n;}
  friend bool operator>=(
    const factorization& x,const factorization& y){return x.n>=y.n;}
  friend bool operator<=(
    const factorization& x,const factorization& y){return x.n<=y.n;}

  friend std::ostream& operator<<(std::ostream& os,const factorization& x)
  {
    return os<<x.n;
  }

  friend std::istream& operator>>(std::istream& is,factorization& x)
  {
    return is>>x.n;
  }

  int n;
};

#endif
