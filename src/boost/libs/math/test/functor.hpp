//  (C) Copyright John Maddock 2007.
//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#include <boost/math/special_functions/trunc.hpp>


#ifndef BOOST_MATH_TEST_FUNCTOR_HPP
#define BOOST_MATH_TEST_FUNCTOR_HPP

struct extract_result_type
{
   extract_result_type(unsigned i) : m_location(i){}

   template <class S>
   typename S::value_type operator()(const S& row)
   {
      return row[m_location];
   }
private:
   unsigned m_location;
};

inline extract_result_type extract_result(unsigned i)
{
   return extract_result_type(i);
}

template <class F>
struct row_binder1
{
   row_binder1(F _f, unsigned i) : f(_f), m_i(i) {}

   template <class S>
   typename S::value_type operator()(const S& row)
   {
      return f(row[m_i]);
   }

private:
   F f;
   unsigned m_i;
};

template<class F>
inline row_binder1<F> bind_func(F f, unsigned i)
{
   return row_binder1<F>(f, i);
}

template <class F>
struct row_binder2
{
   row_binder2(F _f, unsigned i, unsigned j) : f(_f), m_i(i), m_j(j) {}

   template <class S>
   typename S::value_type operator()(const S& row)
   {
      return f(row[m_i], row[m_j]);
   }

private:
   F f;
   unsigned m_i, m_j;
};

template<class F>
inline row_binder2<F> bind_func(F f, unsigned i, unsigned j)
{
   return row_binder2<F>(f, i, j);
}

template <class F>
struct row_binder3
{
   row_binder3(F _f, unsigned i, unsigned j, unsigned k) : f(_f), m_i(i), m_j(j), m_k(k) {}

   template <class S>
   typename S::value_type operator()(const S& row)
   {
      return f(row[m_i], row[m_j], row[m_k]);
   }

private:
   F f;
   unsigned m_i, m_j, m_k;
};

template<class F>
inline row_binder3<F> bind_func(F f, unsigned i, unsigned j, unsigned k)
{
   return row_binder3<F>(f, i, j, k);
}

template <class F>
struct row_binder4
{
   row_binder4(F _f, unsigned i, unsigned j, unsigned k, unsigned l) : f(_f), m_i(i), m_j(j), m_k(k), m_l(l) {}

   template <class S>
   typename S::value_type operator()(const S& row)
   {
      return f(row[m_i], row[m_j], row[m_k], row[m_l]);
   }

private:
   F f;
   unsigned m_i, m_j, m_k, m_l;
};

template<class F>
inline row_binder4<F> bind_func(F f, unsigned i, unsigned j, unsigned k, unsigned l)
{
   return row_binder4<F>(f, i, j, k, l);
}

template <class F>
struct row_binder2_i1
{
   row_binder2_i1(F _f, unsigned i, unsigned j) : f(_f), m_i(i), m_j(j) {}

   template <class S>
   typename S::value_type operator()(const S& row)
   {
      return f(boost::math::itrunc(row[m_i]), row[m_j]);
   }

private:
   F f;
   unsigned m_i, m_j;
};

template<class F>
inline row_binder2_i1<F> bind_func_int1(F f, unsigned i, unsigned j)
{
   return row_binder2_i1<F>(f, i, j);
}

template <class F>
struct row_binder3_i2
{
   row_binder3_i2(F _f, unsigned i, unsigned j, unsigned k) : f(_f), m_i(i), m_j(j), m_k(k) {}

   template <class S>
   typename S::value_type operator()(const S& row)
   {
      return f(
         boost::math::itrunc(row[m_i]), 
         boost::math::itrunc(row[m_j]),
         row[m_k]);
   }

private:
   F f;
   unsigned m_i, m_j, m_k;
};

template<class F>
inline row_binder3_i2<F> bind_func_int2(F f, unsigned i, unsigned j, unsigned k)
{
   return row_binder3_i2<F>(f, i, j, k);
}

template <class F>
struct row_binder4_i2
{
   row_binder4_i2(F _f, unsigned i, unsigned j, unsigned k, unsigned l) : f(_f), m_i(i), m_j(j), m_k(k), m_l(l) {}

   template <class S>
   typename S::value_type operator()(const S& row)
   {
      return f(
         boost::math::itrunc(row[m_i]), 
         boost::math::itrunc(row[m_j]),
         row[m_k],
         row[m_l]);
   }

private:
   F f;
   unsigned m_i, m_j, m_k, m_l;
};

template<class F>
inline row_binder4_i2<F> bind_func_int2(F f, unsigned i, unsigned j, unsigned k, unsigned l)
{
   return row_binder4_i2<F>(f, i, j, k, l);
}

template <class F>
struct negate_type
{
   negate_type(F f) : m_f(f){}

   template <class S>
   typename S::value_type operator()(const S& row)
   {
      return -m_f(row);
   }
private:
   F m_f;
};

template <class F>
inline negate_type<F> negate(F f)
{
   return negate_type<F>(f);
}

#endif
