/*
 *
 * Copyright (c) 2004
 * John Maddock
 *
 * Use, modification and distribution are subject to the 
 * Boost Software License, Version 1.0. (See accompanying file 
 * LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 *
 */
 
 /*
  *   LOCATION:    see http://www.boost.org for most recent version.
  *   FILE         unicode_iterator_test.cpp
  *   VERSION      see <boost/version.hpp>
  *   DESCRIPTION: Simple test suite for Unicode interconversions.
  */

#include <boost/regex/pending/unicode_iterator.hpp>
#include <boost/test/included/test_exec_monitor.hpp>
#include <vector>
#include <iterator>
#include <algorithm>

void spot_checks()
{
   // test specific values ripped straight out of the Unicode standard
   // to verify that our encoding is the same as theirs, as well as 
   // self-consistent:
   ::boost::uint32_t spot16[] = { 0x10302u, };
   typedef boost::u32_to_u16_iterator<const ::boost::uint32_t*> u32to16type;

   u32to16type it(spot16);
   BOOST_CHECK_EQUAL(*it++, 0xD800u);
   BOOST_CHECK_EQUAL(*it++, 0xDF02u);
   BOOST_CHECK_EQUAL(*--it, 0xDF02u);
   BOOST_CHECK_EQUAL(*--it, 0xD800u);

   ::boost::uint32_t spot8[] = { 0x004Du, 0x0430u, 0x4E8Cu, 0x10302u, };
   typedef boost::u32_to_u8_iterator<const ::boost::uint32_t*> u32to8type;

   u32to8type it8(spot8);
   BOOST_CHECK_EQUAL(*it8++, 0x4Du);
   BOOST_CHECK_EQUAL(*it8++, 0xD0u);
   BOOST_CHECK_EQUAL(*it8++, 0xB0u);
   BOOST_CHECK_EQUAL(*it8++, 0xE4u);
   BOOST_CHECK_EQUAL(*it8++, 0xBAu);
   BOOST_CHECK_EQUAL(*it8++, 0x8Cu);
   BOOST_CHECK_EQUAL(*it8++, 0xF0u);
   BOOST_CHECK_EQUAL(*it8++, 0x90u);
   BOOST_CHECK_EQUAL(*it8++, 0x8Cu);
   BOOST_CHECK_EQUAL(*it8++, 0x82u);

   BOOST_CHECK_EQUAL(*--it8, 0x82u);
   BOOST_CHECK_EQUAL(*--it8, 0x8Cu);
   BOOST_CHECK_EQUAL(*--it8, 0x90u);
   BOOST_CHECK_EQUAL(*--it8, 0xF0u);
   BOOST_CHECK_EQUAL(*--it8, 0x8Cu);
   BOOST_CHECK_EQUAL(*--it8, 0xBAu);
   BOOST_CHECK_EQUAL(*--it8, 0xE4u);
   BOOST_CHECK_EQUAL(*--it8, 0xB0u);
   BOOST_CHECK_EQUAL(*--it8, 0xD0u);
   BOOST_CHECK_EQUAL(*--it8, 0x4Du);
}

void test(const std::vector< ::boost::uint32_t>& v)
{
   typedef std::vector< ::boost::uint32_t> vector32_type;
   typedef std::vector< ::boost::uint16_t> vector16_type;
   typedef std::vector< ::boost::uint8_t>  vector8_type;
   typedef boost::u32_to_u16_iterator<vector32_type::const_iterator, ::boost::uint16_t> u32to16type;
   typedef boost::u16_to_u32_iterator<vector16_type::const_iterator, ::boost::uint32_t> u16to32type;
#if !defined(BOOST_NO_TEMPLATE_PARTIAL_SPECIALIZATION) && !defined(BOOST_NO_STD_ITERATOR) && !defined(_RWSTD_NO_CLASS_PARTIAL_SPEC)
   typedef std::reverse_iterator<u32to16type> ru32to16type;
   typedef std::reverse_iterator<u16to32type> ru16to32type;
#endif
   typedef boost::u32_to_u8_iterator<vector32_type::const_iterator, ::boost::uint8_t> u32to8type;
   typedef boost::u8_to_u32_iterator<vector8_type::const_iterator, ::boost::uint32_t> u8to32type;
#if !defined(BOOST_NO_TEMPLATE_PARTIAL_SPECIALIZATION) && !defined(BOOST_NO_STD_ITERATOR) && !defined(_RWSTD_NO_CLASS_PARTIAL_SPEC)
   typedef std::reverse_iterator<u32to8type> ru32to8type;
   typedef std::reverse_iterator<u8to32type> ru8to32type;
#endif
   vector8_type  v8;
   vector16_type v16;
   vector32_type v32;
   vector32_type::const_iterator i, j, k;
   //
   // begin by testing forward iteration, of 32-16 bit interconversions:
   //
#if !defined(BOOST_NO_TEMPLATED_ITERATOR_CONSTRUCTORS)
   v16.assign(u32to16type(v.begin()), u32to16type(v.end()));
#else
   v16.clear();
   std::copy(u32to16type(v.begin()), u32to16type(v.end()), std::back_inserter(v16));
#endif
#ifndef BOOST_NO_STD_DISTANCE
   BOOST_CHECK_EQUAL(std::distance(u32to16type(v.begin()), u32to16type(v.end())), v16.size());
#endif
#if !defined(BOOST_NO_TEMPLATED_ITERATOR_CONSTRUCTORS)
   v32.assign(u16to32type(v16.begin()), u16to32type(v16.end()));
#else
   v32.clear();
   std::copy(u16to32type(v16.begin()), u16to32type(v16.end()), std::back_inserter(v32));
#endif
#ifndef BOOST_NO_STD_DISTANCE
   BOOST_CHECK_EQUAL(std::distance(u16to32type(v16.begin()), u16to32type(v16.end())), v32.size());
#endif
   BOOST_CHECK_EQUAL(v.size(), v32.size());
   i = v.begin();
   j = i;
   std::advance(j, (std::min)(v.size(), v32.size()));
   k = v32.begin();
   BOOST_CHECK_EQUAL_COLLECTIONS(v.begin(), v.end(), v32.begin(), v32.end());
   //
   // test backward iteration, of 32-16 bit interconversions:
   //
#if !defined(BOOST_NO_TEMPLATE_PARTIAL_SPECIALIZATION) && !defined(BOOST_NO_STD_ITERATOR) && !defined(_RWSTD_NO_CLASS_PARTIAL_SPEC)
   v16.assign(ru32to16type(u32to16type(v.end())), ru32to16type(u32to16type(v.begin())));
#ifndef BOOST_NO_STD_DISTANCE
   BOOST_CHECK_EQUAL(std::distance(ru32to16type(u32to16type(v.end())), ru32to16type(u32to16type(v.begin()))), v16.size());
#endif
   std::reverse(v16.begin(), v16.end());
   v32.assign(ru16to32type(u16to32type(v16.end())), ru16to32type(u16to32type(v16.begin())));
#ifndef BOOST_NO_STD_DISTANCE
   BOOST_CHECK_EQUAL(std::distance(ru16to32type(u16to32type(v16.end())), ru16to32type(u16to32type(v16.begin()))), v32.size());
#endif
   BOOST_CHECK_EQUAL(v.size(), v32.size());
   std::reverse(v32.begin(), v32.end());
   i = v.begin();
   j = i;
   std::advance(j, (std::min)(v.size(), v32.size()));
   k = v32.begin();
   BOOST_CHECK_EQUAL_COLLECTIONS(v.begin(), v.end(), v32.begin(), v32.end());
#endif
   //
   // Test forward iteration, of 32-8 bit interconversions:
   //
#if !defined(BOOST_NO_TEMPLATED_ITERATOR_CONSTRUCTORS)
   v8.assign(u32to8type(v.begin()), u32to8type(v.end()));
#else
   v8.clear();
   std::copy(u32to8type(v.begin()), u32to8type(v.end()), std::back_inserter(v8));
#endif
#ifndef BOOST_NO_STD_DISTANCE
   BOOST_CHECK_EQUAL(std::distance(u32to8type(v.begin()), u32to8type(v.end())), v8.size());
#endif
#if !defined(BOOST_NO_TEMPLATED_ITERATOR_CONSTRUCTORS)
   v32.assign(u8to32type(v8.begin()), u8to32type(v8.end()));
#else
   v32.clear();
   std::copy(u8to32type(v8.begin()), u8to32type(v8.end()), std::back_inserter(v32));
#endif
#ifndef BOOST_NO_STD_DISTANCE
   BOOST_CHECK_EQUAL(std::distance(u8to32type(v8.begin()), u8to32type(v8.end())), v32.size());
#endif
   BOOST_CHECK_EQUAL(v.size(), v32.size());
   i = v.begin();
   j = i;
   std::advance(j, (std::min)(v.size(), v32.size()));
   k = v32.begin();
   BOOST_CHECK_EQUAL_COLLECTIONS(v.begin(), v.end(), v32.begin(), v32.end());
   //
   // test backward iteration, of 32-8 bit interconversions:
   //
#if !defined(BOOST_NO_TEMPLATE_PARTIAL_SPECIALIZATION) && !defined(BOOST_NO_STD_ITERATOR) && !defined(_RWSTD_NO_CLASS_PARTIAL_SPEC)
   v8.assign(ru32to8type(u32to8type(v.end())), ru32to8type(u32to8type(v.begin())));
#ifndef BOOST_NO_STD_DISTANCE
   BOOST_CHECK_EQUAL(std::distance(ru32to8type(u32to8type(v.end())), ru32to8type(u32to8type(v.begin()))), v8.size());
#endif
   std::reverse(v8.begin(), v8.end());
   v32.assign(ru8to32type(u8to32type(v8.end())), ru8to32type(u8to32type(v8.begin())));
#ifndef BOOST_NO_STD_DISTANCE
   BOOST_CHECK_EQUAL(std::distance(ru8to32type(u8to32type(v8.end())), ru8to32type(u8to32type(v8.begin()))), v32.size());
#endif
   BOOST_CHECK_EQUAL(v.size(), v32.size());
   std::reverse(v32.begin(), v32.end());
   i = v.begin();
   j = i;
   std::advance(j, (std::min)(v.size(), v32.size()));
   k = v32.begin();
   BOOST_CHECK_EQUAL_COLLECTIONS(v.begin(), v.end(), v32.begin(), v32.end());
#endif
}

int test_main( int, char* [] ) 
{
   // test specific value points from the standard:
   spot_checks();
   // now test a bunch of values for self-consistency and round-tripping:
   std::vector< ::boost::uint32_t> v;
   // start with boundary conditions:
   v.push_back(0);
   v.push_back(0xD7FF);
   v.push_back(0xE000);
   v.push_back(0xFFFF);
   v.push_back(0x10000);
   v.push_back(0x10FFFF);
   v.push_back(0x80u);
   v.push_back(0x80u - 1);
   v.push_back(0x800u);
   v.push_back(0x800u - 1);
   v.push_back(0x10000u);
   v.push_back(0x10000u - 1);
   test(v);
   return 0;
}

#include <boost/test/included/test_exec_monitor.hpp>
