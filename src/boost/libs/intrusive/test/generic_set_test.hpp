/////////////////////////////////////////////////////////////////////////////
//
// (C) Copyright Olaf Krzikalla 2004-2006.
// (C) Copyright Ion Gaztanaga  2006-2008.
//
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
//
// See http://www.boost.org/libs/intrusive for documentation.
//
/////////////////////////////////////////////////////////////////////////////
#include <vector>
#include <boost/intrusive/detail/config_begin.hpp>
#include "common_functors.hpp"
#include <boost/detail/lightweight_test.hpp>
#include <boost/intrusive/options.hpp>
#include "test_macros.hpp"
#include "test_container.hpp"
#include "generic_assoc_test.hpp"

namespace boost{
namespace intrusive{
namespace test{

template<class ValueTraits, template <class = ::boost::intrusive::none, class = ::boost::intrusive::none, class = ::boost::intrusive::none, class = ::boost::intrusive::none> class ContainerDefiner>
struct test_generic_set 
{
   typedef typename ValueTraits::value_type value_type;
   static void test_all();
   static void test_sort(std::vector<value_type>& values);
   static void test_insert(std::vector<value_type>& values);
   static void test_swap(std::vector<value_type>& values);
   static void test_find(std::vector<value_type>& values);
   static void test_impl();
};


template<class ValueTraits, template <class = ::boost::intrusive::none, class = ::boost::intrusive::none, class = ::boost::intrusive::none, class = ::boost::intrusive::none> class ContainerDefiner>
void test_generic_set<ValueTraits, ContainerDefiner>::test_all()
{
   typedef typename ValueTraits::value_type value_type;
   static const int random_init[6] = { 3, 2, 4, 1, 5, 2 };
   std::vector<value_type> values (6);
   for (int i = 0; i < 6; ++i)
      values[i].value_ = random_init[i]; 

   typedef typename ContainerDefiner
      < value_type
      , value_traits<ValueTraits>
      , constant_time_size<value_type::constant_time_size>
      >::type set_type;
   {
      set_type testset(values.begin(), values.end());
      test::test_container(testset);
      testset.clear();
      testset.insert(values.begin(), values.end());
      test::test_common_unordered_and_associative_container(testset, values);
      testset.clear();
      testset.insert(values.begin(), values.end());
      test::test_associative_container(testset, values);
      testset.clear();
      testset.insert(values.begin(), values.end());
      test::test_unique_container(testset, values);
   }
   test_sort(values);
   test_insert(values);
   test_swap(values);
   test_find(values);
   test_impl();
   test_generic_assoc<ValueTraits, ContainerDefiner>::test_all(values);
}

//test case due to an error in tree implementation:
template<class ValueTraits, template <class = ::boost::intrusive::none, class = ::boost::intrusive::none, class = ::boost::intrusive::none, class = ::boost::intrusive::none> class ContainerDefiner>
void test_generic_set<ValueTraits, ContainerDefiner>::test_impl()
{
   typedef typename ValueTraits::value_type value_type;
   std::vector<value_type> values (5);
   for (int i = 0; i < 5; ++i)
      values[i].value_ = i; 

   typedef typename ValueTraits::value_type value_type;
   typedef typename ContainerDefiner
      < value_type
      , value_traits<ValueTraits>
      , constant_time_size<value_type::constant_time_size>
      >::type set_type;
   set_type testset;
   for (int i = 0; i < 5; ++i)
      testset.insert (values[i]);

   testset.erase (testset.iterator_to (values[0]));
   testset.erase (testset.iterator_to (values[1]));
   testset.insert (values[1]);
     
   testset.erase (testset.iterator_to (values[2]));
   testset.erase (testset.iterator_to (values[3]));
}

//test: constructor, iterator, clear, reverse_iterator, front, back, size:
template<class ValueTraits, template <class = ::boost::intrusive::none, class = ::boost::intrusive::none, class = ::boost::intrusive::none, class = ::boost::intrusive::none> class ContainerDefiner>
void test_generic_set<ValueTraits, ContainerDefiner>::test_sort(std::vector<typename ValueTraits::value_type>& values)
{
   typedef typename ValueTraits::value_type value_type;
   typedef typename ContainerDefiner
      < value_type
      , value_traits<ValueTraits>
      , constant_time_size<value_type::constant_time_size>
      >::type set_type;
   set_type testset1 (values.begin(), values.end());
   {  int init_values [] = { 1, 2, 3, 4, 5 };
      TEST_INTRUSIVE_SEQUENCE( init_values, testset1.begin() );  }

   testset1.clear();
   BOOST_TEST (testset1.empty());

   typedef typename ValueTraits::value_type value_type;
   typedef typename ContainerDefiner
      < value_type
      , compare<even_odd>
      , value_traits<ValueTraits>
      , constant_time_size<value_type::constant_time_size>
      >::type set_type2;
   set_type2 testset2 (&values[0], &values[0] + 6);
   {  int init_values [] = { 5, 3, 1, 4, 2 };
      TEST_INTRUSIVE_SEQUENCE( init_values, testset2.rbegin() );  }
   BOOST_TEST (testset2.begin()->value_ == 2);
   BOOST_TEST (testset2.rbegin()->value_ == 5);
}  
  
//test: insert, const_iterator, const_reverse_iterator, erase, s_iterator_to:
template<class ValueTraits, template <class = ::boost::intrusive::none, class = ::boost::intrusive::none, class = ::boost::intrusive::none, class = ::boost::intrusive::none> class ContainerDefiner>
void test_generic_set<ValueTraits, ContainerDefiner>::test_insert(std::vector<typename ValueTraits::value_type>& values)
{
   typedef typename ValueTraits::value_type value_type;
   typedef typename ContainerDefiner
      < value_type
      , value_traits<ValueTraits>
      , constant_time_size<value_type::constant_time_size>
      >::type set_type;
   set_type testset;
   testset.insert(&values[0] + 2, &values[0] + 5);

   const set_type& const_testset = testset;
   {  int init_values [] = { 1, 4, 5 };
      TEST_INTRUSIVE_SEQUENCE( init_values, const_testset.begin() );  }

   typename set_type::iterator i = testset.begin();
   BOOST_TEST (i->value_ == 1);

   i = testset.insert (i, values[0]);
   BOOST_TEST (&*i == &values[0]);

   {  int init_values [] = { 5, 4, 3, 1 };
      TEST_INTRUSIVE_SEQUENCE( init_values, testset.rbegin() );  }

   i = testset.iterator_to (values[2]);
   BOOST_TEST (&*i == &values[2]);

   i = set_type::s_iterator_to(values[2]);
   BOOST_TEST (&*i == &values[2]);

   testset.erase (i);
   {  int init_values [] = { 1, 3, 5 };
      TEST_INTRUSIVE_SEQUENCE( init_values, testset.begin() );  }
}  

//test: insert (seq-version), swap, erase (seq-version), size:
template<class ValueTraits, template <class = ::boost::intrusive::none, class = ::boost::intrusive::none, class = ::boost::intrusive::none, class = ::boost::intrusive::none> class ContainerDefiner>
void test_generic_set<ValueTraits, ContainerDefiner>::test_swap(std::vector<typename ValueTraits::value_type>& values)
{
   typedef typename ValueTraits::value_type value_type;
   typedef typename ContainerDefiner
      < value_type
      , value_traits<ValueTraits>
      , constant_time_size<value_type::constant_time_size>
      >::type set_type;
   set_type testset1 (&values[0], &values[0] + 2);
   set_type testset2;
   testset2.insert (&values[0] + 2, &values[0] + 6);
   testset1.swap (testset2);

   {  int init_values [] = { 1, 2, 4, 5 };
      TEST_INTRUSIVE_SEQUENCE( init_values, testset1.begin() );  }

   {  int init_values [] = { 2, 3 };
      TEST_INTRUSIVE_SEQUENCE( init_values, testset2.begin() );  }

   testset1.erase (testset1.iterator_to(values[5]), testset1.end());
   BOOST_TEST (testset1.size() == 1);
   //  BOOST_TEST (&testset1.front() == &values[3]);
   BOOST_TEST (&*testset1.begin() == &values[3]);
}  

//test: find, equal_range (lower_bound, upper_bound):
template<class ValueTraits, template <class = ::boost::intrusive::none, class = ::boost::intrusive::none, class = ::boost::intrusive::none, class = ::boost::intrusive::none> class ContainerDefiner>
void test_generic_set<ValueTraits, ContainerDefiner>::test_find(std::vector<typename ValueTraits::value_type>& values)
{
   typedef typename ValueTraits::value_type value_type;
   typedef typename ContainerDefiner
      < value_type
      , value_traits<ValueTraits>
      , constant_time_size<value_type::constant_time_size>
      >::type set_type;
   set_type testset (values.begin(), values.end());
   typedef typename set_type::iterator iterator;

   value_type cmp_val;
   cmp_val.value_ = 2;
   iterator i = testset.find (cmp_val);
   BOOST_TEST (i->value_ == 2);
   BOOST_TEST ((++i)->value_ != 2);
   std::pair<iterator,iterator> range = testset.equal_range (cmp_val);
     
   BOOST_TEST (range.first->value_ == 2);
   BOOST_TEST (range.second->value_ == 3);
   BOOST_TEST (std::distance (range.first, range.second) == 1);

   cmp_val.value_ = 7;
   BOOST_TEST (testset.find (cmp_val) == testset.end());
} 

}}}   //namespace boost::intrusive::test

#include <boost/intrusive/detail/config_end.hpp>
