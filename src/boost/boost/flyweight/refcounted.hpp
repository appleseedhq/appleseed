/* Copyright 2006-2009 Joaquin M Lopez Munoz.
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *
 * See http://www.boost.org/libs/flyweight for library home page.
 */

#ifndef BOOST_FLYWEIGHT_REFCOUNTED_HPP
#define BOOST_FLYWEIGHT_REFCOUNTED_HPP

#if defined(_MSC_VER)&&(_MSC_VER>=1200)
#pragma once
#endif

#include <boost/config.hpp> /* keep it first to prevent nasty warns in MSVC */
#include <algorithm>
#include <boost/assert.hpp>
#include <boost/detail/atomic_count.hpp>
#include <boost/detail/workaround.hpp>
#include <boost/flyweight/refcounted_fwd.hpp>
#include <boost/flyweight/tracking_tag.hpp>
#include <boost/utility/swap.hpp>

/* Refcounting tracking policy.
 * The implementation deserves some explanation; values are equipped with a
 * reference count with the following semantics:
 *   - 0: newly created value.
 *   - n: (n-1) active references to the value.
 * When the number of references reaches zero, the value can be erased. The
 * exact protocol, however, is a little more complicated to avoid data races
 * like the following:
 *   - Thread A detaches the last reference to x and is preempted.
 *   - Thread B looks for x, finds it and attaches a reference to it.
 *   - Thread A resumes and proceeds with erasing x, leaving a dangling
 *     reference in thread B.
 * To cope with this, values are equipped with an additional count of threads
 * preempted during erasure. Such a preemption are detected by the preempting
 * thread by checking whether the reference count of the object is 1 (hence
 * the uncommon refcounting semantics distinguishing between a newly created
 * value and a value with no active references.
 */

namespace boost{

namespace flyweights{

namespace detail{

template<typename Value,typename Key>
class refcounted_value
{
public:
  explicit refcounted_value(const Value& x_):
    x(x_),ref(0),del_ref(0)
  {}
  
  refcounted_value(const refcounted_value& r):
    x(r.x),ref(0),del_ref(0)
  {}

  ~refcounted_value()
  {
    /* count()>1 most likely indicates that the flyweight factory
     * has been destructed before some of the flyweight objects using
     * it. Check for static initialization order problems with this
     * flyweight type.
     */

    BOOST_ASSERT(count()<=1);
  }

  refcounted_value& operator=(const refcounted_value& r)
  {
    x=r.x;
    return *this;
  }
  
  operator const Value&()const{return x;}
  operator const Key&()const{return x;}
    
#if !defined(BOOST_NO_MEMBER_TEMPLATE_FRIENDS)
private:
  template<typename,typename> friend class refcounted_handle;
#endif

  long count()const{return ref;}
  long add_ref()const{return ++ref;}
  bool release()const{return (--ref==1);}

  long count_deleters()const{return del_ref;}
  void add_deleter()const{++del_ref;}
  void release_deleter()const{--del_ref;}

private:
  Value                               x;
  mutable boost::detail::atomic_count ref;
  mutable long                        del_ref;
};

template<typename Handle,typename TrackingHelper>
class refcounted_handle
{
public:
  explicit refcounted_handle(const Handle& h_):h(h_)
  {
    switch(TrackingHelper::entry(*this).add_ref()){
      case 1: /* newly created object, make count()==2 (1 active reference) */
        TrackingHelper::entry(*this).add_ref();
        break;
      case 2: /* object was about to be erased, increment the deleter count */
        TrackingHelper::entry(*this).add_deleter();
        break;
      default:break;
    }
  }
  
  refcounted_handle(const refcounted_handle& x):h(x.h)
  {
    TrackingHelper::entry(*this).add_ref();
  }

  refcounted_handle& operator=(refcounted_handle x)
  {
    swap(*this,x);
    return *this;
  }

  ~refcounted_handle()
  {
    if(TrackingHelper::entry(*this).release()){
      TrackingHelper::erase(*this,check_erase);
    }
  }

  operator const Handle&()const{return h;}

  friend void swap(refcounted_handle& x, refcounted_handle& y)
  {
    boost::swap(x.h,y.h);
  }

private:
  static bool check_erase(const refcounted_handle& x)
  {
    if(TrackingHelper::entry(x).count_deleters()){
      TrackingHelper::entry(x).release_deleter();
      return false;
    }
    return true;
  }

  Handle h;
};

} /* namespace flyweights::detail */

struct refcounted:tracking_marker
{
  struct entry_type
  {
    template<typename Value,typename Key>
    struct apply
    {
      typedef detail::refcounted_value<Value,Key> type;
    };
  };

  struct handle_type
  {
    template<typename Handle,typename TrackingHelper>
    struct apply
    {
      typedef detail::refcounted_handle<Handle,TrackingHelper> type;
    };
  };
};

} /* namespace flyweights */

} /* namespace boost */

#endif
