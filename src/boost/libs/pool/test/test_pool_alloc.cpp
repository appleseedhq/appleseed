// Copyright (C) 2000, 2001 Stephen Cleary
//
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

// Uncomment this to stub out all MT locking
#define BOOST_NO_MT

#include <boost/pool/pool_alloc.hpp>
#include <boost/pool/object_pool.hpp>

#include <stdlib.h>
#include <stdexcept>
#include <iostream>
#include <vector>
#include <algorithm>
#include <list>
#include <deque>
#include <set>

// use of test_main() eases automatic regression testing by eliminating
// the need for manual intervention on test failures (Beman Dawes)
#include <boost/test/test_tools.hpp>

// VERBOSE will print out trace-like statements to show exactly what this
//  test file is doing.
//#define VERBOSE

// Each "tester" object below checks into and out of the "cdtor_checker",
//  which will report any problems related to the construction/destruction of
//  "tester" objects.
class cdtor_checker
{
  private:
    // Each constructed object registers its "this" pointer into "objs"
    std::set<void *> objs;

  public:
    ~cdtor_checker()
    {
      // At end of program, print out memory leaks
      //  (assuming no static "tester"s)
      for (std::set<void *>::const_iterator i = objs.begin();
          i != objs.end(); ++i)
        std::cout << "Memory leak: " << *i << std::endl;
    }

    void check_in(void * const This)
    {
      if (objs.find(This) != objs.end())
        std::cout << "Double-constructed memory: " << This << std::endl;
      objs.insert(This);
    }
    void check_out(void * const This)
    {
      std::set<void *>::iterator i = objs.find(This);
      if (i == objs.end())
        std::cout << "Destroyed non-constructed memory: " << This << std::endl;
      objs.erase(This);
    }

    // true iff all objects that have checked in have checked out
    bool ok() const { return objs.empty(); }
};
static cdtor_checker mem;

struct tester
{
  tester(int arg1, int arg2)
  {
    if (arg1 == 17 && arg2 == 17)
    {
#ifdef VERBOSE
      std::cout << this << ": tester not constructed" << std::endl;
#endif
      throw std::logic_error("No construction allowed!");
    }
#ifdef VERBOSE
    std::cout << this << ": tester::tester()" << std::endl;
#endif
    mem.check_in(this);
  }
#ifdef VERBOSE
  tester(const tester & other)
  {
    std::cout << this << ": tester::tester(" << &other << ')' << std::endl;
#else
  tester(const tester &)
  {
#endif
    mem.check_in(this);
  }
  ~tester()
  {
#ifdef VERBOSE
    std::cout << this << ": tester::~tester()" << std::endl;
#endif
    mem.check_out(this);
  }
};

void test()
{
  {
    // should do nothing
    boost::object_pool<tester> pool;
  }

  {
    // Construct several tester objects.  Don't delete them (i.e.,
    //  test pool's garbage collection).
#ifdef VERBOSE
    std::cout << "Testing pool. . ." << std::endl;
#endif
    boost::object_pool<tester> pool;
    for (int i = 0; i < 10; ++i)
      pool.construct(13, 13);
  }

  {
    // Construct several tester objects.  Delete some of them.
#ifdef VERBOSE
    std::cout << "Testing pool with some removed. . ." << std::endl;
#endif
    boost::object_pool<tester> pool;
    std::vector<tester *> v;
    for (int i = 0; i < 10; ++i)
      v.push_back(pool.construct(13, 13));
    std::random_shuffle(v.begin(), v.end());
    for (int j = 0; j < 5; ++j)
      pool.destroy(v[j]);
  }

  {
    // Test how pool reacts with constructors that throw exceptions.
    //  Shouldn't have any memory leaks.
#ifdef VERBOSE
    std::cout << "Testing with exceptional constructors :). . ." << std::endl;
#endif
    boost::object_pool<tester> pool;
    for (int i = 0; i < 5; ++i)
    {
      pool.construct(13, 13);
    }
    for (int j = 0; j < 5; ++j)
    {
      try
      {
        // The following constructor will raise an exception.
        pool.construct(17, 17);
      }
      catch (const std::logic_error &) { }
    }
  }
}

void test_alloc()
{
#ifdef VERBOSE
  std::cout << "Testing allocator. . ." << std::endl;
#endif

  {
    // Allocate several tester objects.  Delete one.
#ifdef VERBOSE
    std::cout << "with vector. . ." << std::endl;
#endif
    std::vector<tester, boost::pool_allocator<tester> > l;
    for (int i = 0; i < 10; ++i)
      l.push_back(tester(13, 13));
    l.pop_back();
  }

  {
    // Allocate several tester objects.  Delete two.
#ifdef VERBOSE
    std::cout << "with deque. . ." << std::endl;
#endif
    std::deque<tester, boost::pool_allocator<tester> > l;
    for (int i = 0; i < 10; ++i)
      l.push_back(tester(13, 13));
    l.pop_back();
    l.pop_front();
  }

  {
    // Allocate several tester objects.  Delete two.
#ifdef VERBOSE
    std::cout << "with list. . ." << std::endl;
#endif
    std::list<tester, boost::fast_pool_allocator<tester> > l;
    // lists rebind their allocators, so dumping is useless
    for (int i = 0; i < 10; ++i)
      l.push_back(tester(13, 13));
    l.pop_back();
    l.pop_front();
  }

  tester * tmp;
  {
    // Create a memory leak on purpose.  (Allocator doesn't have
    //  garbage collection)
#ifdef VERBOSE
    std::cout << "Testing allocator cleanup. . ." << std::endl;
#endif
    // (Note: memory leak)
    boost::pool_allocator<tester> a;
    tmp = a.allocate(1, 0);
    new (tmp) tester(13, 13);
  }
  if (mem.ok())
    std::cout << "Error: Pool allocator cleaned up!" << std::endl;
  // Remove memory checker entry (to avoid error later) and
  //  clean up memory leak
  tmp->~tester();
  boost::pool_allocator<tester>::deallocate(tmp, 1);
}

// This is a wrapper around a UserAllocator.  It just registers alloc/dealloc
//  to/from the system memory.  It's used to make sure pool's are allocating
//  and deallocating system memory properly.
// Do NOT use this class with static or singleton pools.
template <typename UserAllocator>
struct TrackAlloc
{
  typedef typename UserAllocator::size_type size_type;
  typedef typename UserAllocator::difference_type difference_type;

  static std::set<char *> allocated_blocks;

  static char * malloc(const size_type bytes)
  {
    char * const ret = UserAllocator::malloc(bytes);
    allocated_blocks.insert(ret);
    return ret;
  }
  static void free(char * const block)
  {
    if (allocated_blocks.find(block) == allocated_blocks.end())
      std::cout << "Free'd non-malloc'ed block: " << (void *) block << std::endl;
    allocated_blocks.erase(block);
    UserAllocator::free(block);
  }

  static bool ok() { return allocated_blocks.empty(); }
};
template <typename UserAllocator>
std::set<char *> TrackAlloc<UserAllocator>::allocated_blocks;

typedef TrackAlloc<boost::default_user_allocator_new_delete> track_alloc;

void test_mem_usage()
{
#ifdef VERBOSE
  std::cout << "Testing memory usage. . ." << std::endl;
#endif

  typedef boost::pool<track_alloc> pool_type;

  {
    // Constructor should do nothing; no memory allocation
    pool_type pool(sizeof(int));
    if (!track_alloc::ok())
      std::cout << "Memory error" << std::endl;
    if (pool.release_memory())
      std::cout << "Pool released memory" << std::endl;
    if (pool.purge_memory())
      std::cout << "Pool purged memory" << std::endl;

    // Should allocate from system
    pool.free(pool.malloc());
    if (track_alloc::ok())
      std::cout << "Memory error" << std::endl;

    // Ask pool to give up memory it's not using; this should succeed
    if (!pool.release_memory())
      std::cout << "Pool didn't release memory" << std::endl;
    if (!track_alloc::ok())
      std::cout << "Memory error" << std::endl;

    // Should allocate from system again
    pool.malloc(); // loses the pointer to the returned chunk (*A*)

    // Ask pool to give up memory it's not using; this should fail
    if (pool.release_memory())
      std::cout << "Pool released memory" << std::endl;

    // Force pool to give up memory it's not using; this should succeed
    //  This will clean up the memory leak from (*A*)
    if (!pool.purge_memory())
      std::cout << "Pool didn't purge memory" << std::endl;
    if (!track_alloc::ok())
      std::cout << "Memory error" << std::endl;

    // Should allocate from system again
    pool.malloc(); // loses the pointer to the returned chunk (*B*)

    // pool's destructor should purge the memory
    //  This will clean up the memory leak from (*B*)
  }

  if (!track_alloc::ok())
    std::cout << "Memory error" << std::endl;
}

void test_void()
{
#ifdef VERBOSE
    std::cout << "Testing void specialization. . ." << std::endl;
#endif

    typedef boost::pool_allocator<void> void_allocator;
    typedef boost::fast_pool_allocator<void> fast_void_allocator;

    typedef void_allocator::rebind<int>::other int_allocator;
    typedef fast_void_allocator::rebind<int>::other fast_int_allocator;

    std::vector<int, int_allocator> v1;
    std::vector<int, fast_int_allocator> v2;
}

int test_main(int, char * [])
{
  test();
  test_alloc();
  test_mem_usage();
  test_void();

#ifdef VERBOSE
  std::cout << "main() exiting. . ." << std::endl;
#endif
  if (mem.ok() && track_alloc::ok())
    std::cout << "All tests passed!" << std::endl;
  else
    std::cout << "Memory inconsistent!" << std::endl;
  return 0;
}


