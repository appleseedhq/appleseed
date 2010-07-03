// Copyright (C) 2000, 2001 Stephen Cleary
//
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#include <boost/pool/pool_alloc.hpp>
#include <boost/pool/object_pool.hpp>

#include <iostream>
#include <vector>
#include <list>
#include <set>

#include <ctime>
#include <cerrno>

#include "sys_allocator.hpp"

unsigned long num_ints;

template <unsigned N>
struct larger_structure
{
  char data[N];
};

unsigned test_number;

template <unsigned N>
static void timing_test_alloc_larger()
{
  typedef boost::fast_pool_allocator<larger_structure<N>,
      boost::default_user_allocator_new_delete,
      boost::details::pool::null_mutex> alloc;
  typedef boost::fast_pool_allocator<larger_structure<N> > alloc_sync;

  double end[1][6];
  std::clock_t start;

  start = std::clock();
  {
    std::allocator<larger_structure<N> > a;
    for (unsigned long i = 0; i < num_ints; ++i)
      a.deallocate(a.allocate(1), 1);
  }
  end[0][0] = (std::clock() - start) / ((double) CLOCKS_PER_SEC);

  start = std::clock();
  {
    for (unsigned long i = 0; i < num_ints; ++i)
      std::free(std::malloc(sizeof(larger_structure<N>)));
  }
  end[0][1] = (std::clock() - start) / ((double) CLOCKS_PER_SEC);

  start = std::clock();
  {
    for (unsigned long i = 0; i < num_ints; ++i)
      delete new (std::nothrow) larger_structure<N>;
  }
  end[0][2] = (std::clock() - start) / ((double) CLOCKS_PER_SEC);

  start = std::clock();
  {
    for (unsigned long i = 0; i < num_ints; ++i)
      alloc::deallocate(alloc::allocate());
  }
  end[0][3] = (std::clock() - start) / ((double) CLOCKS_PER_SEC);

  start = std::clock();
  {
    for (unsigned long i = 0; i < num_ints; ++i)
      alloc_sync::deallocate(alloc_sync::allocate());
  }
  end[0][4] = (std::clock() - start) / ((double) CLOCKS_PER_SEC);

  start = std::clock();
  {
    boost::pool<> p(sizeof(larger_structure<N>));
    for (unsigned long i = 0; i < num_ints; ++i)
    {
      void * const t = p.malloc();
      if (t != 0)
        p.free(t);
    }
  }
  end[0][5] = (std::clock() - start) / ((double) CLOCKS_PER_SEC);

  std::cout << "Test " << test_number++ << ": Alloc & Dealloc " << num_ints << " structures of size " << sizeof(larger_structure<N>) << ":" << std::endl;
  std::cout << "  std::allocator: " << end[0][0] << " seconds" << std::endl;
  std::cout << "  malloc/free:    " << end[0][1] << " seconds" << std::endl;
  std::cout << "  new/delete:     " << end[0][2] << " seconds" << std::endl;
  std::cout << "  Pool Alloc:     " << end[0][3] << " seconds" << std::endl;
  std::cout << "  Pool /w Sync:   " << end[0][4] << " seconds" << std::endl;
  std::cout << "  Pool:           " << end[0][5] << " seconds" << std::endl;
}

static void timing_test_alloc()
{
  typedef boost::fast_pool_allocator<int,
      boost::default_user_allocator_new_delete,
      boost::details::pool::null_mutex> alloc;
  typedef boost::fast_pool_allocator<int> alloc_sync;

  double end[2][6];
  std::clock_t start;

  int ** p = new int*[num_ints];

  start = std::clock();
  {
    std::allocator<int> a;
    for (unsigned long i = 0; i < num_ints; ++i)
      a.deallocate(a.allocate(1), 1);
  }
  end[0][0] = (std::clock() - start) / ((double) CLOCKS_PER_SEC);

  start = std::clock();
  {
    for (unsigned long i = 0; i < num_ints; ++i)
      std::free(std::malloc(sizeof(int)));
  }
  end[0][1] = (std::clock() - start) / ((double) CLOCKS_PER_SEC);

  start = std::clock();
  {
    for (unsigned long i = 0; i < num_ints; ++i)
      delete new (std::nothrow) int;
  }
  end[0][2] = (std::clock() - start) / ((double) CLOCKS_PER_SEC);

  start = std::clock();
  {
    for (unsigned long i = 0; i < num_ints; ++i)
      alloc::deallocate(alloc::allocate());
  }
  end[0][3] = (std::clock() - start) / ((double) CLOCKS_PER_SEC);

  start = std::clock();
  {
    for (unsigned long i = 0; i < num_ints; ++i)
      alloc_sync::deallocate(alloc_sync::allocate());
  }
  end[0][4] = (std::clock() - start) / ((double) CLOCKS_PER_SEC);

  start = std::clock();
  {
    boost::pool<> p(sizeof(int));
    for (unsigned long i = 0; i < num_ints; ++i)
    {
      void * const t = p.malloc();
      if (t != 0)
        p.free(t);
    }
  }
  end[0][5] = (std::clock() - start) / ((double) CLOCKS_PER_SEC);


  start = std::clock();
  {
    std::allocator<int> a;
    for (unsigned long i = 0; i < num_ints; ++i)
      p[i] = a.allocate(1);
    for (unsigned long i = 0; i < num_ints; ++i)
      a.deallocate(p[i], 1);
  }
  end[1][0] = (std::clock() - start) / ((double) CLOCKS_PER_SEC);

  start = std::clock();
  {
    for (unsigned long i = 0; i < num_ints; ++i)
      p[i] = (int *) std::malloc(sizeof(int));
    for (unsigned long i = 0; i < num_ints; ++i)
      std::free(p[i]);
  }
  end[1][1] = (std::clock() - start) / ((double) CLOCKS_PER_SEC);

  start = std::clock();
  {
    for (unsigned long i = 0; i < num_ints; ++i)
      p[i] = new (std::nothrow) int;
    for (unsigned long i = 0; i < num_ints; ++i)
      delete p[i];
  }
  end[1][2] = (std::clock() - start) / ((double) CLOCKS_PER_SEC);

  start = std::clock();
  {
    for (unsigned long i = 0; i < num_ints; ++i)
      p[i] = alloc::allocate();
    for (unsigned long i = 0; i < num_ints; ++i)
      alloc::deallocate(p[i]);
  }
  end[1][3] = (std::clock() - start) / ((double) CLOCKS_PER_SEC);

  start = std::clock();
  {
    for (unsigned long i = 0; i < num_ints; ++i)
      p[i] = alloc_sync::allocate();
    for (unsigned long i = 0; i < num_ints; ++i)
      alloc_sync::deallocate(p[i]);
  }
  end[1][4] = (std::clock() - start) / ((double) CLOCKS_PER_SEC);

  start = std::clock();
  {
    boost::pool<> pl(sizeof(int));
    for (unsigned long i = 0; i < num_ints; ++i)
      p[i] = reinterpret_cast<int *>(pl.malloc());
    for (unsigned long i = 0; i < num_ints; ++i)
      if (p[i] != 0)
        pl.free(p[i]);
  }
  end[1][5] = (std::clock() - start) / ((double) CLOCKS_PER_SEC);

  delete [] p;

  std::cout << "Test 3: Alloc & Dealloc " << num_ints << " ints:" << std::endl;
  std::cout << "  std::allocator: " << end[0][0] << " seconds" << std::endl;
  std::cout << "  malloc/free:    " << end[0][1] << " seconds" << std::endl;
  std::cout << "  new/delete:     " << end[0][2] << " seconds" << std::endl;
  std::cout << "  Pool Alloc:     " << end[0][3] << " seconds" << std::endl;
  std::cout << "  Pool /w Sync:   " << end[0][4] << " seconds" << std::endl;
  std::cout << "  Pool:           " << end[0][5] << " seconds" << std::endl;

  std::cout << "Test 4: Alloc " << num_ints << " ints & Dealloc " << num_ints << " ints:" << std::endl;
  std::cout << "  std::allocator: " << end[1][0] << " seconds" << std::endl;
  std::cout << "  malloc/free:    " << end[1][1] << " seconds" << std::endl;
  std::cout << "  new/delete:     " << end[1][2] << " seconds" << std::endl;
  std::cout << "  Pool Alloc:     " << end[1][3] << " seconds" << std::endl;
  std::cout << "  Pool /w Sync:   " << end[1][4] << " seconds" << std::endl;
  std::cout << "  Pool:           " << end[1][5] << " seconds" << std::endl;
}

static void timing_test_containers()
{
  typedef boost::pool_allocator<int,
      boost::default_user_allocator_new_delete,
      boost::details::pool::null_mutex> alloc;
  typedef boost::pool_allocator<int> alloc_sync;
  typedef boost::fast_pool_allocator<int,
      boost::default_user_allocator_new_delete,
      boost::details::pool::null_mutex> fast_alloc;
  typedef boost::fast_pool_allocator<int> fast_alloc_sync;

  double end[3][5];
  std::clock_t start;

  start = std::clock();
  {
    std::vector<int, std::allocator<int> > x;
    for (unsigned long i = 0; i < num_ints; ++i)
      x.push_back(0);
  }
  end[0][0] = (std::clock() - start) / ((double) CLOCKS_PER_SEC);

  start = std::clock();
  {
    std::vector<int, malloc_allocator<int> > x;
    for (unsigned long i = 0; i < num_ints; ++i)
      x.push_back(0);
  }
  end[0][1] = (std::clock() - start) / ((double) CLOCKS_PER_SEC);

  start = std::clock();
  {
    std::vector<int, new_delete_allocator<int> > x;
    for (unsigned long i = 0; i < num_ints; ++i)
      x.push_back(0);
  }
  end[0][2] = (std::clock() - start) / ((double) CLOCKS_PER_SEC);

  start = std::clock();
  {
    std::vector<int, alloc> x;
    for (unsigned long i = 0; i < num_ints; ++i)
      x.push_back(0);
  }
  end[0][3] = (std::clock() - start) / ((double) CLOCKS_PER_SEC);

  start = std::clock();
  {
    std::vector<int, alloc_sync> x;
    for (unsigned long i = 0; i < num_ints; ++i)
      x.push_back(0);
  }
  end[0][4] = (std::clock() - start) / ((double) CLOCKS_PER_SEC);


  start = std::clock();
  {
    std::set<int, std::less<int>, std::allocator<int> > x;
    for (unsigned long i = 0; i < num_ints; ++i)
      x.insert(0);
  }
  end[1][0] = (std::clock() - start) / ((double) CLOCKS_PER_SEC);

  start = std::clock();
  {
    std::set<int, std::less<int>, malloc_allocator<int> > x;
    for (unsigned long i = 0; i < num_ints; ++i)
      x.insert(0);
  }
  end[1][1] = (std::clock() - start) / ((double) CLOCKS_PER_SEC);

  start = std::clock();
  {
    std::set<int, std::less<int>, new_delete_allocator<int> > x;
    for (unsigned long i = 0; i < num_ints; ++i)
      x.insert(0);
  }
  end[1][2] = (std::clock() - start) / ((double) CLOCKS_PER_SEC);

  start = std::clock();
  {
    std::set<int, std::less<int>, fast_alloc> x;
    for (unsigned long i = 0; i < num_ints; ++i)
      x.insert(0);
  }
  end[1][3] = (std::clock() - start) / ((double) CLOCKS_PER_SEC);

  start = std::clock();
  {
    std::set<int, std::less<int>, fast_alloc_sync> x;
    for (unsigned long i = 0; i < num_ints; ++i)
      x.insert(0);
  }
  end[1][4] = (std::clock() - start) / ((double) CLOCKS_PER_SEC);


  start = std::clock();
  {
    std::list<int, std::allocator<int> > x;
    for (unsigned long i = 0; i < num_ints; ++i)
      x.push_back(0);
  }
  end[2][0] = (std::clock() - start) / ((double) CLOCKS_PER_SEC);

  start = std::clock();
  {
    std::list<int, malloc_allocator<int> > x;
    for (unsigned long i = 0; i < num_ints; ++i)
      x.push_back(0);
  }
  end[2][1] = (std::clock() - start) / ((double) CLOCKS_PER_SEC);

  start = std::clock();
  {
    std::list<int, new_delete_allocator<int> > x;
    for (unsigned long i = 0; i < num_ints; ++i)
      x.push_back(0);
  }
  end[2][2] = (std::clock() - start) / ((double) CLOCKS_PER_SEC);

  start = std::clock();
  {
    std::list<int, fast_alloc> x;
    for (unsigned long i = 0; i < num_ints; ++i)
      x.push_back(0);
  }
  end[2][3] = (std::clock() - start) / ((double) CLOCKS_PER_SEC);

  start = std::clock();
  {
    std::list<int, fast_alloc_sync> x;
    for (unsigned long i = 0; i < num_ints; ++i)
      x.push_back(0);
  }
  end[2][4] = (std::clock() - start) / ((double) CLOCKS_PER_SEC);

  std::cout << "Test 0: Insertion & deletion of " << num_ints << " ints in a vector:" << std::endl;
  std::cout << "  std::allocator: " << end[0][0] << " seconds" << std::endl;
  std::cout << "  malloc/free:    " << end[0][1] << " seconds" << std::endl;
  std::cout << "  new/delete:     " << end[0][2] << " seconds" << std::endl;
  std::cout << "  Pool Alloc:     " << end[0][3] << " seconds" << std::endl;
  std::cout << "  Pool /w Sync:   " << end[0][4] << " seconds" << std::endl;
  std::cout << "  Pool:           not possible" << std::endl;
  std::cout << "Test 1: Insertion & deletion of " << num_ints << " ints in a set:" << std::endl;
  std::cout << "  std::allocator: " << end[1][0] << " seconds" << std::endl;
  std::cout << "  malloc/free:    " << end[1][1] << " seconds" << std::endl;
  std::cout << "  new/delete:     " << end[1][2] << " seconds" << std::endl;
  std::cout << "  Pool Alloc:     " << end[1][3] << " seconds" << std::endl;
  std::cout << "  Pool /w Sync:   " << end[1][4] << " seconds" << std::endl;
  std::cout << "  Pool:           not possible" << std::endl;
  std::cout << "Test 2: Insertion & deletion of " << num_ints << " ints in a list:" << std::endl;
  std::cout << "  std::allocator: " << end[2][0] << " seconds" << std::endl;
  std::cout << "  malloc/free:    " << end[2][1] << " seconds" << std::endl;
  std::cout << "  new/delete:     " << end[2][2] << " seconds" << std::endl;
  std::cout << "  Pool Alloc:     " << end[2][3] << " seconds" << std::endl;
  std::cout << "  Pool /w Sync:   " << end[2][4] << " seconds" << std::endl;
  std::cout << "  Pool:           not possible" << std::endl;
}

int main(int argc, char * argv[])
{
  if (argc != 1 && argc != 2)
  {
    std::cerr << "Usage: " << argv[0]
        << " [number_of_ints_to_use_each_try]" << std::endl;
    return 1;
  }

  errno = 0;

  if (argc == 2)
  {
    num_ints = std::strtoul(argv[1], 0, 10);
    if (errno != 0)
    {
      std::cerr << "Cannot convert number \"" << argv[1] << '"' << std::endl;
      return 2;
    }
  }
  else
    num_ints = 700000;

  try
  {
    timing_test_containers();
    timing_test_alloc();
    test_number = 5;
    timing_test_alloc_larger<64>();
    test_number = 6;
    timing_test_alloc_larger<256>();
    test_number = 7;
    timing_test_alloc_larger<4096>();
  }
  catch (const std::bad_alloc &)
  {
    std::cerr << "Timing tests ran out of memory; try again with a lower value for number of ints"
        << " (current value is " << num_ints << ")" << std::endl;
    return 3;
  }
  catch (const std::exception & e)
  {
    std::cerr << "Error: " << e.what() << std::endl;
    return 4;
  }
  catch (...)
  {
    std::cerr << "Unknown error" << std::endl;
    return 5;
  }

  return 0;
}


