/*****************************************************************************/
/*                                                                           */
/*  Header: test.c pp                                                        */
/*                                                                           */
/*  Accompanies STANN Version 0.71 B                                         */
/*  Dec 07, 2009                                                             */
/*                                                                           */
/*  Copyright 2007, 2008                                                     */
/*  Michael Connor and Piyush Kumar                                          */
/*  Florida State University                                                 */
/*  Tallahassee FL, 32306-4532                                               */
/*                                                                           */
/*****************************************************************************/




#include <iostream>
#include <limits>
#include <test.hpp>

//Test Size determines the number of random points 
//each algorithm will be tested with.  It's small
//because everything is tested against an agonizingly
//slow brute force algorithm.

#define TEST_SIZE 500
#define K 5

int passed = 0;
int failed = 0;

void result(bool pass)
{
  if(pass) 
    {
      passed++;
      std::cerr << "passed." << std::endl;
    }
  else 
    {
      failed++;
      std::cerr << "failed." << std::endl;
    }
}

int main()
{
  bool pass;

  __srand48__(static_cast<unsigned int>(time(0)));



  //
  //Test of zorder less than operator
  //

  std::cerr << "3d unsigned int zorder test...";
  pass = testZLT<unsigned int, 3>(0, (std::numeric_limits<unsigned int>::max)());
  result(pass);
  
  std::cerr << "3d signed int zorder test...";
  pass = testZLT<int, 3>(-(std::numeric_limits<int>::max)(), (std::numeric_limits<int>::max)());
  result(pass);

  std::cerr << "3d float zorder test...";
  pass = testZLT<float, 3>(-(std::numeric_limits<float>::max)(), (std::numeric_limits<float>::max)());
  result(pass);

  std::cerr << "3d unit float zorder test...";
  pass = testZLT<float, 3>(0, 1);
  result(pass);

std::cerr << "3d double zorder test...";
  pass = testZLT<double, 3>(-(std::numeric_limits<double>::max)(), (std::numeric_limits<double>::max)());
  result(pass);

std::cerr << "3d unit double zorder test...";
 pass = testZLT<double, 3>(0,1);
  result(pass);

  //
  //Sorting test
  //

  std::cerr << "3d unsigned int sort test...";
  pass = testSORT<unsigned int, 3>(TEST_SIZE, 0, (std::numeric_limits<unsigned int>::max)());
  result(pass);

  std::cerr << "3d int sort test...";
  pass = testSORT<int, 3>(TEST_SIZE, -(std::numeric_limits<int>::max)(), (std::numeric_limits<int>::max)());
  result(pass);

  std::cerr << "3d float sort test...";
  pass = testSORT<float, 3>(TEST_SIZE, -(std::numeric_limits<float>::max)(), (std::numeric_limits<float>::max)());
  result(pass);

  std::cerr << "3d unit float sort test...";
  pass = testSORT<float, 3>(TEST_SIZE, 0, 1);
  result(pass);

  std::cerr << "3d double sort test...";
  pass = testSORT<double, 3>(TEST_SIZE, -(std::numeric_limits<double>::max)(), (std::numeric_limits<double>::max)());
  result(pass);

  std::cerr << "3d unit double sort test...";
  pass = testSORT<double, 3>(TEST_SIZE, 0, 1);
  result(pass);

  //
  //Test of signed integer coordinate types
  //
    
  std::cerr << "3d short test...";
  pass = testNN<short, 3>(TEST_SIZE, K, (std::numeric_limits<short>::min)(), (std::numeric_limits<short>::max)());
  result(pass);
  std::cerr << "5d short test...";
  pass = testNN<short, 5>(TEST_SIZE, K, -12000, 12000);
  result(pass);

  std::cerr << "3d int test...";
  pass = testNN<int, 3>(TEST_SIZE, K, -1000000000, 100000000);
  result(pass);
  std::cerr << "5d int test...";
  pass = testNN<int, 5>(TEST_SIZE, K, -1000000000, 100000000);
  result(pass);

  std::cerr << "3d long test...";
  pass = testNN<long, 3>(TEST_SIZE, K, -1000000000, 100000000);
  result(pass);
  std::cerr << "5d long test...";
  pass = testNN<long, 5>(TEST_SIZE, K, -1000000000, 100000000);
  result(pass);
  
  //
  //Test of  unsigned coordinate types
  //
  
  std::cerr << "3d unsigned short test...";
  pass = testNN<unsigned short int, 3>(TEST_SIZE, K, 0, 25000);
  result(pass);
  std::cerr << "5d unsigned short test...";
  pass = testNN<unsigned short int, 5>(TEST_SIZE, K, 0, 12000);
  result(pass);
  std::cerr << "3d unsigned int test...";
  pass = testNN<unsigned int, 3>(TEST_SIZE, K, 0, 100000000);
  result(pass);
  std::cerr << "5d unsigned int test...";
  pass = testNN<unsigned int, 5>(TEST_SIZE, K, 0, 100000000);
  result(pass);
  std::cerr << "3d unsigned long int test...";
  pass = testNN<unsigned long int, 3>(TEST_SIZE, K, 0, 100000000);
  result(pass);
  std::cerr << "5d unsigned long int test...";
  pass = testNN<unsigned long int, 5>(TEST_SIZE, K, 0, 100000000);
  result(pass);

  //
  //Test of floating point types
  //
  
  std::cerr << "3d float test...";
  pass = testNN<float, 3>(TEST_SIZE, K, (float) -pow((float) 2.0, 12), (float) pow((float) 2.0,12));
  result(pass);
  
  std::cerr << "3d double test....";
  pass = testNN<double, 3>(TEST_SIZE, K, -pow(2.0, 32), pow(2.0,32));
  result(pass);
  
  std::cerr << "3d unit double test....";
  pass = testNN<double, 3>(TEST_SIZE, K, -1, 1);
  result(pass);
  std::cerr << "3d double knng test....";
  pass = testKNNG<double, 3>(TEST_SIZE, K, -pow(2.0, 32), pow(2.0, 32), 1);
  result(pass);
  std::cerr << "3d float knng test....";
  pass = testKNNG<float, 3>(TEST_SIZE, K, (float) -pow((float) 2.0, 12), (float) pow((float) 2.0, 12), 1);
  result(pass);
  std::cerr << "3d unit double knng test....";
  pass = testKNNG<double, 3>(TEST_SIZE, K, -1, 1, 1);
  result(pass);
/*  
  std::cerr << "2d Geometric Minimum Spanning Tree test...";
  pass = testGMST<2>(TEST_SIZE);
  result(pass);
  std::cerr << "3d Geometric Minimum Spanning Tree test...";
  pass = testGMST<3>(TEST_SIZE);
  result(pass);
  std::cerr << "4d Geometric Minimum Spanning Tree test...";
  pass = testGMST<4>(TEST_SIZE);
  result(pass);
  
  std::cerr << "5d Geometric Minimum Spanning Tree test...";
  pass = testGMST<5>(TEST_SIZE);
  result(pass);*/
  /**/  
  std::cerr << "Tests completed.  "  << passed << " tests passed. " << failed << " tests failed" << std::endl;

  return 0;
}
