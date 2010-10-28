Simple Thread-safe Approximate Nearest  Neighbor (STANN)
Version 0.71_beta

See Copyright.txt for copyright information.

Homepage: http://www.compgeom.com/~stann
Contact : stann @ compgeom DOT com

Developers:
	Michael Connor	(Florida State University)
	Piyush Kumar	(Florida State University)

*------------------------------------------------------------------------------*
| About STANN                                                                  |
*------------------------------------------------------------------------------*

STANN is a lightweight thread-safe library for doing Nearest Neighbor searches
and other geometric point based algorithms.
It is designed for low dimensional data sets, preferably 3d (STANN should give a 
correct answer for a NN search in any dimension, however it suffers significant 
performance loss as the dimension increases).  It is meant to be easy to 
integrate into existing code.

*------------------------------------------------------------------------------*
| A Note About Compilers                                                       |
*------------------------------------------------------------------------------*

STANN has been tested using gcc version 4.2.1 and Intel's C++ Compiler 11.1.
We have also compiled and tested it on Visual Studio 2008 and 2010.
For using Intel's compiler, just change the g++ to icpc in the Makefile.
To use the Visual C++ compiler, move the Makefile.vc++ to Makefile and 
execute "nmake". STANN does not work with Sun's C++ Compiler.

**IMPORTANT**

STANN works best using optimization from the compiler.  Code should be compiled
using -O3 for gcc.

note:  The sfcnn_knng (K-nearest neighbor graph) algorithm requires openmp
       in order to operate using multiple processors.  Use -fopenmp
       for gcc version 4.2 and above

note: gmst (Geometric minimum spanning trees) requires openmp and the parallel 
STL algorithms introduced in GCC 4.3 to fully utilize parallel implementation.  
For best results, use gcc version 4.3 or above, and use the -fopenmp and 
-D_GLIBCXX_PARALLEL flags.

*------------------------------------------------------------------------------*
| Using STANN for K-Nearest Neighbors                                          |
*------------------------------------------------------------------------------*

Currently, STANN contains one general K-nearest neighbor search algorithm, and
one K-nearest neighbor graph algorithm.

sfcnn:
	sfcnn is a general K-nearest neighbor search data structure.
	To include it in a program, include sfcnn.hpp

	To initialize:

	sfcnn< MyPointType, Dimension, CoordinateType > 
                      NN(&FirstDataPoint, Size);

	MyPointType:  This can be any user defined point type, as long as the 
		      coordinates can be referenced by the bracket operator.
		      For example, vector<double> or float[], are both valid.

	Dimension  :  This is the dimension of the points in the data set.

	CoordType  :  This is the type of coordinate the points use.
		      Currently supported types are: 
		      (unsigned) short, int, long
		      short, int, long
		      float, double, long double.

		      IMPORTANT:  Searches on integer types operate
		      		  about 3x faster than floating point
				  types.  If you can use them, it is
				  recommended.

	FirstPoint :  A pointer to the first point in the input data set

	Size	   :  The size of the input data set


	To make a query:

	Queries are thread-safe, and may be done in parallel using 
	openmp, posix threads etc.

	NN.ksearch(MyPointType Q, 
	           int k, 
		   vector<unsigned long> answer, 
		   double epsilon)
	or
	NN.ksearch(MyPointType Q, 
	           int k, 
		   vector<unsigned long> answer, 
		   vector<double> distance, 
		   double epsilon)

	Q	   :   A query point of type MyPointType.

	k	   :   The number of nearest neighbors to return.  Ie. k=5 will
		       return the 5 nearest points to Q in the data set

        answer     :   This vector will contain the indexes of the k nearest
		       data points.  These indexes are based off the 
		       FirstDataPoint given at initialization.  If
		       the points have been moved, the indexes will be
		       incorrect.

        distance   :   This vector will contain the squared distance from
		       the query point to the corresponding data point

        epsilon    :   This parameter is optional, and defaults to 0.
		       This approximation parameter guarantees that the
		       distance to the farthest returned point is
		       no farther than (1+epsilon)*(distance to actual k
		       nearest neighbor).  To compute an exact solution
		       leave it to be 0.

sfcnn_knng:
		sfcnn_knng is a specialized version of the sfcnn algorithm
		designed to quickly generate K-nearest neighbor graphs.
		If compiled using openmp, it will use multiple processors
		to compute the solution.
		To include it in a program, use sfcnn_knng.hpp

		To initialize:
		
		sfcnn_knng<MyPointType, Dimension, CoordType> 
				     NN(&FirstPoint, Size, k, ThreadCount);

                ThreadCount  :  This is an optional parameter whose default 
 			     	value is 1. If compiled using openmp, this will
			        set the number of processors to use during 
				construction.
 
                See sfcnn for definition of remaining parameters.

		Note: All calculation for the k nearest neighbor graph is
		      done at initialization.

		To make a query:

		NN[i][j]     :  This returns the index of the jth nearest 
			     	neighbor to the ith data point.  The indexes
				are all relative to the first data point
				given at initialization.  If the data points
				have moved, the answer will be incorrect.


*------------------------------------------------------------------------------*
| Using STANN for Geometric Minimum Spanning Trees                             |
*------------------------------------------------------------------------------*

STANN includes a fast, parallel geometric minimum spanning tree algorithm.
Currently, it can be invoked using the header gmst.hpp, and the function 

void gmst(vector<Point> &points, 
     	  vector<pair<typename vector<Point>::size_type,  
                      typename vector<Point>::size_type> > &mst)

It receives as input a vector of points, and writes output to the
provided vector.  

Output edges are stored as STL pairs of indexes to the
original vector.  In most cases, the mst vector could be declared
simply as vector<pair<int, int> >.

Note:  This algorithm modifies the order of points in the original vector.

*------------------------------------------------------------------------------*
| Using STANN with ANN                                                         |
*------------------------------------------------------------------------------*


TODO

