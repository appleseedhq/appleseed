
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//

#include "appleseed.foundation.h"

#include <cstddef>
#include <iostream>
#include <vector>

using namespace foundation;
using namespace std;

void test_knn()
{
    const size_t PointCount = 1000;     // total number of points
    const size_t AnswerSize = 5;        // number of neighbors per query (the k in knn)
    const size_t QueryCount = 3;        // number of knn queries

    // Generate random points in the unit cube [0,1]^3.
    MersenneTwister rng;
    vector<Vector3d> points;
    for (size_t i = 0; i < PointCount; ++i)
    {
        Vector3d p;
        p.x = rand_double1(rng);
        p.y = rand_double1(rng);
        p.z = rand_double1(rng);
        points.push_back(p);
    }

    // Build a tree out of these points.
    knn::Tree3d tree;
    knn::Builder3d builder(tree);
    builder.build(&points[0], PointCount);

    // Construct the knn query objects.
    knn::Answer<double> answer(AnswerSize);
    knn::Query3d query(tree, answer);

    for (size_t i = 0; i < QueryCount; ++i)
    {
        // Select a query point at random in the unit cube.
        Vector3d q;
        q.x = rand_double1(rng);
        q.y = rand_double1(rng);
        q.z = rand_double1(rng);

        // Run the knn query.
        query.run(q);
        answer.sort();

        // Print the results of the query.
        cerr << "Query #" << i << ':';
        for (size_t j = 0; j < AnswerSize; ++j)
            cerr << ' ' << answer.get(j).m_index;
        cerr << endl;
    }
}

int main()
{
    test_knn();
    return 0;
}
