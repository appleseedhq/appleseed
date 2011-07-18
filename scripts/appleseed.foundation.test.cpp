
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
