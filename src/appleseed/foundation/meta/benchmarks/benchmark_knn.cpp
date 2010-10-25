
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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

// appleseed.foundation headers.
#include "foundation/math/knn.h"
#include "foundation/math/vector.h"
#include "foundation/utility/benchmark.h"

// Standard headers.
#include <cstddef>
#include <cstdio>
#include <vector>

using namespace foundation;
using namespace std;

BENCHMARK_SUITE(Foundation_Math_Knn)
{
    struct Fixture
    {
        vector<Vector3f>    m_points;
        KnnData3f           m_knn_data;

        Fixture()
        {
            load_points();

            if (!m_points.empty())
            {
                KnnBuilder3f builder;
                builder.build(&m_points[0], m_points.size(), m_knn_data);
            }
        }

        void load_points()
        {
            FILE* file = fopen("data/test_knn_points.txt", "rt");

            if (file == 0)
                return;

            size_t point_count;
            fscanf(file, FMT_SIZE_T, &point_count);

            m_points.resize(point_count);

            for (size_t i = 0; i < point_count; ++i)
            {
                Vector3f& p = m_points[i];
                fscanf(file, "%f %f %f", &p.x, &p.y, &p.z);
            }

            fclose(file);
        }
    };

    BENCHMARK_CASE_WITH_FIXTURE(Test, Fixture)
    {
        const size_t point_count = m_points.size();
        const size_t K = 3;

        for (size_t i = 0; i < point_count; ++i)
        {
            KnnQuery3f knn_query(m_knn_data, K);
            knn_query.run(m_points[i]);
        }
    }
}
