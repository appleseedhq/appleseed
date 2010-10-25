
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

#ifndef APPLESEED_FOUNDATION_MATH_KNN_KNN_BUILDER_H
#define APPLESEED_FOUNDATION_MATH_KNN_KNN_BUILDER_H

// appleseed.foundation headers.
#include "foundation/math/knn/knn_data.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cstddef>

namespace foundation
{

template <typename T>
class KnnBuilder3
{
  public:
    typedef Vector<T, 3> VectorType;
    typedef KnnData3<T> KnnDataType;

    void build(
        const VectorType*   points,
        const size_t        count,
        KnnDataType&        knn_data);
};

typedef KnnBuilder3<float> KnnBuilder3f;
typedef KnnBuilder3<double> KnnBuilder3d;


//
// Implementation.
//

template <typename T>
void KnnBuilder3<T>::build(
    const VectorType*       points,
    const size_t            count,
    KnnDataType&            knn_data)
{
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_KNN_KNN_BUILDER_H
