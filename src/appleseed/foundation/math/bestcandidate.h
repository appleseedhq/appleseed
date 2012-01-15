
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz
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

#ifndef APPLESEED_FOUNDATION_MATH_BESTCANDIDATE_H
#define APPLESEED_FOUNDATION_MATH_BESTCANDIDATE_H

// appleseed.foundation headers.
#include "foundation/math/rng.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace foundation
{

//
// Best-candidate sequences.
//
// todo:
//
//   reimplement best_candidate_sequence using a better acceleration structure than grid.
//

// Generate a best-candidate sequence of arbitrary dimension.
// This sequence approximates a Poisson-disk distribution.
// Warning: very slow, only for offline usage.
template <typename T, size_t Dim>
void best_candidate_sequence(
    const size_t        count,          // total number of samples in sequence
    const size_t        num_cand,       // number of candidates per sample
    Vector<T, Dim>      samples[]);     // [out] generated samples


//
// Best-candidate sequences implementation.
//

template <typename T, size_t Dim>
void best_candidate_sequence(
    const size_t        count,
    const size_t        num_cand,
    Vector<T, Dim>      samples[])
{
    // Compute acceleration grid size.
    const size_t grid_size =
        truncate<size_t>(std::ceil(std::pow(count, T(1.0) / Dim) * T(0.1)));

    // Allocate acceleration grid.
    const size_t num_cells = pow_int(grid_size, Dim);
    std::vector<size_t>* grid = new std::vector<size_t>[num_cells];

    MersenneTwister mt;

    for (size_t n = 0; n < count; ++n)
    {
        // Compute a sample by generating a small number of
        // candidates samples and keeping only the best one.
        // todo: adjust the number of candidates at each
        // iteration, to ensure somewhat constant quality.
        T best_dist = 0.0;
        Vector<T, Dim> best_candidate;
        Vector<int, Dim> best_cand_coords;
        for (size_t c = 0; c < num_cand; ++c)
        {
            // Generate a random candidate.
            Vector<T, Dim> candidate;
            for (size_t d = 0; d < Dim; ++d)
                candidate[d] = rand_double1(mt);

            // Compute grid coordinates of this candidate.
            Vector<int, Dim> cand_coords;
            for (size_t d = 0; d < Dim; ++d)
            {
                const int coord = static_cast<int>(
                    truncate<int>(candidate[d] * grid_size));
                cand_coords[d] = clamp(coord, 0, int(grid_size - 1));
            }

            // Find in the grid the sample which is the closest
            // to the current candidate, according to the
            // euclidean norm.

            // Compute the coordinates of the neighboring of the
            // current candidate.
            Vector<int, Dim> min, max;
            for (size_t d = 0; d < Dim; ++d)
            {
                min[d] = clamp(cand_coords[d] - 1, 0, int(grid_size - 1));
                max[d] = clamp(cand_coords[d] + 1, 0, int(grid_size - 1));
//                min[d] = 0;
//                max[d] = int(grid_size - 1);
            }

            // Iterate over the neighboring of the candidate,
            // compute the euclidean distance to each of the
            // sample and keep track of the distance (really
            // the square of the distance) to the closest one.
            Vector<int, Dim> cursor = min;
            Vector<T, Dim> closest;
            T smallest_dist = std::numeric_limits<T>::max();
            while (true)
            {
                // Compute index of this cell.
                size_t cell_index = 0;
                for (size_t rd = Dim; rd > 0; --rd)
                {
                    cell_index *= grid_size;
                    cell_index += cursor[rd - 1];
                }

                // Search for the closest sample in this cell.
                const std::vector<size_t>& cell = grid[cell_index];
                for (size_t j = 0; j < cell.size(); ++j)
                {
                    // Fetch sample.
                    const size_t sample_index = cell[j];
                    assert(sample_index < n);   // only n generated samples
                    const Vector<T, Dim>& sample = samples[sample_index];

                    // Compute the minimum of (1) the square of the euclidean
                    // distance between this sample and the current candidate
                    // and (2) the square of the distance between this sample
                    // and the current candidate in every 1d projection.
                    // todo: implement torus distance to generate tileable patterns.
                    T dist = 0.0;
                    for (size_t d = 0; d < Dim; ++d)
                        dist += square(sample[d] - candidate[d]);
                    for (size_t d = 0; d < Dim; ++d)
                        dist = std::min(dist, std::abs(sample[d] - candidate[d]));

                    // Keep track of the smallest distance.
                    if (dist < smallest_dist)
                        smallest_dist = dist;
                }

                // Find next neighboring cell.
                size_t d = 0;
                while (true)
                {
                    if (cursor[d] < max[d])
                    {
                        ++cursor[d];
                        break;
                    }
                    else
                    {
                        cursor[d] = min[d];
                        if (d + 1 < Dim)
                            ++d;
                        else goto exit_visit;
                    }
                }
            }

exit_visit:

            // Keep track of the sample which is the furthest away
            // from its closest neighbor in the grid.
            if (smallest_dist == std::numeric_limits<T>::max())
            {
                // First candidate, as there is no neighboring samples.
                best_candidate = candidate;
                best_cand_coords = cand_coords;
                break;
            }
            else if (smallest_dist > best_dist)
            {
                // Found a better candidate.
                best_dist = smallest_dist;
                best_candidate = candidate;
                best_cand_coords = cand_coords;
            }
        }

        // Store the sample in the output array.
        samples[n] = best_candidate;

        // Store the sample in the acceleration grid.
        size_t sample_index = 0;
        for (size_t rd = Dim; rd > 0; --rd)
        {
            sample_index *= grid_size;
            sample_index += best_cand_coords[rd - 1];
        }
        assert(sample_index < num_cells);
        grid[sample_index].push_back(n);
    }

    // Delete acceleration grid.
    delete [] grid;
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_BESTCANDIDATE_H
