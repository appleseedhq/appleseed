// This file comes from the original BCD implementation,
// with minor changes to remove dependencies, unused code
// and re-formatting. Original license follows:

// This file is part of the reference implementation for the paper
//   Bayesian Collaborative Denoising for Monte-Carlo Rendering
//   Malik Boughida and Tamy Boubekeur.
//   Computer Graphics Forum (Proc. EGSR 2017), vol. 36, no. 4, p. 137-153, 2017
//
// All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE.txt file.

#ifndef COVARIANCE_MATRIX_H
#define COVARIANCE_MATRIX_H

// Standard headers.
#include <array>
#include <vector>

namespace bcd
{

enum class ESymmetricMatrix3x3Data
{
    e_xx,
    e_yy,
    e_zz,
    e_yz,
    e_xz,
    e_xy,
    e_nb
};

typedef ESymmetricMatrix3x3Data ESymMatData;

//  Class to represent a 3x3 symmetric matrix
struct SymmetricMatrix3x3
{
    SymmetricMatrix3x3() {}

    SymmetricMatrix3x3& operator+=(const SymmetricMatrix3x3& i_rSymMat);
    SymmetricMatrix3x3& operator*=(float i_factor);

    void copyFrom(const float* i_pData);

    typedef std::array<float, static_cast<std::size_t>(ESymMatData::e_nb)>::const_iterator const_iterator;

    std::array<float, static_cast<std::size_t>(ESymMatData::e_nb)> m_data;
};

typedef SymmetricMatrix3x3 CovMat3x3;

//  Class to represent a symmetric block-diagonal matrix, with blocks of size 3x3
struct Block3x3DiagonalSymmetricMatrix
{
    Block3x3DiagonalSymmetricMatrix()
    {
    }

    explicit Block3x3DiagonalSymmetricMatrix(std::size_t i_size)
      : m_blocks(i_size)
    {
    }

    Block3x3DiagonalSymmetricMatrix& operator+=(const Block3x3DiagonalSymmetricMatrix& i_rMat);
    Block3x3DiagonalSymmetricMatrix& operator*=(float i_factor);

    typedef std::vector<SymmetricMatrix3x3>::const_iterator const_iterator;

    std::vector<SymmetricMatrix3x3> m_blocks;
};

typedef Block3x3DiagonalSymmetricMatrix CovMatPatch;

} // namespace bcd

#endif // COVARIANCE_MATRIX_H
