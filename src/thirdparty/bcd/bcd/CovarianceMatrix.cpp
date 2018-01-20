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

// BCD headers.
#include "CovarianceMatrix.h"

namespace bcd
{

SymmetricMatrix3x3& SymmetricMatrix3x3::operator+=(const SymmetricMatrix3x3& i_rSymMat)
{
    const_iterator it = i_rSymMat.m_data.begin();

    for (float& rValue : m_data)
        rValue += *it++;

    return *this;
}

SymmetricMatrix3x3& SymmetricMatrix3x3::operator*=(float i_factor)
{
    for (float& rValue : m_data)
        rValue *= i_factor;

    return *this;
}

void SymmetricMatrix3x3::copyFrom(const float* i_pData)
{
    std::copy(
        i_pData,
        i_pData + static_cast<std::size_t>(ESymMatData::e_nb),
        m_data.begin());
}

Block3x3DiagonalSymmetricMatrix& Block3x3DiagonalSymmetricMatrix::operator+=(
    const Block3x3DiagonalSymmetricMatrix&  i_rMat)
{
    const_iterator it = i_rMat.m_blocks.begin();

    for (SymmetricMatrix3x3& rBlock : m_blocks)
        rBlock += *it++;

    return *this;
}

Block3x3DiagonalSymmetricMatrix& Block3x3DiagonalSymmetricMatrix::operator*=(
    float i_factor)
{
    for (SymmetricMatrix3x3& rBlock : m_blocks)
        rBlock *= i_factor;

    return *this;
}

} // namespace bcd
