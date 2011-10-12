//-*****************************************************************************
//
// Copyright (c) 2009-2011,
//  Sony Pictures Imageworks Inc. and
//  Industrial Light & Magic, a division of Lucasfilm Entertainment Company Ltd.
//
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
// *       Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
// *       Redistributions in binary form must reproduce the above
// copyright notice, this list of conditions and the following disclaimer
// in the documentation and/or other materials provided with the
// distribution.
// *       Neither the name of Sony Pictures Imageworks, nor
// Industrial Light & Magic, nor the names of their contributors may be used
// to endorse or promote products derived from this software without specific
// prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//-*****************************************************************************

#include <Alembic/Abc/ISampleSelector.h>

namespace Alembic {
namespace Abc {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
index_t ISampleSelector::getIndex( const AbcA::TimeSamplingPtr & iTsmp,
    index_t iNumSamples ) const
{
    index_t retIdx;

    if ( m_requestedIndex >= 0 )
    {
        retIdx = m_requestedIndex;
    }
    else if ( m_requestedTimeIndexType == kNearIndex )
    {
        retIdx = iTsmp->getNearIndex( m_requestedTime, iNumSamples ).first;
    }
    else if ( m_requestedTimeIndexType == kFloorIndex )
    {
        retIdx = iTsmp->getFloorIndex( m_requestedTime, iNumSamples ).first;
    }
    else
    {
        assert( m_requestedTimeIndexType == kCeilIndex );
        retIdx = iTsmp->getCeilIndex( m_requestedTime, iNumSamples ).first;
    }

    return retIdx < 0 ? 0 : ( retIdx < iNumSamples ? retIdx : iNumSamples-1 );
}


} // End namespace ALEMBIC_VERSION_NS
} // End namespace Abc
} // End namespace Alembic

