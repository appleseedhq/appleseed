//-*****************************************************************************
//
// Copyright (c) 2009-2011,
//  Sony Pictures Imageworks, Inc. and
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
// Industrial Light & Magic nor the names of their contributors may be used
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

#ifndef _Alembic_Abc_ISampleSelector_h_
#define _Alembic_Abc_ISampleSelector_h_

#include <Alembic/Abc/Foundation.h>

namespace Alembic {
namespace Abc {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
class ISampleSelector
{
public:
    enum TimeIndexType
    {
        kFloorIndex,
        kCeilIndex,
        kNearIndex
    };

    ISampleSelector()
      : m_requestedIndex( 0 ),
        m_requestedTime( 0.0 ),
        m_requestedTimeIndexType( kNearIndex ) {}

    ISampleSelector( index_t iReqIdx )
      : m_requestedIndex( iReqIdx ),
        m_requestedTime( 0.0 ),
        m_requestedTimeIndexType( kNearIndex ) {}

    explicit ISampleSelector( chrono_t iReqTime,
                              TimeIndexType iReqIdxType = kNearIndex )
      : m_requestedIndex( -1 ),
        m_requestedTime( iReqTime ),
        m_requestedTimeIndexType( iReqIdxType ) {}

    index_t getRequestedIndex() const { return m_requestedIndex; }
    chrono_t getRequestedTime() const { return m_requestedTime; }
    TimeIndexType getRequestedTimeIndexType() const
    { return m_requestedTimeIndexType; }

    index_t getIndex( const AbcA::TimeSamplingPtr & iTsmp, index_t
        iNumSamples ) const;

private:
    index_t m_requestedIndex;
    chrono_t m_requestedTime;
    TimeIndexType m_requestedTimeIndexType;
};

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace Abc
} // End namespace Alembic

#endif
