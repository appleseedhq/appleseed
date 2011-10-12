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

#ifndef _Alembic_AbcCoreHDF5_SpwImpl_h_
#define _Alembic_AbcCoreHDF5_SpwImpl_h_

#include <Alembic/AbcCoreHDF5/Foundation.h>
#include <Alembic/AbcCoreHDF5/SimplePwImpl.h>

namespace Alembic {
namespace AbcCoreHDF5 {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
// This type is a no-op.
// It is here to support the template interface which, in order to generalize
// across arrays & scalars, needs a "key" type to compare array samples with.
class ScalarSampleKey
{
public:
    ScalarSampleKey(){}
};

//-*****************************************************************************
// Scalar Property Writer. 
class SpwImpl
    : public SimplePwImpl<AbcA::ScalarPropertyWriter,
                          SpwImpl,
                          const void *,
                          ScalarSampleKey>
    , public boost::enable_shared_from_this<SpwImpl>
{
protected:
    friend class BaseCpwImpl;

    //-*************************************************************************
    SpwImpl( AbcA::CompoundPropertyWriterPtr iParent,
             hid_t iParentGroup,
             const std::string & iName,
             const AbcA::MetaData & iMetaData,
             const AbcA::DataType & iDataType,
             uint32_t iTimeSamplingIndex );

    AbcA::ScalarPropertyWriterPtr asScalarPtr();

public:
    virtual ~SpwImpl();

    //-*************************************************************************
    // No-op return of no-op class.
    static ScalarSampleKey computeSampleKey( const void *iSamp )
    {
        return ScalarSampleKey();
    }

    //-*************************************************************************
    bool sameAsPreviousSample( const void *iSamp,
                               const ScalarSampleKey &iKey ) const
    {
        // CJH: Could include tolerances here.
        return m_previousSample.equalWithRelAbsError( iSamp,
                                                      1.0e-9 );
    }

    //-*************************************************************************
    void copyPreviousSample( hid_t iGroup,
                             const std::string &iSampleName,
                             index_t iSampleIndex );

    //-*************************************************************************
    void writeSample( hid_t iGroup,
                      const std::string &iSampleName,
                      index_t iSampleIndex,
                      const void *iSamp,
                      const ScalarSampleKey &iKey );

protected:
    // Use the AbcCoreAbstract's magnificent "ScalarSample"
    // helper class to keep track of our storage.
    AbcA::ScalarSample m_previousSample;
};

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcCoreHDF5
} // End namespace Alembic

#endif
