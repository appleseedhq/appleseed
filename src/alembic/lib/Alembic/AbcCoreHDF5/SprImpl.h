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

#ifndef _Alembic_AbcCoreHDF5_SprImpl_h_
#define _Alembic_AbcCoreHDF5_SprImpl_h_

#include <Alembic/AbcCoreHDF5/Foundation.h>
#include <Alembic/AbcCoreHDF5/SimplePrImpl.h>

namespace Alembic {
namespace AbcCoreHDF5 {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
// The Scalar Property Reader fills up bytes corresponding to memory for
// a single scalar sample at a particular index.
class SprImpl
    : public SimplePrImpl<AbcA::ScalarPropertyReader,
                          SprImpl,
                          void*>
    , public boost::enable_shared_from_this<SprImpl>
{
public:
    SprImpl( AbcA::CompoundPropertyReaderPtr iParent,
             hid_t iParentGroup,
             PropertyHeaderPtr iHeader,
             uint32_t iNumSamples,
             uint32_t iFirstChangedIndex,
             uint32_t iLastChangedIndex )
      : SimplePrImpl<AbcA::ScalarPropertyReader, SprImpl, void*>
        ( iParent, iParentGroup, iHeader, iNumSamples, iFirstChangedIndex,
          iLastChangedIndex )
    {
        if ( m_header->getPropertyType() != AbcA::kScalarProperty )
        {
            ABCA_THROW( "Attempted to create a ScalarPropertyReader from a "
                        "non-scalar property type" );
        }
    }

    virtual AbcA::ScalarPropertyReaderPtr asScalarPtr();

protected:
    friend class SimplePrImpl<AbcA::ScalarPropertyReader,
                              SprImpl,
                              void*>;
    
    // This function is called by SimplePrImpl to provide the actual
    // property reading.
    // It will dispatch its work out to different read utils, based
    // on the type of the property.
    void readSample( hid_t iGroup,
                     const std::string &iSampleName,
                     index_t iSampleIndex,
                     void *oSampleBytes );
    
    //-*************************************************************************
    // This function is called by SimplePrImpl, scalar props do not have keys.
    bool readKey( hid_t iGroup,
                  const std::string &iSampleName,
                  AbcA::ArraySampleKey & oSamplePtr ) { return false; }

};

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcCoreHDF5
} // End namespace Alembic

#endif
