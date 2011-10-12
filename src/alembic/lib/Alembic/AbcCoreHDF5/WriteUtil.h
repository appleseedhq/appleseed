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

#ifndef _Alembic_AbcCoreHDF5_WriteUtil_h_
#define _Alembic_AbcCoreHDF5_WriteUtil_h_

#include <Alembic/AbcCoreHDF5/Foundation.h>
#include <Alembic/AbcCoreHDF5/WrittenArraySampleMap.h>
#include <Alembic/AbcCoreHDF5/StringWriteUtil.h>

namespace Alembic {
namespace AbcCoreHDF5 {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
WrittenArraySampleMap& GetWrittenArraySampleMap(
    AbcA::ArchiveWriterPtr iArchive );

//-*****************************************************************************
void
WriteDimensions( hid_t iParent,
                 const std::string &iName,
                 const Dimensions &iDims );

//-*****************************************************************************
void
WriteKey( hid_t iParent,
          const std::string &iAttrName,
          const AbcA::ArraySample::Key &iKey );

//-*****************************************************************************
void
WriteMetaData( hid_t iParent,
               const std::string &iName,
               const AbcA::MetaData &iMetaData );

//-*****************************************************************************
void
WriteDataToAttr( hid_t iParent,
                 hid_t iDspace,
                 const std::string &iAttrName,
                 hid_t iFileType,
                 hid_t iNativeType,
                 const void *iData );

//-*****************************************************************************
void
WriteScalar( hid_t iParent,
             const std::string &iName,
             hid_t iFileType,
             hid_t iNativeType,
             const void *iData );

//-*****************************************************************************
void
WriteSmallArray( hid_t iParent,
                 const std::string &iName,
                 hid_t iFileType,
                 hid_t iNativeType,
                 size_t iNumVals,
                 const void *Data );

//-*****************************************************************************
void
CopyWrittenArray( hid_t iParent,
                  const std::string &iName,
                  WrittenArraySampleIDPtr iRef );

//-*****************************************************************************
WrittenArraySampleIDPtr
WriteArray( WrittenArraySampleMap &iMap,
            hid_t iGroup,
            const std::string &iName,
            const AbcA::ArraySample &iSamp,
            const AbcA::ArraySample::Key &iKey,
            hid_t iFileType,
            hid_t iNativeType,
            int iCompressionLevel );

//-*****************************************************************************
void
WritePropertyInfo( hid_t iGroup,
                   const std::string &iName,
                   AbcA::PropertyType iPropertyType,
                   const AbcA::DataType &iDataType,
                   bool isScalarLike,
                   uint32_t iTimeSamplingIndex,
                   uint32_t iNumSamples,
                   uint32_t iFirstChangedIndex,
                   uint32_t iLastChangedIndex );

//-*****************************************************************************
void
WriteTimeSampling( hid_t iGroup,
                   const std::string &iName,
                   const AbcA::TimeSampling &iTsmp );

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcCoreHDF5
} // End namespace Alembic

#endif

