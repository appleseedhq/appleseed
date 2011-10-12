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

#ifndef _Alembic_AbcCoreHDF5_StringReadUtil_h_
#define _Alembic_AbcCoreHDF5_StringReadUtil_h_

#include <Alembic/AbcCoreHDF5/Foundation.h>

namespace Alembic {
namespace AbcCoreHDF5 {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
void
ReadString( hid_t iParent,
            const std::string &iStringName,
            std::string &oString );

//-*****************************************************************************
void
ReadWstring( hid_t iParent,
             const std::string &iStringName,
             std::wstring &oString );

//-*****************************************************************************
// For data types with more than one string.
void
ReadStrings( hid_t iParent,
             const std::string &iStringsName,
             size_t numStrings,
             std::string *oStrings );

//-*****************************************************************************
// For data types with more than one wstring
void
ReadWstrings( hid_t iParent,
              const std::string &iStringsName,
              size_t numStrings,
              std::wstring *oStrings );

//-*****************************************************************************
AbcA::ArraySamplePtr
ReadStringArray( AbcA::ReadArraySampleCachePtr iCache,
                 hid_t iParent,
                 const std::string &iArrayName,
                 const AbcA::DataType &iDataType );

//-*****************************************************************************
AbcA::ArraySamplePtr
ReadWstringArray( AbcA::ReadArraySampleCachePtr iCache,
                  hid_t iParent,
                  const std::string &iArrayName,
                  const AbcA::DataType &iDataType );

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcCoreHDF5
} // End namespace Alembic

#endif
