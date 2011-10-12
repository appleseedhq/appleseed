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

#ifndef _Alembic_AbcCoreAbstract_Foundation_h_
#define _Alembic_AbcCoreAbstract_Foundation_h_

#include <Alembic/Util/All.h>

#include <boost/smart_ptr.hpp>
#include <boost/make_shared.hpp>
#include <boost/operators.hpp>
#include <boost/format.hpp>

#include <limits>
#include <utility>
#include <vector>

#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

namespace Alembic {
namespace AbcCoreAbstract {
namespace ALEMBIC_VERSION_NS {

// Just pull the whole Util namespace in. This is safe.
using namespace ::Alembic::Util;

//! Index type
//! Just being pedantic.
typedef int64_t index_t;

//! Chrono type.
//! This is used whenever time values are needed in the library. They are
//! generally assumed to be seconds, but this does not need to be explicitly
//! enforced by the API.
typedef float64_t chrono_t;
// Util/PlainOldDataType.h defines the enum PlainOldDataType
#define kChrono_TPOD  kFloat64POD

//-*****************************************************************************
// Alembic version information:
// 
// Version information is expressed in these locations:
// - CMakeLists.txt as PROJECT_VERSION 
//   . Names used in messages and for install directory
// - lib/Alembic/AbcCoreAbstract/Foundation.h as ALEMBIC_LIBRARY_VERSION
//   . An easy to compare numeric value.
// - lib/Alembic/AbcCoreAbstract/Foundation.cpp
//   . Implementation of handy functions for reporting version info.
//
// - lib/Alembic/AbcCoreHDF5/Foundation.h as ALEMBIC_HDF5_FILE_VERSION
//   . The version number identifying the low level HDF5 structures used
//     to express data types and samples in Alembic

//! Alembic version number Major/Minor/Patch XX.YY.ZZ
#define ALEMBIC_LIBRARY_VERSION 10001

//! Helper function which returns the version and date built in a string
//! e.g. "Alembic 1.0.0 (built Jul  6 2011)"
std::string GetLibraryVersion();
//! Returns just the version number, as a string, of the Alembic library.
//! e.g. "1.0.0"
std::string GetLibraryVersionShort();

//-*****************************************************************************
//! Exception types borrowed from Alembic::Util. We should probably eventually
//! create specific exception types.
#define ABCA_THROW( TEXT ) ABC_THROW( TEXT )

//-*****************************************************************************
#define ABCA_ASSERT( COND, TEXT )               \
do                                              \
{                                               \
    if ( !( COND ) )                            \
    {                                           \
        ABCA_THROW( TEXT );                     \
    }                                           \
}                                               \
while( 0 )


} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcCoreAbstract
} // End namespace Alembic

#endif
