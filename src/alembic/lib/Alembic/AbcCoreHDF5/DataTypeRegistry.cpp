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

#include <Alembic/AbcCoreHDF5/DataTypeRegistry.h>
#include <Alembic/AbcCoreHDF5/HDF5Util.h>

namespace Alembic {
namespace AbcCoreHDF5 {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
using namespace AbcA;

//-*****************************************************************************
static hid_t GetNativeBoolH5T()
{
    hid_t ret = H5Tcopy( H5T_NATIVE_UINT8 );
    H5Tset_size( ret, 1 );
    H5Tset_precision( ret, 1 );
    H5Tset_sign( ret, H5T_SGN_NONE );
    H5Tset_offset( ret, 0 );
    H5Tset_pad( ret, H5T_PAD_ZERO, H5T_PAD_ZERO );
    return ret;
}

//-*****************************************************************************
static hid_t GetFileBoolH5T()
{
    hid_t ret = H5Tcopy( H5T_STD_U8LE );
    H5Tset_size( ret, 1 );
    H5Tset_precision( ret, 1 );
    H5Tset_sign( ret, H5T_SGN_NONE );
    H5Tset_offset( ret, 0 );
    H5Tset_pad( ret, H5T_PAD_ZERO, H5T_PAD_ZERO );
    return ret;
}

//-*****************************************************************************
static hid_t GetNativeHalfH5T()
{
    hid_t ret = H5Tcopy( H5T_NATIVE_FLOAT );
    H5Tset_fields( ret,
                   15,   // sign bit position
                   10,   // exponent lsb position
                   5,    // exponent size
                   0,    // mantissa lsb position
                   10 ); // mantissa size
    H5Tset_size( ret, 2 );
    return ret;
}

//-*****************************************************************************
static hid_t GetFileHalfH5T()
{
    hid_t ret = H5Tcopy( H5T_IEEE_F32LE );
    H5Tset_fields( ret,
                   15,   // sign bit position
                   10,   // exponent lsb position
                   5,    // exponent size
                   0,    // mantissa lsb position
                   10 ); // mantissa size
    H5Tset_size( ret, 2 );
    return ret;
}

//-*****************************************************************************
//-*****************************************************************************
//-*****************************************************************************
// SUPPORT FUNCTIONS
//-*****************************************************************************
//-*****************************************************************************
//-*****************************************************************************
// In order to read or write a slab of a given DataType, we need an H5::Datatype
// for the dataset in the file, and a 'native' one for how it will be
// stored in local memory. We are not bothering with named datatypes for now.
// Though, it may prove timesaving later on.
//
// So - we need some sort of structure which allows you to take an
// AlembicAsset::DataType and come up with an H5::Datatype for it.
hid_t GetNativeH5T( const AbcA::DataType &adt, bool &oCleanUp )
{
    oCleanUp = false;

    hid_t baseDtype = -1;
    switch ( adt.getPod() )
    {
    case kBooleanPOD:
        oCleanUp = true;
        baseDtype = GetNativeBoolH5T();
        break;
    case kUint8POD:     baseDtype = H5T_NATIVE_UINT8; break;
    case kInt8POD:      baseDtype = H5T_NATIVE_INT8; break;
    case kUint16POD:    baseDtype = H5T_NATIVE_UINT16; break;
    case kInt16POD:     baseDtype = H5T_NATIVE_INT16; break;
    case kUint32POD:    baseDtype = H5T_NATIVE_UINT32; break;
    case kInt32POD:     baseDtype = H5T_NATIVE_INT32; break;
    case kUint64POD:    baseDtype = H5T_NATIVE_UINT64; break;
    case kInt64POD:     baseDtype = H5T_NATIVE_INT64; break;
    case kFloat16POD:
        oCleanUp = true;
        baseDtype = GetNativeHalfH5T();
        break;
    case kFloat32POD:   baseDtype = H5T_NATIVE_FLOAT; break;
    case kFloat64POD:   baseDtype = H5T_NATIVE_DOUBLE; break;
    default:
        ABCA_THROW( "Unsuppored POD type: " << PODName( adt.getPod() ) );
    }

    ABCA_ASSERT( baseDtype >= 0, "Bad base datatype id" );

    return baseDtype;
}

//-*****************************************************************************
// Same as above, only this time for the files.
// Alembic uses LittleEndian by default.
hid_t GetFileH5T( const AbcA::DataType &adt, bool &oCleanUp )
{
    oCleanUp = false;
    hid_t baseDtype = -1;
    switch ( adt.getPod() )
    {
    case kBooleanPOD:
        oCleanUp = true;
        baseDtype = GetFileBoolH5T(); break;
    case kUint8POD:     baseDtype = H5T_STD_U8LE; break;
    case kInt8POD:      baseDtype = H5T_STD_I8LE; break;
    case kUint16POD:    baseDtype = H5T_STD_U16LE; break;
    case kInt16POD:     baseDtype = H5T_STD_I16LE; break;
    case kUint32POD:    baseDtype = H5T_STD_U32LE; break;
    case kInt32POD:     baseDtype = H5T_STD_I32LE; break;
    case kUint64POD:    baseDtype = H5T_STD_U64LE; break;
    case kInt64POD:     baseDtype = H5T_STD_I64LE; break;
    case kFloat16POD:
        oCleanUp = true;
        baseDtype = GetFileHalfH5T(); break;
    case kFloat32POD:   baseDtype = H5T_IEEE_F32LE; break;
    case kFloat64POD:   baseDtype = H5T_IEEE_F64LE; break;
    default:
        ABCA_THROW( "Unsuppored POD type: " << PODName( adt.getPod() ) );
    }

    ABCA_ASSERT( baseDtype >= 0, "Bad base datatype id" );

    return baseDtype;
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace AbcCoreHDF5
} // End namespace Alembic
