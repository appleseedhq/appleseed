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

#ifndef _Alembic_Abc_TypedPropertyTraits_h_
#define _Alembic_Abc_TypedPropertyTraits_h_

#include <Alembic/Abc/Foundation.h>

//-*****************************************************************************
// The property traits class maps ValueTypes with their protocol
// strings.

namespace Alembic {
namespace Abc {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
//-*****************************************************************************
// This macro relies on there being a default constructor for the type
// VAL
#define ALEMBIC_ABC_DECLARE_TYPE_TRAITS( VAL, POD, EXTENT, INTERP, PTDEF ) \
struct PTDEF                                                            \
{                                                                       \
    typedef VAL         value_type;                                     \
    static const char * interpretation()  { return ( INTERP ) ; }       \
    static AbcA::DataType     dataType()                                \
    { return AbcA::DataType( POD, EXTENT ) ; }                          \
    static value_type   defaultValue()                                  \
    { value_type v; return v; }                                         \
}

//-*****************************************************************************
//-*****************************************************************************
#define DECLARE_POD_TRAITS( POD_TYPE , PTDEF )                          \
struct PTDEF                                                            \
{                                                                       \
    typedef POD_TYPE    value_type;                                     \
    static const char * interpretation()  { return ""; }                \
    static AbcA::DataType     dataType()                                \
    { return AbcA::DataType( PODTraitsFromType< POD_TYPE >::pod_enum, 1 ); } \
    static value_type   defaultValue()                                  \
    { return PODTraitsFromType< POD_TYPE >::default_value(); }    \
}

//-*****************************************************************************
DECLARE_POD_TRAITS( bool_t, BooleanTPTraits );
DECLARE_POD_TRAITS( uint8_t, Uint8TPTraits );
DECLARE_POD_TRAITS( int8_t, Int8TPTraits );
DECLARE_POD_TRAITS( uint16_t, Uint16TPTraits );
DECLARE_POD_TRAITS( int16_t, Int16TPTraits );
DECLARE_POD_TRAITS( uint32_t, Uint32TPTraits );
DECLARE_POD_TRAITS( int32_t, Int32TPTraits );
DECLARE_POD_TRAITS( uint64_t, Uint64TPTraits );
DECLARE_POD_TRAITS( int64_t, Int64TPTraits );
DECLARE_POD_TRAITS( float16_t, Float16TPTraits );
DECLARE_POD_TRAITS( float32_t, Float32TPTraits );
DECLARE_POD_TRAITS( float64_t, Float64TPTraits );
DECLARE_POD_TRAITS( string, StringTPTraits );
DECLARE_POD_TRAITS( wstring, WstringTPTraits );

#undef DECLARE_POD_TRAITS

//-*****************************************************************************
ALEMBIC_ABC_DECLARE_TYPE_TRAITS( V2s, kInt16POD, 2, "vector", V2sTPTraits );
ALEMBIC_ABC_DECLARE_TYPE_TRAITS( V2i, kInt32POD, 2, "vector", V2iTPTraits );
ALEMBIC_ABC_DECLARE_TYPE_TRAITS( V2f, kFloat32POD, 2, "vector", V2fTPTraits );
ALEMBIC_ABC_DECLARE_TYPE_TRAITS( V2d, kFloat64POD, 2, "vector", V2dTPTraits );

ALEMBIC_ABC_DECLARE_TYPE_TRAITS( V3s, kInt16POD, 3, "vector", V3sTPTraits );
ALEMBIC_ABC_DECLARE_TYPE_TRAITS( V3i, kInt32POD, 3, "vector", V3iTPTraits );
ALEMBIC_ABC_DECLARE_TYPE_TRAITS( V3f, kFloat32POD, 3, "vector", V3fTPTraits );
ALEMBIC_ABC_DECLARE_TYPE_TRAITS( V3d, kFloat64POD, 3, "vector", V3dTPTraits );

ALEMBIC_ABC_DECLARE_TYPE_TRAITS( V2s, kInt16POD, 2, "point", P2sTPTraits );
ALEMBIC_ABC_DECLARE_TYPE_TRAITS( V2i, kInt32POD, 2, "point", P2iTPTraits );
ALEMBIC_ABC_DECLARE_TYPE_TRAITS( V2f, kFloat32POD, 2, "point", P2fTPTraits );
ALEMBIC_ABC_DECLARE_TYPE_TRAITS( V2d, kFloat64POD, 2, "point", P2dTPTraits );

ALEMBIC_ABC_DECLARE_TYPE_TRAITS( V3s, kInt16POD, 3, "point", P3sTPTraits );
ALEMBIC_ABC_DECLARE_TYPE_TRAITS( V3i, kInt32POD, 3, "point", P3iTPTraits );
ALEMBIC_ABC_DECLARE_TYPE_TRAITS( V3f, kFloat32POD, 3, "point", P3fTPTraits );
ALEMBIC_ABC_DECLARE_TYPE_TRAITS( V3d, kFloat64POD, 3, "point", P3dTPTraits );

ALEMBIC_ABC_DECLARE_TYPE_TRAITS( Box2s, kInt16POD, 4, "box", Box2sTPTraits );
ALEMBIC_ABC_DECLARE_TYPE_TRAITS( Box2i, kInt32POD, 4, "box", Box2iTPTraits );
ALEMBIC_ABC_DECLARE_TYPE_TRAITS( Box2f, kFloat32POD, 4, "box", Box2fTPTraits );
ALEMBIC_ABC_DECLARE_TYPE_TRAITS( Box2d, kFloat64POD, 4, "box", Box2dTPTraits );

ALEMBIC_ABC_DECLARE_TYPE_TRAITS( Box3s, kInt16POD, 6, "box", Box3sTPTraits );
ALEMBIC_ABC_DECLARE_TYPE_TRAITS( Box3i, kInt32POD, 6, "box", Box3iTPTraits );
ALEMBIC_ABC_DECLARE_TYPE_TRAITS( Box3f, kFloat32POD, 6, "box", Box3fTPTraits );
ALEMBIC_ABC_DECLARE_TYPE_TRAITS( Box3d, kFloat64POD, 6, "box", Box3dTPTraits );

ALEMBIC_ABC_DECLARE_TYPE_TRAITS( M33f, kFloat32POD, 9, "matrix", M33fTPTraits );
ALEMBIC_ABC_DECLARE_TYPE_TRAITS( M33d, kFloat64POD, 9, "matrix", M33dTPTraits );
ALEMBIC_ABC_DECLARE_TYPE_TRAITS( M44f, kFloat32POD, 16, "matrix", M44fTPTraits );
ALEMBIC_ABC_DECLARE_TYPE_TRAITS( M44d, kFloat64POD, 16, "matrix", M44dTPTraits );

ALEMBIC_ABC_DECLARE_TYPE_TRAITS( Quatf, kFloat32POD, 4, "quat", QuatfTPTraits );
ALEMBIC_ABC_DECLARE_TYPE_TRAITS( Quatd, kFloat64POD, 4, "quat", QuatdTPTraits );

//-*****************************************************************************
// colors. For now, using "rgb"/"rgba" as the interpretation
ALEMBIC_ABC_DECLARE_TYPE_TRAITS( C3h, kFloat16POD, 3, "rgb", C3hTPTraits );
ALEMBIC_ABC_DECLARE_TYPE_TRAITS( C3f, kFloat32POD, 3, "rgb", C3fTPTraits );
ALEMBIC_ABC_DECLARE_TYPE_TRAITS( C3c, kUint8POD, 3, "rgb", C3cTPTraits );

ALEMBIC_ABC_DECLARE_TYPE_TRAITS( C4h, kFloat16POD, 4, "rgba", C4hTPTraits );
ALEMBIC_ABC_DECLARE_TYPE_TRAITS( C4f, kFloat32POD, 4, "rgba", C4fTPTraits );
ALEMBIC_ABC_DECLARE_TYPE_TRAITS( C4c, kUint8POD, 4, "rgba", C4cTPTraits );

//-*****************************************************************************
// Normals.
ALEMBIC_ABC_DECLARE_TYPE_TRAITS( V2f, kFloat32POD, 2, "normal", N2fTPTraits );
ALEMBIC_ABC_DECLARE_TYPE_TRAITS( V2d, kFloat64POD, 2, "normal", N2dTPTraits );

// N3f and N3d is typedefed in Foundation from V3f and V3d
ALEMBIC_ABC_DECLARE_TYPE_TRAITS( N3f, kFloat32POD, 3, "normal", N3fTPTraits );
ALEMBIC_ABC_DECLARE_TYPE_TRAITS( N3d, kFloat64POD, 3, "normal", N3dTPTraits );




} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace Abc
} // End namespace Alembic

#endif
