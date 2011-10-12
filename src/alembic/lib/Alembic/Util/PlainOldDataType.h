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

#ifndef _Alembic_Util_PlainOldDataType_h_
#define _Alembic_Util_PlainOldDataType_h_

#include <Alembic/Util/Foundation.h>
#include <Alembic/Util/Exception.h>

namespace Alembic {
namespace Util {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
//! Bytes are unsigned chars, by definition.
//! We use bytes in Alembic as the name of anonymous storage memory, since
//! it is not possible to create arrays of voids. 
typedef unsigned char           byte_t;

//-*****************************************************************************
//-*****************************************************************************
//! BOOLEAN BASE TYPE - since C++ doesn't explicitly demand that bool
//! be a given bit depth, but we need it to be here, we make our own
//! bool type, which is a bit silly. This is purely for storage reasons.
//-*****************************************************************************
class bool_t
{
public:
    bool_t() : m_byte( 0 ) {}
    
    bool_t( bool tf ) : m_byte( static_cast<byte_t>( tf ) ) {}
    bool_t( byte_t b ) : m_byte( b ) {}

    
    //! Using default copy constructor
    //! ...
    
    //! Using default assignment operator
    //! ...
    
    bool_t& operator=( bool tf )
    {
        m_byte = static_cast<byte_t>( tf );
        return *this;
    }

    bool_t& operator=( byte_t b )
    {
        m_byte = b;
        return *this;
    }

    bool_t operator!( void )
    {
        return bool_t( m_byte == 0 );
    }

    bool asBool() const { return ( m_byte != 0 ); }
    operator bool() const { return ( m_byte != 0 ); }

private:
    byte_t m_byte;
};

//-*****************************************************************************
inline bool operator==( const bool_t &a, const bool_t &b )
{
    return a.asBool() == b.asBool();
}

//-*****************************************************************************
inline bool operator==( const bool_t &a, bool b )
{
    return a.asBool() == b;
}

//-*****************************************************************************
inline bool operator==( bool a, const bool_t &b )
{
    return a == b.asBool();
}

//-*****************************************************************************
inline bool operator!=( const bool_t &a, const bool_t &b )
{
    return a.asBool() != b.asBool();
}

//-*****************************************************************************
inline bool operator!=( const bool_t &a, bool b )
{
    return a.asBool() != b;
}

//-*****************************************************************************
inline bool operator!=( bool a, const bool_t &b )
{
    return a != b.asBool();
}

//-*****************************************************************************
//! Name standardization and namespace promotion of explicit types.
//! The reason we use the boost types instead of the cstdint types is because
//! Microsoft has wisely chosen to omit <cstdint> from Windows Visual C++.
//! Grumble. boost::cstdint.hpp remedies this oversight.
using boost::uint8_t;
using boost::int8_t;
using boost::uint16_t;
using boost::int16_t;
using boost::uint32_t;
using boost::int32_t;
using boost::uint64_t;
using boost::int64_t;
typedef half                    float16_t;
typedef float                   float32_t;
typedef double                  float64_t;

//! Last, but not least, standard strings.
//! These are CLEARLY not "Plain Old Data Types", however, "strings" are
//! such ubiquitous components of programming, and without an enclosing
//! structure like std::string, they're so difficult to use from an API
//! point of view (call first time to find out length! allocate your own array!
//! call second time to get string value!), that I'm going to put my foot down
//! and say - from Alembic's point of view, std::string and std::wstring are
//! "Kinda Sorta POD types". Please pardon the abuse of the idiom.
using std::string;
using std::wstring;

//-*****************************************************************************
//! I'm using explicit names here because the terms 'int', 'short', 'long', etc,
//! have different bit-depths on different machine architectures. To avoid
//! any ambiguity whatsoever, I'm just making these explicit. End users will
//! rarely see these anyway, so it's okay to be a bit pedantic.
//!
//! These are always represented in the endian-ness of the host machine when
//! resident in working memory, but need to have an explicit endian-ness when
//! being written out. That's hidden from the user by HDF5.
enum PlainOldDataType
{
    //! Booleans are difficult to store in arrays in a 'one bit per bool'
    //! kind of way, so we actually file them as bytes (uint8).  But again
    //! this is entirely hidden from the end user. Implemented via the
    //! "bool_t" type defined above.
    kBooleanPOD,

    //! Char/UChar
    kUint8POD,
    kInt8POD,

    //! Short/UShort
    kUint16POD,
    kInt16POD,

    //! Int/UInt
    kUint32POD,
    kInt32POD,

    //! Long/ULong
    kUint64POD,
    kInt64POD,

    //! Half/Float/Double
    kFloat16POD,
    kFloat32POD,
    kFloat64POD,

    //! String Pointer
    kStringPOD,

    //! Wide String Pointer
    kWstringPOD,

    //! Number of POD
    kNumPlainOldDataTypes,

    //! Unknown
    kUnknownPOD = 127
};

//-*****************************************************************************
//-*****************************************************************************
//-*****************************************************************************
// A little traits class that binds these things together.
//-*****************************************************************************
//-*****************************************************************************
//-*****************************************************************************
template <PlainOldDataType PODT, class T > struct PODTraits {};

//-*****************************************************************************
//! Unfortunately, C++ only allows for static const declaration of constants
//! with integral types, not floating. Therefore, we have the whole
//! inlined static function for default values.
#define DECLARE_TRAITS( PENUM, PTYPE, PNAME, DFLT, PTDEF )                    \
template <>                                                                   \
struct PODTraits< PENUM , PTYPE >                                             \
{                                                                             \
    static const PlainOldDataType       pod_enum = PENUM ;                    \
    typedef PTYPE                       value_type ;                          \
    static const char *                 name() { return PNAME ; }             \
    static PTYPE                        default_value()                       \
    { return ( DFLT ) ; }                                                     \
    static size_t                       numBytes()                            \
    { return sizeof( PTYPE ) ; }                                              \
};                                                                            \
typedef PODTraits< PENUM , PTYPE > PTDEF 

//-*****************************************************************************
// Actual specialized traits
DECLARE_TRAITS( kBooleanPOD, bool_t,    "bool_t",    false, BooleanPODTraits );
DECLARE_TRAITS( kUint8POD,   uint8_t,   "uint8_t",   0,     Uint8PODTraits );
DECLARE_TRAITS( kInt8POD,    int8_t,    "int8_t",    0,     Int8PODTraits );
DECLARE_TRAITS( kUint16POD,  uint16_t,  "uint16_t",  0,     Uint16PODTraits );
DECLARE_TRAITS( kInt16POD,   int16_t,   "int16_t",   0,     Int16PODTraits );
DECLARE_TRAITS( kUint32POD,  uint32_t,  "uint32_t",  0,     Uint32PODTraits );
DECLARE_TRAITS( kInt32POD,   int32_t,   "int32_t",   0,     Int32PODTraits );
DECLARE_TRAITS( kUint64POD,  uint64_t,  "uint64_t",  0,     Uint64PODTraits );
DECLARE_TRAITS( kInt64POD,   int64_t,   "int64_t",   0,     Int64PODTraits );
DECLARE_TRAITS( kFloat16POD, float16_t, "float16_t", 0,     Float16PODTraits );
DECLARE_TRAITS( kFloat32POD, float32_t, "float32_t", 0,     Float32PODTraits );
DECLARE_TRAITS( kFloat64POD, float64_t, "float64_t", 0,     Float64PODTraits );
DECLARE_TRAITS( kStringPOD,  string,    "string",    "",    StringPODTraits );
DECLARE_TRAITS( kWstringPOD, wstring,   "wstring",   L"",   WstringPODTraits );

#undef DECLARE_TRAITS

//-*****************************************************************************
//-*****************************************************************************
// Okay, now tools for extracting POD Traits from enums and from types.
// No easy way to do it from a name.
//-*****************************************************************************

//-*****************************************************************************
//-*****************************************************************************
// FROM ENUMS
//-*****************************************************************************
//-*****************************************************************************
template <PlainOldDataType PENUM>
struct PODTraitsFromEnum {};

//-*****************************************************************************
// Actual specializations
template <> struct PODTraitsFromEnum<kBooleanPOD> : public BooleanPODTraits {};
template <> struct PODTraitsFromEnum<kUint8POD> : public Uint8PODTraits {};
template <> struct PODTraitsFromEnum<kInt8POD> : public Int8PODTraits {};
template <> struct PODTraitsFromEnum<kUint16POD> : public Uint16PODTraits {};
template <> struct PODTraitsFromEnum<kInt16POD> : public Int16PODTraits {};
template <> struct PODTraitsFromEnum<kUint32POD> : public Uint32PODTraits {};
template <> struct PODTraitsFromEnum<kInt32POD> : public Int32PODTraits {};
template <> struct PODTraitsFromEnum<kUint64POD> : public Uint64PODTraits {};
template <> struct PODTraitsFromEnum<kInt64POD> : public Int64PODTraits {};
template <> struct PODTraitsFromEnum<kFloat16POD> : public Float16PODTraits {};
template <> struct PODTraitsFromEnum<kFloat32POD> : public Float32PODTraits {};
template <> struct PODTraitsFromEnum<kFloat64POD> : public Float64PODTraits {};
template <> struct PODTraitsFromEnum<kStringPOD> : public StringPODTraits {};
template <> struct PODTraitsFromEnum<kWstringPOD> : public WstringPODTraits {};

//-*****************************************************************************
//-*****************************************************************************
// FROM TYPES
//-*****************************************************************************
//-*****************************************************************************
template <class PTYPE>
struct PODTraitsFromType {};

//-*****************************************************************************
// Actual specializations
template <> struct PODTraitsFromType<bool_t> : public BooleanPODTraits {};
template <> struct PODTraitsFromType<uint8_t> : public Uint8PODTraits {};
template <> struct PODTraitsFromType<int8_t> : public Int8PODTraits {};
template <> struct PODTraitsFromType<uint16_t> : public Uint16PODTraits {};
template <> struct PODTraitsFromType<int16_t> : public Int16PODTraits {};
template <> struct PODTraitsFromType<uint32_t> : public Uint32PODTraits {};
template <> struct PODTraitsFromType<int32_t> : public Int32PODTraits {};
template <> struct PODTraitsFromType<uint64_t> : public Uint64PODTraits {};
template <> struct PODTraitsFromType<int64_t> : public Int64PODTraits {};
template <> struct PODTraitsFromType<float16_t> : public Float16PODTraits {};
template <> struct PODTraitsFromType<float32_t> : public Float32PODTraits {};
template <> struct PODTraitsFromType<float64_t> : public Float64PODTraits {};
template <> struct PODTraitsFromType<string> : public StringPODTraits {};
template <> struct PODTraitsFromType<wstring> : public WstringPODTraits {};

//-*****************************************************************************
//-*****************************************************************************
// Some runtime stuff, for when templates won't help.
//-*****************************************************************************
//-*****************************************************************************
inline size_t PODNumBytes( PlainOldDataType pod )
{
    switch ( pod )
    {
    case kBooleanPOD: return BooleanPODTraits::numBytes();
    case kUint8POD: return Uint8PODTraits::numBytes();
    case kInt8POD: return Int8PODTraits::numBytes();
    case kUint16POD: return Uint16PODTraits::numBytes();
    case kInt16POD: return Int16PODTraits::numBytes();
    case kUint32POD: return Uint32PODTraits::numBytes();
    case kInt32POD: return Int32PODTraits::numBytes();
    case kUint64POD: return Uint64PODTraits::numBytes();
    case kInt64POD: return Int64PODTraits::numBytes();
    case kFloat16POD: return Float16PODTraits::numBytes();
    case kFloat32POD: return Float32PODTraits::numBytes();
    case kFloat64POD: return Float64PODTraits::numBytes();
    case kStringPOD: return StringPODTraits::numBytes();
    case kWstringPOD: return WstringPODTraits::numBytes();
    default:
        // Badness!
        assert( false );
        return 0;
    };
}

//-*****************************************************************************
inline const char *PODName( PlainOldDataType pod )
{
    switch ( pod )
    {
    case kBooleanPOD: return BooleanPODTraits::name();
    case kUint8POD: return Uint8PODTraits::name();
    case kInt8POD: return Int8PODTraits::name();
    case kUint16POD: return Uint16PODTraits::name();
    case kInt16POD: return Int16PODTraits::name();
    case kUint32POD: return Uint32PODTraits::name();
    case kInt32POD: return Int32PODTraits::name();
    case kUint64POD: return Uint64PODTraits::name();
    case kInt64POD: return Int64PODTraits::name();
    case kFloat16POD: return Float16PODTraits::name();
    case kFloat32POD: return Float32PODTraits::name();
    case kFloat64POD: return Float64PODTraits::name();
    case kStringPOD: return StringPODTraits::name();
    case kWstringPOD: return WstringPODTraits::name();
    default:
        // Can't throw from here, so just return 0.
        // assert( false );
        return "UNKNOWN";
        // return 0;
    };
}

//-*****************************************************************************
inline PlainOldDataType PODFromName( const std::string &n )
{
    if ( n == BooleanPODTraits::name() ) return BooleanPODTraits::pod_enum;
    else if ( n == Uint8PODTraits::name() ) return Uint8PODTraits::pod_enum;
    else if ( n == Int8PODTraits::name() ) return Int8PODTraits::pod_enum;
    else if ( n == Uint16PODTraits::name() ) return Uint16PODTraits::pod_enum;
    else if ( n == Int16PODTraits::name() ) return Int16PODTraits::pod_enum;
    else if ( n == Uint32PODTraits::name() ) return Uint32PODTraits::pod_enum;
    else if ( n == Int32PODTraits::name() ) return Int32PODTraits::pod_enum;
    else if ( n == Uint64PODTraits::name() ) return Uint64PODTraits::pod_enum;
    else if ( n == Int64PODTraits::name() ) return Int64PODTraits::pod_enum;
    else if ( n == Float16PODTraits::name() ) return Float16PODTraits::pod_enum;
    else if ( n == Float32PODTraits::name() ) return Float32PODTraits::pod_enum;
    else if ( n == Float64PODTraits::name() ) return Float64PODTraits::pod_enum;
    else if ( n == StringPODTraits::name() ) return StringPODTraits::pod_enum;
    else if ( n == WstringPODTraits::name() ) return WstringPODTraits::pod_enum;
    else return kUnknownPOD;
}

//-*****************************************************************************
//! This actually does work with strings!
template <PlainOldDataType POD>
inline void PODSetDefaultPOD( void *addr )
{
    typedef typename PODTraitsFromEnum<POD>::value_type value_type;
    value_type *valPtr = reinterpret_cast<value_type*>( addr );
    if ( valPtr ) { *valPtr = PODTraitsFromEnum<POD>::default_value(); }
}

//-*****************************************************************************
inline void PODSetDefault( PlainOldDataType pod, void *bytes )
{
    switch ( pod )
    {
    case kBooleanPOD: PODSetDefaultPOD<kBooleanPOD>( bytes ); return;
    case kUint8POD: PODSetDefaultPOD<kUint8POD>( bytes ); return;
    case kInt8POD: PODSetDefaultPOD<kInt8POD>( bytes ); return;
    case kUint16POD: PODSetDefaultPOD<kUint16POD>( bytes ); return;
    case kInt16POD: PODSetDefaultPOD<kInt16POD>( bytes ); return;
    case kUint32POD: PODSetDefaultPOD<kUint32POD>( bytes ); return;
    case kInt32POD: PODSetDefaultPOD<kInt32POD>( bytes ); return;
    case kUint64POD: PODSetDefaultPOD<kUint64POD>( bytes ); return;
    case kInt64POD: PODSetDefaultPOD<kInt64POD>( bytes ); return;
    case kFloat16POD: PODSetDefaultPOD<kFloat16POD>( bytes ); return;
    case kFloat32POD: PODSetDefaultPOD<kFloat32POD>( bytes ); return;
    case kFloat64POD: PODSetDefaultPOD<kFloat64POD>( bytes ); return;

        // This isn't tremendously valid for the string types. Eeek.
    case kStringPOD: PODSetDefaultPOD<kStringPOD>( bytes ); return;
    case kWstringPOD: PODSetDefaultPOD<kWstringPOD>( bytes ); return;
    default:
        // Can't throw, but in debug...
        assert( false );
    };
}

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace Util
} // End namespace Alembic

#endif

