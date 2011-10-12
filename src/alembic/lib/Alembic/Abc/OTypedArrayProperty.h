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
#ifndef _Alembic_Abc_OTypedArrayProperty_h_
#define _Alembic_Abc_OTypedArrayProperty_h_

#include <Alembic/Abc/Foundation.h>
#include <Alembic/Abc/OArrayProperty.h>
#include <Alembic/Abc/TypedPropertyTraits.h>
#include <Alembic/Abc/TypedArraySample.h>

namespace Alembic {
namespace Abc {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
template <class TRAITS>
class OTypedArrayProperty : public OArrayProperty
{
public:
    //-*************************************************************************
    // TYPEDEFS AND IDENTIFIERS
    //-*************************************************************************
    typedef TRAITS traits_type;
    typedef OTypedArrayProperty<TRAITS> this_type;
    typedef typename TRAITS::value_type value_type;
    typedef TypedArraySample<TRAITS> sample_type;

    //! Return the interpretation expected of this
    //! property. An empty interpretation matches everything
    static const std::string &getInterpretation()
    {
        static std::string sInterpretation = TRAITS::interpretation();
        return sInterpretation;
    }

    //! This will check whether or not a given entity (as represented by
    //! a metadata) strictly matches the interpretation of this
    //! typed property
    static bool matches( const AbcA::MetaData &iMetaData,
                         SchemaInterpMatching iMatching = kStrictMatching )
    {
        return ( getInterpretation() == "" ||
                 ( iMetaData.get( "interpretation" ) ==
                   getInterpretation() ) );
    }

    //! This will check whether or not a given object (as represented by
    //! an property header) strictly matches the interpretation of this
    //! typed property, as well as the data type.
    static bool matches( const AbcA::PropertyHeader &iHeader,
                         SchemaInterpMatching iMatching = kStrictMatching )
    {
        return ( iHeader.getDataType() == TRAITS::dataType() ) &&
            matches( iHeader.getMetaData(), iMatching );
    }

    //-*************************************************************************
    // CONSTRUCTION, DESTRUCTION, ASSIGNMENT
    //-*************************************************************************

    //! Create a default (empty) TypedArrayProperty
    //! ...
    OTypedArrayProperty() {}

    //! Create a new TypedArrayProperty
    //! as a child of the passed COMPOUND_PTR
    //! Arguments can specify metadata, timesampling, and error handling.
    template <class COMPOUND_PTR>
    OTypedArrayProperty(
        COMPOUND_PTR iParent,
        const std::string &iName,

        const Argument &iArg0 = Argument(),
        const Argument &iArg1 = Argument(),
        const Argument &iArg2 = Argument() );

    //! Wrap an existing property. This will check to make sure
    //! it can wrap.
    OTypedArrayProperty(
        AbcA::ArrayPropertyWriterPtr iProp,
        WrapExistingFlag iWrapFlag,
        const Argument &iArg0 = Argument(),
        const Argument &iArg1 = Argument() );

    //-*************************************************************************
    // ARRAY PROPERTY FEATURES
    //-*************************************************************************

    //! Set a sample using a reference to a typed array sample-type,
    //! instead of a void* ArraySample
    void set( const sample_type &iVal )
    {
        OArrayProperty::set( iVal );
    }
};

//-*****************************************************************************
// TEMPLATE AND INLINE FUNCTIONS
//-*****************************************************************************

//-*****************************************************************************
template <class TRAITS>
template <class COMPOUND_PTR>
OTypedArrayProperty<TRAITS>::OTypedArrayProperty
(
    COMPOUND_PTR iParent,
    const std::string &iName,

    const Argument &iArg0,
    const Argument &iArg1,
    const Argument &iArg2 )
{
    Arguments args( GetErrorHandlerPolicy( iParent ) );
    iArg0.setInto( args );
    iArg1.setInto( args );
    iArg2.setInto( args );

    getErrorHandler().setPolicy( args.getErrorHandlerPolicy() );

    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OTypedArrayProperty::init()" );

    // Get actual writer for parent.
    AbcA::CompoundPropertyWriterPtr parent =
        GetCompoundPropertyWriterPtr( iParent );
    ABCA_ASSERT( parent, "NULL CompoundPropertyWriterPtr" );

    // Put interpretation into metadata.
    AbcA::MetaData mdata = args.getMetaData();
    if ( getInterpretation() != "" )
    {
        mdata.set( "interpretation", getInterpretation() );
    }

    // Create property.
    AbcA::TimeSamplingPtr tsPtr = args.getTimeSampling();
    uint32_t tsIndex = args.getTimeSamplingIndex();

    // if we specified a valid TimeSamplingPtr, use it to determine the index
    // otherwise we'll use the index, which defaults to the intrinsic 0 index
    if (tsPtr)
    {
        tsIndex = parent->getObject()->getArchive()->addTimeSampling(*tsPtr);
    }

    m_property = parent->createArrayProperty( iName, mdata, 
        TRAITS::dataType(), tsIndex );

    ALEMBIC_ABC_SAFE_CALL_END_RESET();
}

//-*****************************************************************************
template<class TRAITS>
inline OTypedArrayProperty<TRAITS>::OTypedArrayProperty(
    AbcA::ArrayPropertyWriterPtr iProperty,
    WrapExistingFlag iFlag,
    const Argument &iArg0,
    const Argument &iArg1 )
  : OArrayProperty( iProperty,
                    iFlag,
                    GetErrorHandlerPolicy( iProperty, iArg0, iArg1 ) )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN(
        "OTypedArrayProperty::OTypedArrayProperty()" );

    const AbcA::PropertyHeader &pheader = iProperty->getHeader();

    ABCA_ASSERT( matches( pheader, GetSchemaInterpMatching( iArg0, iArg1 ) ),

                 "Incorrect match of header datatype: "
                 << pheader.getDataType()
                 << " to expected: "
                 << TRAITS::dataType()
                 << ",\n...or incorrect match of interpretation: "
                 << pheader.getMetaData().get( "interpretation" )
                 << " to expected: "
                 << TRAITS::interpretation() );

    ALEMBIC_ABC_SAFE_CALL_END_RESET();
}

//-*****************************************************************************
//-*****************************************************************************
//-*****************************************************************************

typedef OTypedArrayProperty<BooleanTPTraits>         OBoolArrayProperty;
typedef OTypedArrayProperty<Uint8TPTraits>           OUcharArrayProperty;
typedef OTypedArrayProperty<Int8TPTraits>            OCharArrayProperty;
typedef OTypedArrayProperty<Uint16TPTraits>          OUInt16ArrayProperty;
typedef OTypedArrayProperty<Int16TPTraits>           OInt16ArrayProperty;
typedef OTypedArrayProperty<Uint32TPTraits>          OUInt32ArrayProperty;
typedef OTypedArrayProperty<Int32TPTraits>           OInt32ArrayProperty;
typedef OTypedArrayProperty<Uint64TPTraits>          OUInt64ArrayProperty;
typedef OTypedArrayProperty<Int64TPTraits>           OInt64ArrayProperty;
typedef OTypedArrayProperty<Float16TPTraits>         OHalfArrayProperty;
typedef OTypedArrayProperty<Float32TPTraits>         OFloatArrayProperty;
typedef OTypedArrayProperty<Float64TPTraits>         ODoubleArrayProperty;
typedef OTypedArrayProperty<StringTPTraits>          OStringArrayProperty;
typedef OTypedArrayProperty<WstringTPTraits>         OWstringArrayProperty;

typedef OTypedArrayProperty<V2sTPTraits>             OV2sArrayProperty;
typedef OTypedArrayProperty<V2iTPTraits>             OV2iArrayProperty;
typedef OTypedArrayProperty<V2fTPTraits>             OV2fArrayProperty;
typedef OTypedArrayProperty<V2dTPTraits>             OV2dArrayProperty;

typedef OTypedArrayProperty<V3sTPTraits>             OV3sArrayProperty;
typedef OTypedArrayProperty<V3iTPTraits>             OV3iArrayProperty;
typedef OTypedArrayProperty<V3fTPTraits>             OV3fArrayProperty;
typedef OTypedArrayProperty<V3dTPTraits>             OV3dArrayProperty;

typedef OTypedArrayProperty<P2sTPTraits>             OP2sArrayProperty;
typedef OTypedArrayProperty<P2iTPTraits>             OP2iArrayProperty;
typedef OTypedArrayProperty<P2fTPTraits>             OP2fArrayProperty;
typedef OTypedArrayProperty<P2dTPTraits>             OP2dArrayProperty;

typedef OTypedArrayProperty<P3sTPTraits>             OP3sArrayProperty;
typedef OTypedArrayProperty<P3iTPTraits>             OP3iArrayProperty;
typedef OTypedArrayProperty<P3fTPTraits>             OP3fArrayProperty;
typedef OTypedArrayProperty<P3dTPTraits>             OP3dArrayProperty;

typedef OTypedArrayProperty<Box2sTPTraits>           OBox2sArrayProperty;
typedef OTypedArrayProperty<Box2iTPTraits>           OBox2iArrayProperty;
typedef OTypedArrayProperty<Box2fTPTraits>           OBox2fArrayProperty;
typedef OTypedArrayProperty<Box2dTPTraits>           OBox2dArrayProperty;

typedef OTypedArrayProperty<Box3sTPTraits>           OBox3sArrayProperty;
typedef OTypedArrayProperty<Box3iTPTraits>           OBox3iArrayProperty;
typedef OTypedArrayProperty<Box3fTPTraits>           OBox3fArrayProperty;
typedef OTypedArrayProperty<Box3dTPTraits>           OBox3dArrayProperty;

typedef OTypedArrayProperty<M33fTPTraits>            OM33fArrayProperty;
typedef OTypedArrayProperty<M33dTPTraits>            OM33dArrayProperty;
typedef OTypedArrayProperty<M44fTPTraits>            OM44fArrayProperty;
typedef OTypedArrayProperty<M44dTPTraits>            OM44dArrayProperty;

typedef OTypedArrayProperty<QuatfTPTraits>           OQuatfArrayProperty;
typedef OTypedArrayProperty<QuatdTPTraits>           OQuatdArrayProperty;

typedef OTypedArrayProperty<C3hTPTraits>             OC3hArrayProperty;
typedef OTypedArrayProperty<C3fTPTraits>             OC3fArrayProperty;
typedef OTypedArrayProperty<C3cTPTraits>             OC3cArrayProperty;

typedef OTypedArrayProperty<C4hTPTraits>             OC4hArrayProperty;
typedef OTypedArrayProperty<C4fTPTraits>             OC4fArrayProperty;
typedef OTypedArrayProperty<C4cTPTraits>             OC4cArrayProperty;

typedef OTypedArrayProperty<N2fTPTraits>             ON2fArrayProperty;
typedef OTypedArrayProperty<N2dTPTraits>             ON2dArrayProperty;

typedef OTypedArrayProperty<N3fTPTraits>             ON3fArrayProperty;
typedef OTypedArrayProperty<N3dTPTraits>             ON3dArrayProperty;

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace Abc
} // End namespace Alembic

#endif
