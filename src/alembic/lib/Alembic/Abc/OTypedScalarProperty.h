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
#ifndef _Alembic_Abc_OTypedScalarProperty_h_
#define _Alembic_Abc_OTypedScalarProperty_h_

#include <Alembic/Abc/Foundation.h>
#include <Alembic/Abc/OScalarProperty.h>
#include <Alembic/Abc/TypedPropertyTraits.h>

namespace Alembic {
namespace Abc {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
template <class TRAITS>
class OTypedScalarProperty : public OScalarProperty
{
public:
    //-*************************************************************************
    // TYPE IDENTIFICATION
    //-*************************************************************************
    typedef TRAITS traits_type;
    typedef OTypedScalarProperty<TRAITS> this_type;
    typedef typename TRAITS::value_type value_type;

    //! Return the interpretation expected of this
    //! property. An empty interpretation matches everything
    static const std::string &getInterpretation()
    {
        static std::string sInterpretation = TRAITS::interpretation();
        return sInterpretation;
    }

    //! This will check whether or not a given entity (as represented by
    //! a metadata) strictly matches the interpretation of this
    //! schema object
    static bool matches( const AbcA::MetaData &iMetaData,
                         SchemaInterpMatching iMatching = kStrictMatching )
    {
        return ( getInterpretation() == "" ||
                 ( iMetaData.get( "interpretation" ) ==
                   getInterpretation() ) );
    }

    //! This will check whether or not a given object (as represented by
    //! an object header) strictly matches the interpretation of this
    //! schema object, as well as the data type.
    static bool matches( const AbcA::PropertyHeader &iHeader,
                         SchemaInterpMatching iMatching = kStrictMatching )
    {
        return ( iHeader.getDataType() == TRAITS::dataType() ) &&
                 matches( iHeader.getMetaData(), iMatching );
    }

    //-*************************************************************************
    // CONSTRUCTION, DESTRUCTION, ASSIGNMENT
    //-*************************************************************************

    //! Create a default (empty) TypedScalarProperty
    //! ...
    OTypedScalarProperty() {}

    //! Create a new TypedScalarProperty
    //! as a child of the passed COMPOUND_PTR
    //! Arguments can specify metadata, timesampling, and error handling.
    template <class COMPOUND_PTR>
    OTypedScalarProperty(
        COMPOUND_PTR iParent,
        const std::string &iName,

        const Argument &iArg0 = Argument(),
        const Argument &iArg1 = Argument(),
        const Argument &iArg2 = Argument() );

    //! Wrap an existing scalar property,
    //! checking to make sure it matches data type and also
    //! (if requested) interpretation.
    OTypedScalarProperty(
        AbcA::ScalarPropertyWriterPtr iProp,
        WrapExistingFlag iWrapFlag,
        const Argument &iArg0 = Argument(),
        const Argument &iArg1 = Argument() );

    //-*************************************************************************
    // SCALAR PROPERTY FEATURES
    //-*************************************************************************

    //! Set a sample using a reference to a value-type,
    //! instead of a void*
    void set( const value_type &iVal )
    {
        OScalarProperty::set( reinterpret_cast<const void *>( &iVal ) );
    }
};

//-*****************************************************************************
// TEMPLATE AND INLINE FUNCTIONS
//-*****************************************************************************

//-*****************************************************************************
template <class TRAITS>
template <class COMPOUND_PTR>
OTypedScalarProperty<TRAITS>::OTypedScalarProperty(
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

    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OTypedScalarProperty::init()" );

    AbcA::CompoundPropertyWriterPtr parent =
        GetCompoundPropertyWriterPtr( iParent );

    ABCA_ASSERT( parent, "NULL CompoundPropertyWriterPtr" );

    AbcA::MetaData mdata = args.getMetaData();
    if ( getInterpretation() != "" )
    {
        mdata.set( "interpretation", getInterpretation() );
    }

    AbcA::TimeSamplingPtr tsPtr = args.getTimeSampling();

    uint32_t tsIndex = args.getTimeSamplingIndex();

    // if we specified a valid TimeSamplingPtr, use it to determine the index
    // otherwise we'll use the index, which defaults to the intrinsic 0 index
    if (tsPtr)
    {
        tsIndex = parent->getObject()->getArchive()->addTimeSampling(*tsPtr);
    }

    m_property = parent->createScalarProperty( iName, mdata,
        TRAITS::dataType(), tsIndex );

    ALEMBIC_ABC_SAFE_CALL_END_RESET();
}

//-*****************************************************************************
template<class TRAITS>
inline OTypedScalarProperty<TRAITS>::OTypedScalarProperty(
    AbcA::ScalarPropertyWriterPtr iProperty,
    WrapExistingFlag iFlag,
    const Argument &iArg0,
    const Argument &iArg1 )
  : OScalarProperty( iProperty,
                     iFlag,
                     GetErrorHandlerPolicy( iProperty, iArg0, iArg1 ) )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN(
        "ITypedScalarProperty::ITypedScalarProperty()" );

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

typedef OTypedScalarProperty<BooleanTPTraits>         OBoolProperty;
typedef OTypedScalarProperty<Uint8TPTraits>           OUcharProperty;
typedef OTypedScalarProperty<Int8TPTraits>            OCharProperty;
typedef OTypedScalarProperty<Uint16TPTraits>          OUInt16Property;
typedef OTypedScalarProperty<Int16TPTraits>           OInt16Property;
typedef OTypedScalarProperty<Uint32TPTraits>          OUInt32Property;
typedef OTypedScalarProperty<Int32TPTraits>           OInt32Property;
typedef OTypedScalarProperty<Uint64TPTraits>          OUInt64Property;
typedef OTypedScalarProperty<Int64TPTraits>           OInt64Property;
typedef OTypedScalarProperty<Float16TPTraits>         OHalfProperty;
typedef OTypedScalarProperty<Float32TPTraits>         OFloatProperty;
typedef OTypedScalarProperty<Float64TPTraits>         ODoubleProperty;
typedef OTypedScalarProperty<StringTPTraits>          OStringProperty;
typedef OTypedScalarProperty<WstringTPTraits>         OWstringProperty;

typedef OTypedScalarProperty<V2sTPTraits>             OV2sProperty;
typedef OTypedScalarProperty<V2iTPTraits>             OV2iProperty;
typedef OTypedScalarProperty<V2fTPTraits>             OV2fProperty;
typedef OTypedScalarProperty<V2dTPTraits>             OV2dProperty;

typedef OTypedScalarProperty<V3sTPTraits>             OV3sProperty;
typedef OTypedScalarProperty<V3iTPTraits>             OV3iProperty;
typedef OTypedScalarProperty<V3fTPTraits>             OV3fProperty;
typedef OTypedScalarProperty<V3dTPTraits>             OV3dProperty;

typedef OTypedScalarProperty<P2sTPTraits>             OP2sProperty;
typedef OTypedScalarProperty<P2iTPTraits>             OP2iProperty;
typedef OTypedScalarProperty<P2fTPTraits>             OP2fProperty;
typedef OTypedScalarProperty<P2dTPTraits>             OP2dProperty;

typedef OTypedScalarProperty<P3sTPTraits>             OP3sProperty;
typedef OTypedScalarProperty<P3iTPTraits>             OP3iProperty;
typedef OTypedScalarProperty<P3fTPTraits>             OP3fProperty;
typedef OTypedScalarProperty<P3dTPTraits>             OP3dProperty;

typedef OTypedScalarProperty<Box2sTPTraits>           OBox2sProperty;
typedef OTypedScalarProperty<Box2iTPTraits>           OBox2iProperty;
typedef OTypedScalarProperty<Box2fTPTraits>           OBox2fProperty;
typedef OTypedScalarProperty<Box2dTPTraits>           OBox2dProperty;

typedef OTypedScalarProperty<Box3sTPTraits>           OBox3sProperty;
typedef OTypedScalarProperty<Box3iTPTraits>           OBox3iProperty;
typedef OTypedScalarProperty<Box3fTPTraits>           OBox3fProperty;
typedef OTypedScalarProperty<Box3dTPTraits>           OBox3dProperty;

typedef OTypedScalarProperty<M33fTPTraits>            OM33fProperty;
typedef OTypedScalarProperty<M33dTPTraits>            OM33dProperty;
typedef OTypedScalarProperty<M44fTPTraits>            OM44fProperty;
typedef OTypedScalarProperty<M44dTPTraits>            OM44dProperty;

typedef OTypedScalarProperty<QuatfTPTraits>           OQuatfProperty;
typedef OTypedScalarProperty<QuatdTPTraits>           OQuatdProperty;

typedef OTypedScalarProperty<C3hTPTraits>             OC3hProperty;
typedef OTypedScalarProperty<C3fTPTraits>             OC3fProperty;
typedef OTypedScalarProperty<C3cTPTraits>             OC3cProperty;

typedef OTypedScalarProperty<C4hTPTraits>             OC4hProperty;
typedef OTypedScalarProperty<C4fTPTraits>             OC4fProperty;
typedef OTypedScalarProperty<C4cTPTraits>             OC4cProperty;

typedef OTypedScalarProperty<N2fTPTraits>             ON2fProperty;
typedef OTypedScalarProperty<N2dTPTraits>             ON2dProperty;

typedef OTypedScalarProperty<N3fTPTraits>             ON3fProperty;
typedef OTypedScalarProperty<N3dTPTraits>             ON3dProperty;

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace Abc
} // End namespace Alembic

#endif
