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
#ifndef _Alembic_Abc_ITypedScalarProperty_h_
#define _Alembic_Abc_ITypedScalarProperty_h_

#include <Alembic/Abc/Foundation.h>
#include <Alembic/Abc/IScalarProperty.h>
#include <Alembic/Abc/TypedPropertyTraits.h>

namespace Alembic {
namespace Abc {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
template <class TRAITS>
class ITypedScalarProperty : public IScalarProperty
{
public:
    //-*************************************************************************
    // TYPEDEFS AND IDENTIFIERS
    //-*************************************************************************
    typedef TRAITS traits_type;
    typedef ITypedScalarProperty<TRAITS> this_type;
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
        if ( iMatching == kStrictMatching )
        {
            return ( getInterpretation() == "" ||
                     ( iMetaData.get( "interpretation" ) ==
                       getInterpretation() ) );
        }
        return true;
    }

    //! This will check whether or not a given object (as represented by
    //! an object header) strictly matches the interpretation of this
    //! schema object
    static bool matches( const AbcA::PropertyHeader &iHeader,
                         SchemaInterpMatching iMatching = kStrictMatching )
    {
        return ( iHeader.getDataType() == TRAITS::dataType() ) &&
            matches( iHeader.getMetaData(), iMatching );
    }

    //-*************************************************************************
    // CONSTRUCTION, DESTRUCTION, ASSIGNMENT
    //-*************************************************************************

    //! Default constructor
    //! ...
    ITypedScalarProperty() {}

    //! This templated, explicit function creates a new
    //! typed scalar property reader.
    //! The first argument is any Abc (or AbcCoreAbstract) object
    //! which can intrusively be converted to a CompoundPropertyReaderPtr
    //! to use as a parent, from which the error handler policy for
    //! inheritance is also derived.  The remaining optional arguments
    //! can be used to override the ErrorHandlerPolicy, to specify
    //! schema matching policy, and that's it.
    template <class COMPOUND_PTR>
    ITypedScalarProperty( COMPOUND_PTR iParent,
                          const std::string &iName,

                          const Argument &iArg0 = Argument(),
                          const Argument &iArg1 = Argument() );

    //! Explicitly wrap an existing property
    //! It will check the data type and also verify the schema,
    //! if requested.
    ITypedScalarProperty( AbcA::ScalarPropertyReaderPtr iProp,
                          WrapExistingFlag iWrapFlag,
                          const Argument &iArg0 = Argument(),
                          const Argument &iArg1 = Argument() );


    //-*************************************************************************
    // SCALAR PROPERTY FEATURES
    //-*************************************************************************

    //! Get the typed sample.
    //! ...
    void get( value_type &iVal,
              const ISampleSelector &iSS = ISampleSelector() ) const
    {
        IScalarProperty::get( reinterpret_cast<void*>( &iVal ), iSS );
    }

    //! Return the typed sample by value.
    //! ...
    value_type getValue( const ISampleSelector &iSS = ISampleSelector() ) const
    {
        value_type ret;
        get( ret, iSS );
        return ret;
    }
};

//-*****************************************************************************
// TEMPLATE AND INLINE FUNCTIONS
//-*****************************************************************************

//-*****************************************************************************
template <class TRAITS>
template <class COMPOUND_PTR>
ITypedScalarProperty<TRAITS>::ITypedScalarProperty
(
    COMPOUND_PTR iParent,
    const std::string &iName,
    const Argument &iArg0,
    const Argument &iArg1 )
{
    Arguments args( GetErrorHandlerPolicy( iParent ) );
    iArg0.setInto( args );
    iArg1.setInto( args );

    getErrorHandler().setPolicy( args.getErrorHandlerPolicy() );

    ALEMBIC_ABC_SAFE_CALL_BEGIN(
        "ITypedScalarProperty::ITypedScalarProperty()" );

    AbcA::CompoundPropertyReaderPtr parent =
        GetCompoundPropertyReaderPtr( iParent );
    ABCA_ASSERT( parent != NULL,
                 "NULL CompoundPropertyReader passed into "
                 << "ITypedScalarProperty ctor" );

    const AbcA::PropertyHeader *pheader =
        parent->getPropertyHeader( iName );
    ABCA_ASSERT( pheader != NULL,
                 "Nonexistent scalar property: " << iName );

    ABCA_ASSERT( matches( *pheader, args.getSchemaInterpMatching() ),

                 "Incorrect match of header datatype: "
                 << pheader->getDataType()
                 << " to expected: "
                 << TRAITS::dataType()
                 << ",\n...or incorrect match of interpretation: "
                 << pheader->getMetaData().get( "interpretation" )
                 << " to expected: "
                 << TRAITS::interpretation() );

    m_property = parent->getScalarProperty( iName );

    ALEMBIC_ABC_SAFE_CALL_END_RESET();
}

//-*****************************************************************************
template <class TRAITS>
ITypedScalarProperty<TRAITS>::ITypedScalarProperty(
    AbcA::ScalarPropertyReaderPtr iProperty,
    WrapExistingFlag iFlag,
    const Argument &iArg0,
    const Argument &iArg1 )
  : IScalarProperty( iProperty,
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

typedef ITypedScalarProperty<BooleanTPTraits>         IBoolProperty;
typedef ITypedScalarProperty<Uint8TPTraits>           IUcharProperty;
typedef ITypedScalarProperty<Int8TPTraits>            ICharProperty;
typedef ITypedScalarProperty<Uint16TPTraits>          IUInt16Property;
typedef ITypedScalarProperty<Int16TPTraits>           IInt16Property;
typedef ITypedScalarProperty<Uint32TPTraits>          IUInt32Property;
typedef ITypedScalarProperty<Int32TPTraits>           IInt32Property;
typedef ITypedScalarProperty<Uint64TPTraits>          IUInt64Property;
typedef ITypedScalarProperty<Int64TPTraits>           IInt64Property;
typedef ITypedScalarProperty<Float16TPTraits>         IHalfProperty;
typedef ITypedScalarProperty<Float32TPTraits>         IFloatProperty;
typedef ITypedScalarProperty<Float64TPTraits>         IDoubleProperty;
typedef ITypedScalarProperty<StringTPTraits>          IStringProperty;
typedef ITypedScalarProperty<WstringTPTraits>         IWstringProperty;

typedef ITypedScalarProperty<V2sTPTraits>             IV2sProperty;
typedef ITypedScalarProperty<V2iTPTraits>             IV2iProperty;
typedef ITypedScalarProperty<V2fTPTraits>             IV2fProperty;
typedef ITypedScalarProperty<V2dTPTraits>             IV2dProperty;

typedef ITypedScalarProperty<V3sTPTraits>             IV3sProperty;
typedef ITypedScalarProperty<V3iTPTraits>             IV3iProperty;
typedef ITypedScalarProperty<V3fTPTraits>             IV3fProperty;
typedef ITypedScalarProperty<V3dTPTraits>             IV3dProperty;

typedef ITypedScalarProperty<P2sTPTraits>             IP2sProperty;
typedef ITypedScalarProperty<P2iTPTraits>             IP2iProperty;
typedef ITypedScalarProperty<P2fTPTraits>             IP2fProperty;
typedef ITypedScalarProperty<P2dTPTraits>             IP2dProperty;

typedef ITypedScalarProperty<P3sTPTraits>             IP3sProperty;
typedef ITypedScalarProperty<P3iTPTraits>             IP3iProperty;
typedef ITypedScalarProperty<P3fTPTraits>             IP3fProperty;
typedef ITypedScalarProperty<P3dTPTraits>             IP3dProperty;

typedef ITypedScalarProperty<Box2sTPTraits>           IBox2sProperty;
typedef ITypedScalarProperty<Box2iTPTraits>           IBox2iProperty;
typedef ITypedScalarProperty<Box2fTPTraits>           IBox2fProperty;
typedef ITypedScalarProperty<Box2dTPTraits>           IBox2dProperty;

typedef ITypedScalarProperty<Box3sTPTraits>           IBox3sProperty;
typedef ITypedScalarProperty<Box3iTPTraits>           IBox3iProperty;
typedef ITypedScalarProperty<Box3fTPTraits>           IBox3fProperty;
typedef ITypedScalarProperty<Box3dTPTraits>           IBox3dProperty;

typedef ITypedScalarProperty<M33fTPTraits>            IM33fProperty;
typedef ITypedScalarProperty<M33dTPTraits>            IM33dProperty;
typedef ITypedScalarProperty<M44fTPTraits>            IM44fProperty;
typedef ITypedScalarProperty<M44dTPTraits>            IM44dProperty;

typedef ITypedScalarProperty<QuatfTPTraits>           IQuatfProperty;
typedef ITypedScalarProperty<QuatdTPTraits>           IQuatdProperty;

typedef ITypedScalarProperty<C3hTPTraits>             IC3hProperty;
typedef ITypedScalarProperty<C3fTPTraits>             IC3fProperty;
typedef ITypedScalarProperty<C3cTPTraits>             IC3cProperty;

typedef ITypedScalarProperty<C4hTPTraits>             IC4hProperty;
typedef ITypedScalarProperty<C4fTPTraits>             IC4fProperty;
typedef ITypedScalarProperty<C4cTPTraits>             IC4cProperty;

typedef ITypedScalarProperty<N2fTPTraits>             IN2fProperty;
typedef ITypedScalarProperty<N2dTPTraits>             IN2dProperty;

typedef ITypedScalarProperty<N3fTPTraits>             IN3fProperty;
typedef ITypedScalarProperty<N3dTPTraits>             IN3dProperty;

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace Abc
} // End namespace Alembic

#endif
