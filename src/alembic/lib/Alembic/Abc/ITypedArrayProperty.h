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
#ifndef _Alembic_Abc_ITypedArrayProperty_h_
#define _Alembic_Abc_ITypedArrayProperty_h_

#include <Alembic/Abc/Foundation.h>
#include <Alembic/Abc/IArrayProperty.h>
#include <Alembic/Abc/TypedPropertyTraits.h>
#include <Alembic/Abc/TypedArraySample.h>

namespace Alembic {
namespace Abc {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
template <class TRAITS>
class ITypedArrayProperty : public IArrayProperty
{
public:
    //-*************************************************************************
    // TYPEDEFS AND IDENTIFIERS
    //-*************************************************************************
    typedef TRAITS traits_type;
    typedef ITypedArrayProperty<TRAITS> this_type;
    typedef typename TRAITS::value_type value_type;
    typedef TypedArraySample<TRAITS> sample_type;
    typedef boost::shared_ptr<sample_type> sample_ptr_type;

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
    ITypedArrayProperty() {}

    //! This templated, explicit function creates a new
    //! typed array property reader.
    //! The first argument is any Abc (or AbcCoreAbstract) object
    //! which can intrusively be converted to a CompoundPropertyReaderPtr
    //! to use as a parent, from which the error handler policy for
    //! inheritance is also derived.  The remaining optional arguments
    //! can be used to override the ErrorHandlerPolicy, to specify
    //! schema matching policy, and that's it.
    template <class COMPOUND_PTR>
    ITypedArrayProperty( COMPOUND_PTR iParent,
                         const std::string &iName,

                         const Argument &iArg0 = Argument(),
                         const Argument &iArg1 = Argument() );

    //! Explicitly wrap an existing property
    //! It will check the data type and also verify the schema,
    //! if requested.
    ITypedArrayProperty( AbcA::ArrayPropertyReaderPtr iProp,
                         WrapExistingFlag iWrapFlag,
                         const Argument &iArg0 = Argument(),
                         const Argument &iArg1 = Argument() );


    //-*************************************************************************
    // ARRAY PROPERTY FEATURES
    //-*************************************************************************

    //! Get the typed sample.
    //! ...
    void get( sample_ptr_type& iVal,
              const ISampleSelector &iSS = ISampleSelector() ) const
    {
        AbcA::ArraySamplePtr ptr;
        IArrayProperty::get( ptr, iSS );
        iVal = boost::static_pointer_cast<sample_type, AbcA::ArraySample>( ptr );
    }

    //! Return the typed sample by value.
    //! ...
    sample_ptr_type getValue( const ISampleSelector &iSS = ISampleSelector() ) const
    {
        sample_ptr_type ret;
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
ITypedArrayProperty<TRAITS>::ITypedArrayProperty
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
        "ITypedArrayProperty::ITypedArrayProperty()" );

    AbcA::CompoundPropertyReaderPtr parent =
        GetCompoundPropertyReaderPtr( iParent );
    ABCA_ASSERT( parent != NULL,
                 "NULL CompoundPropertyReader passed into "
                 << "ITypedArrayProperty ctor" );

    const AbcA::PropertyHeader *pheader =
        parent->getPropertyHeader( iName );
    ABCA_ASSERT( pheader != NULL,
                 "Nonexistent array property: " << iName );

    ABCA_ASSERT( matches( *pheader, args.getSchemaInterpMatching() ),

                 "Incorrect match of header datatype: "
                 << pheader->getDataType()
                 << " to expected: "
                 << TRAITS::dataType()
                 << ",\n...or incorrect match of interpretation: "
                 << pheader->getMetaData().get( "interpretation" )
                 << " to expected: "
                 << TRAITS::interpretation() );

    m_property = parent->getArrayProperty( iName );

    ALEMBIC_ABC_SAFE_CALL_END_RESET();
}

//-*****************************************************************************
template <class TRAITS>
ITypedArrayProperty<TRAITS>::ITypedArrayProperty(
    AbcA::ArrayPropertyReaderPtr iProperty,
    WrapExistingFlag iFlag,
    const Argument &iArg0,
    const Argument &iArg1 )
  : IArrayProperty( iProperty,
                    iFlag,
                    GetErrorHandlerPolicy( iProperty, iArg0, iArg1 ) )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN(
        "ITypedArrayProperty::ITypedArrayProperty()" );

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

typedef ITypedArrayProperty<BooleanTPTraits>         IBoolArrayProperty;
typedef ITypedArrayProperty<Uint8TPTraits>           IUcharArrayProperty;
typedef ITypedArrayProperty<Int8TPTraits>            ICharArrayProperty;
typedef ITypedArrayProperty<Uint16TPTraits>          IUInt16ArrayProperty;
typedef ITypedArrayProperty<Int16TPTraits>           IInt16ArrayProperty;
typedef ITypedArrayProperty<Uint32TPTraits>          IUInt32ArrayProperty;
typedef ITypedArrayProperty<Int32TPTraits>           IInt32ArrayProperty;
typedef ITypedArrayProperty<Uint64TPTraits>          IUInt64ArrayProperty;
typedef ITypedArrayProperty<Int64TPTraits>           IInt64ArrayProperty;
typedef ITypedArrayProperty<Float16TPTraits>         IHalfArrayProperty;
typedef ITypedArrayProperty<Float32TPTraits>         IFloatArrayProperty;
typedef ITypedArrayProperty<Float64TPTraits>         IDoubleArrayProperty;
typedef ITypedArrayProperty<StringTPTraits>          IStringArrayProperty;
typedef ITypedArrayProperty<WstringTPTraits>         IWstringArrayProperty;

typedef ITypedArrayProperty<V2sTPTraits>             IV2sArrayProperty;
typedef ITypedArrayProperty<V2iTPTraits>             IV2iArrayProperty;
typedef ITypedArrayProperty<V2fTPTraits>             IV2fArrayProperty;
typedef ITypedArrayProperty<V2dTPTraits>             IV2dArrayProperty;

typedef ITypedArrayProperty<V3sTPTraits>             IV3sArrayProperty;
typedef ITypedArrayProperty<V3iTPTraits>             IV3iArrayProperty;
typedef ITypedArrayProperty<V3fTPTraits>             IV3fArrayProperty;
typedef ITypedArrayProperty<V3dTPTraits>             IV3dArrayProperty;

typedef ITypedArrayProperty<P2sTPTraits>             IP2sArrayProperty;
typedef ITypedArrayProperty<P2iTPTraits>             IP2iArrayProperty;
typedef ITypedArrayProperty<P2fTPTraits>             IP2fArrayProperty;
typedef ITypedArrayProperty<P2dTPTraits>             IP2dArrayProperty;

typedef ITypedArrayProperty<P3sTPTraits>             IP3sArrayProperty;
typedef ITypedArrayProperty<P3iTPTraits>             IP3iArrayProperty;
typedef ITypedArrayProperty<P3fTPTraits>             IP3fArrayProperty;
typedef ITypedArrayProperty<P3dTPTraits>             IP3dArrayProperty;

typedef ITypedArrayProperty<Box2sTPTraits>           IBox2sArrayProperty;
typedef ITypedArrayProperty<Box2iTPTraits>           IBox2iArrayProperty;
typedef ITypedArrayProperty<Box2fTPTraits>           IBox2fArrayProperty;
typedef ITypedArrayProperty<Box2dTPTraits>           IBox2dArrayProperty;

typedef ITypedArrayProperty<Box3sTPTraits>           IBox3sArrayProperty;
typedef ITypedArrayProperty<Box3iTPTraits>           IBox3iArrayProperty;
typedef ITypedArrayProperty<Box3fTPTraits>           IBox3fArrayProperty;
typedef ITypedArrayProperty<Box3dTPTraits>           IBox3dArrayProperty;

typedef ITypedArrayProperty<M33fTPTraits>            IM33fArrayProperty;
typedef ITypedArrayProperty<M33dTPTraits>            IM33dArrayProperty;
typedef ITypedArrayProperty<M44fTPTraits>            IM44fArrayProperty;
typedef ITypedArrayProperty<M44dTPTraits>            IM44dArrayProperty;

typedef ITypedArrayProperty<QuatfTPTraits>           IQuatfArrayProperty;
typedef ITypedArrayProperty<QuatdTPTraits>           IQuatdArrayProperty;

typedef ITypedArrayProperty<C3hTPTraits>             IC3hArrayProperty;
typedef ITypedArrayProperty<C3fTPTraits>             IC3fArrayProperty;
typedef ITypedArrayProperty<C3cTPTraits>             IC3cArrayProperty;

typedef ITypedArrayProperty<C4hTPTraits>             IC4hArrayProperty;
typedef ITypedArrayProperty<C4fTPTraits>             IC4fArrayProperty;
typedef ITypedArrayProperty<C4cTPTraits>             IC4cArrayProperty;

typedef ITypedArrayProperty<N2fTPTraits>             IN2fArrayProperty;
typedef ITypedArrayProperty<N2dTPTraits>             IN2dArrayProperty;

typedef ITypedArrayProperty<N3fTPTraits>             IN3fArrayProperty;
typedef ITypedArrayProperty<N3dTPTraits>             IN3dArrayProperty;


} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace Abc
} // End namespace Alembic

#endif
