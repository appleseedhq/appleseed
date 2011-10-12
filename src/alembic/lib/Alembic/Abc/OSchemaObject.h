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

#ifndef _Alembic_Abc_OSchemaObject_h_
#define _Alembic_Abc_OSchemaObject_h_

#include <Alembic/Abc/Foundation.h>
#include <Alembic/Abc/OObject.h>
#include <Alembic/Abc/OSchema.h>

namespace Alembic {
namespace Abc {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
//! An OSchemaObject is an object with a single schema. This is just
//! a convenience class, really, but it also deals with setting up and
//! validating metadata
template <class SCHEMA>
class OSchemaObject : public OObject
{
public:
    //-*************************************************************************
    // TYPEDEFS AND IDENTIFIERS
    //-*************************************************************************
    typedef SCHEMA schema_type;
    typedef OSchemaObject<SCHEMA> this_type;

    //! Our schema title contains the schema title of the underlying
    //! compound property, along with the default name of that compound
    //! property. So, for example - most AbcGeom types put their
    //! data in ".geom", so, "AbcGeom_PolyMesh_v1:.geom"
    //! Sometimes schema titles from underlying schemas are "", but
    //! ours never are.
    static const std::string &getSchemaObjTitle()
    {
        static std::string soSchemaTitle =
            SCHEMA::getSchemaTitle() + ":" + SCHEMA::getDefaultSchemaName();
        return soSchemaTitle;
    }

    static const std::string &getSchemaTitle()
    {
        static std::string sSchemaTitle = SCHEMA::getSchemaTitle();
        return sSchemaTitle;
    }

    //! This will check whether or not a given entity (as represented by
    //! a metadata) strictly matches the interpretation of this
    //! schema object
    static bool matches( const AbcA::MetaData &iMetaData,
                         SchemaInterpMatching iMatching = kStrictMatching )
    {
        if ( getSchemaTitle() == "" || iMatching == kNoMatching )
        { return true; }

        if ( iMatching == kStrictMatching )
        {

            return iMetaData.get( "schemaObjTitle" ) == getSchemaObjTitle();
        }

        if ( iMatching == kSchemaTitleMatching )
        {
            return iMetaData.get( "schema" ) == getSchemaTitle();
        }

        return false;
    }

    //! This will check whether or not a given object (as represented by
    //! an object header) strictly matches the interpretation of this
    //! schema object, as well as the data type.
    static bool matches( const AbcA::ObjectHeader &iHeader,
                         SchemaInterpMatching iMatching = kStrictMatching )
    {
        return matches( iHeader.getMetaData(), iMatching );
    }


    //-*************************************************************************
    // CONSTRUCTION, DESTRUCTION, ASSIGNMENT
    //-*************************************************************************

    //! The default constructor creates an empty OSchemaObject function set.
    //! ...
    OSchemaObject() {}

    //! The primary constructor creates an OSchemaObject as a child of the
    //! first argument, which is any Abc or AbcCoreAbstract (or other)
    //! object which can be intrusively cast to an ObjectWriterPtr.
    template <class OBJECT_PTR>
    OSchemaObject( OBJECT_PTR iParentObject,
                   const std::string &iName,

                   const Argument &iArg0 = Argument(),
                   const Argument &iArg1 = Argument(),
                   const Argument &iArg2 = Argument() );

    //! Wrap an existing schema object.
    //! ...
    template <class OBJECT_PTR>
    OSchemaObject( OBJECT_PTR iThisObject,
                   WrapExistingFlag iFlag,
                   const Argument &iArg0 = Argument(),
                   const Argument &iArg1 = Argument(),
                   const Argument &iArg2 = Argument() );

    //-*************************************************************************
    // ABC BASE MECHANISMS
    // These functions are used by Abc to deal with errors, rewrapping,
    // and so on.
    //-*************************************************************************

    //! Schemas are not necessarily cheap to copy, so we return by reference
    //! rather than by value.
    SCHEMA &getSchema() { return m_schema; }
    const SCHEMA &getSchema() const { return m_schema; }

    //! Reset returns this function set to an empty, default
    //! state.
    void reset() { m_schema.reset(); OObject::reset(); }

    //! Valid returns whether this function set is
    //! valid.
    bool valid() const
    {
        return ( OObject::valid() && m_schema.valid() );
    }

    //! The unspecified-bool-type operator casts the object to "true"
    //! if it is valid, and "false" otherwise.
    ALEMBIC_OVERRIDE_OPERATOR_BOOL( this_type::valid() );

protected:
    SCHEMA m_schema;
};

//-*****************************************************************************
// TEMPLATE AND INLINE FUNCTIONS
//-*****************************************************************************

//-*****************************************************************************
template <class SCHEMA>
template <class OBJECT_PTR>
OSchemaObject<SCHEMA>::OSchemaObject
(
    OBJECT_PTR iParentObject,
    const std::string &iName,
    const Argument &iArg0,
    const Argument &iArg1,
    const Argument &iArg2 )
{
    Arguments args( GetErrorHandlerPolicy( iParentObject ) );
    iArg0.setInto( args );
    iArg1.setInto( args );
    iArg2.setInto( args );

    getErrorHandler().setPolicy( args.getErrorHandlerPolicy() );

    ALEMBIC_ABC_SAFE_CALL_BEGIN(
        "OSchemaObject::OSchemaObject( OObject )" );

    // Extract the parent.
    AbcA::ObjectWriterPtr parent = GetObjectWriterPtr( iParentObject );
    ABCA_ASSERT( parent,
                 "NULL Parent ObjectWriter in OSchemaObject ctor" );

    // The object schema title is derived from the schema's title.
    // It is never empty.
    AbcA::MetaData metaData = args.getMetaData();
    metaData.set( "schema", SCHEMA::getSchemaTitle() );
    metaData.set( "schemaObjTitle", getSchemaObjTitle() );
    if (SCHEMA::getSchemaBaseType() != "" )
    {
        metaData.set( "schemaBaseType", SCHEMA::getSchemaBaseType() );
    }

    // Make the object.
    AbcA::ObjectHeader ohdr( iName, metaData );
    m_object = parent->createChild( ohdr );

    AbcA::TimeSamplingPtr tsPtr = args.getTimeSampling();
    uint32_t tsIndex = args.getTimeSamplingIndex();

    // if we specified a valid TimeSamplingPtr, use it to determine the index
    // otherwise we'll use the index, which defaults to the intrinsic 0 index
    if (tsPtr)
    {
        tsIndex = parent->getArchive()->addTimeSampling(*tsPtr);
    }

    // Make the schema.
    m_schema = SCHEMA( m_object->getProperties(),
                       this->getErrorHandlerPolicy(),
                       tsIndex );

    ALEMBIC_ABC_SAFE_CALL_END_RESET();
}

//-*****************************************************************************
template<class SCHEMA>
template<class OBJECT_PTR>
inline OSchemaObject<SCHEMA>::OSchemaObject(
    OBJECT_PTR iObject,
    WrapExistingFlag iFlag,
    const Argument &iArg0,
    const Argument &iArg1,
    const Argument &iArg2 )
  : OObject( iObject,
             iFlag,
             GetErrorHandlerPolicy( iObject,
                                    iArg0, iArg1, iArg2 ) )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN(
        "OSchemaObject::OSchemaObject( wrap )" );

    const AbcA::ObjectHeader &oheader = this->getHeader();

    m_schema = SCHEMA(
        this->getProperties().getProperty(
            SCHEMA::getDefaultSchemaName() ).getPtr()->asCompoundPtr(),
        iFlag,
        this->getErrorHandlerPolicy(),
        GetSchemaInterpMatching( iArg0, iArg1, iArg2 ) );


    ABCA_ASSERT( matches( oheader,
                          GetSchemaInterpMatching( iArg0, iArg1, iArg2 ) ),

                 "Incorrect match of schema: "
                 << oheader.getMetaData().get( "schemaObjTitle" )
                 << " to expected: "
                 << getSchemaObjTitle() );

    ALEMBIC_ABC_SAFE_CALL_END_RESET();
}

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace Abc
} // End namespace Alembic

#endif
