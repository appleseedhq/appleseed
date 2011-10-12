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

#ifndef _Alembic_Abc_OSchema_h_
#define _Alembic_Abc_OSchema_h_

#include <Alembic/Abc/Foundation.h>
#include <Alembic/Abc/OCompoundProperty.h>

namespace Alembic {
namespace Abc {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
//! With properties, specific flavors of properties are expressed via the
//! TypedScalarProperty and the TypedArrayProperty. Compound Properties
//! are more complex, and the specific flavors require a more complex
//! treatment - That's what Schemas are. The CompoundProperty equivalent
//! of a TypedArrayProperty or a TypedScalarProperty.
//!
//! A Schema is a collection of grouped properties which implement some
//! complex object, such as a poly mesh. In the simpelest, standard case,
//! there will be a compound property at the top with a certain name, and
//! inside the compound property will be some number of additional properties
//! that implement the object. In the case of a poly mesh, these properties
//! would include a list of vertices (a V3fArray), a list of indices
//! (an Int32Array), and a list of "per-face counts" (also an Int32Array).
//!
//! In somewhat more complex cases, such as a TransformStack, the set of
//! properties that are added may vary based on configuration information
//! provided by the user.
//!
//! Because a Schema is to a CompoundProperty what a TypedArrayProperty
//! or TypedScalarProperty is to a regular property, it is directly derived
//! from CompoundProperty. However... Whereas TypedProperties can be instanced
//! as typedefs, Schemas will invariably require additional functionality,
//! and thus the StdCompoundSchema is intended for use as a base class.
//!
//-*****************************************************************************


//-*****************************************************************************
//! Here is a macro for declaring SCHEMA_INFO
//! It takes three arguments
//! - the SchemaTitle( a string ),
//! - the SchemaBaseType( a string ),
//! - the DefaultSchemaName( a string )
//! - the name of the SchemaTrait Type to be declared.
//! - for example:
//! ALEMBIC_ABC_DECLARE_SCHEMA_INFO( "AbcGeom_PolyMesh_v1",
//!                                  ".geom",
//!                                  PolyMeshSchemaInfo );
#define ALEMBIC_ABC_DECLARE_SCHEMA_INFO( STITLE, SBTYP, SDFLT, STDEF )  \
struct STDEF                                                            \
{                                                                       \
    static const char * title() { return ( STITLE ) ; }                 \
    static const char * defaultName() { return ( SDFLT ); }             \
    static const char * schemaBaseType() { return ( SBTYP ); }          \
}

//-*****************************************************************************
//! Usually used as a base class, but could also theoretically be used
//! as a standalone
template <class INFO>
class OSchema : public OCompoundProperty
{
public:
    //-*************************************************************************
    // TYPEDEFS AND IDENTIFIERS
    //-*************************************************************************

    typedef INFO info_type;
    typedef OSchema<INFO> this_type;

    //! Return the schema title expected of this
    //! property. An empty title matches everything
    static const std::string &getSchemaTitle()
    {
        static std::string sTitle = INFO::title();
        return sTitle;
    }

    //! Return the schema base type expected of this
    //! property. An empty base type means it's the root type.
    static const std::string &getSchemaBaseType()
    {
        static std::string sBaseType = INFO::schemaBaseType();
        return sBaseType;
    }

    //! Return the default name for instances of this schema. Often
    //! something like ".geom"
    static const std::string &getDefaultSchemaName()
    {
        static std::string sName = INFO::defaultName();
        return sName;
    }

    //! This will check whether or not a given entity (as represented by
    //! a metadata) strictly matches the interpretation of this
    //! schema object
    static bool matches( const AbcA::MetaData &iMetaData,
                         SchemaInterpMatching iMatching = kStrictMatching )
    {
        if ( getSchemaTitle() == "" || iMatching == kNoMatching )
        { return true; }

        if ( iMatching == kStrictMatching || iMatching == kSchemaTitleMatching )
        {
            return iMetaData.get( "schema" ) == getSchemaTitle();
        }

        return false;
    }

    //! This will check whether or not a given object (as represented by
    //! an object header) strictly matches the interpretation of this
    //! schema object, as well as the data type.
    static bool matches( const AbcA::PropertyHeader &iHeader,
                         SchemaInterpMatching iMatching = kStrictMatching )
    {
        return matches( iHeader.getMetaData(), iMatching );
    }

    //-*************************************************************************
    // CONSTRUCTION, DESTRUCTION, ASSIGNMENT
    //-*************************************************************************

    //! The default constructor creates an empty OSchema
    //! function set.
    OSchema() {}

    //! Creates a new Compound Property Writer with the schema
    //! information added to the metadata.
    template <class CPROP_PTR>
    OSchema( CPROP_PTR iParentObject,
             const std::string &iName,

             const Argument &iArg0 = Argument(),
             const Argument &iArg1 = Argument(),
             const Argument &iArg2 = Argument() )
    {
        this_type::init( iParentObject, iName, iArg0, iArg1, iArg2 );
    }

    //! Creates a new Compound Property Writer with the schema
    //! information and also the default name.
    template <class CPROP_PTR>
    explicit OSchema( CPROP_PTR iParentObject,

                      const Argument &iArg0 = Argument(),
                      const Argument &iArg1 = Argument(),
                      const Argument &iArg2 = Argument() )
    {
        this_type::init( iParentObject,
                         INFO::defaultName(),
                         iArg0, iArg1, iArg2 );
    }

    //! Wrap an existing compound property, checking that it matches
    //! the schema title info, if strict matching has been selected.
    //! Arguments allow selection of error handling and matching strictness
    template<class CPROP_PTR>
    OSchema( CPROP_PTR iProperty,
             WrapExistingFlag iFlag,
             const Argument &iArg0 = Argument(),
             const Argument &iArg1 = Argument(),
             const Argument &iArg2 = Argument() );

    virtual ~OSchema() {}

    //! Default copy constructor used
    //! Default assignment operator used.

private:
    template <class CPROP_PTR>
    void init( CPROP_PTR iParentObject,
               const std::string &iName,
               const Argument &iArg0,
               const Argument &iArg1,
               const Argument &iArg2 );
};

//-*****************************************************************************
// TEMPLATE AND INLINE FUNCTIONS
//-*****************************************************************************
template <class INFO>
template <class CPROP_PTR>
void OSchema<INFO>::init( CPROP_PTR iParent,
                            const std::string &iName,
                            const Argument &iArg0,
                            const Argument &iArg1,
                            const Argument &iArg2 )
{
    Arguments args;
    iArg0.setInto( args );
    iArg1.setInto( args );
    iArg2.setInto( args );

    getErrorHandler().setPolicy( args.getErrorHandlerPolicy() );

    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OSchema::OSchema::init()" );


    // Get actual writer for parent.
    ABCA_ASSERT( iParent,
                 "NULL parent passed into OSchema ctor" );
    AbcA::CompoundPropertyWriterPtr parent =
        GetCompoundPropertyWriterPtr( iParent );
    ABCA_ASSERT( parent, "NULL CompoundPropertyWriterPtr" );

    // Put schema title into metadata.
    AbcA::MetaData mdata = args.getMetaData();
    if ( getSchemaTitle() != "" )
    {
        mdata.set( "schema", getSchemaTitle() );
    }
    if ( getSchemaBaseType() != "" )
    {
        mdata.set( "schemaBaseType", getSchemaBaseType() );
    }

    // Create property.
    m_property = parent->createCompoundProperty( iName, mdata );

    ALEMBIC_ABC_SAFE_CALL_END_RESET();
}

//-*****************************************************************************
template<class INFO>
template<class COMPOUND_PTR>
inline OSchema<INFO>::OSchema(
    COMPOUND_PTR iProperty,
    WrapExistingFlag iFlag,
    const Argument &iArg0,
    const Argument &iArg1,
    const Argument &iArg2 )
  : OCompoundProperty( iProperty,
                       iFlag,
                       GetErrorHandlerPolicy( iProperty,
                                              iArg0, iArg1, iArg2 ) )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN(
        "OSchema::OSchema( wrap )" );

    const AbcA::PropertyHeader &pheader = this->getHeader();

    ABCA_ASSERT( matches( pheader,
                          GetSchemaInterpMatching( iArg0, iArg1, iArg2 ) ),

                 "Incorrect match of schema: "
                 << pheader.getMetaData().get( "schema" )
                 << " to expected: "
                 << INFO::title() );

    ALEMBIC_ABC_SAFE_CALL_END_RESET();
}


} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace Abc
} // End namespace Alembic

#endif
