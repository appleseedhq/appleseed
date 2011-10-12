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

#ifndef _Alembic_Abc_ISchema_h_
#define _Alembic_Abc_ISchema_h_

#include <Alembic/Abc/Foundation.h>
#include <Alembic/Abc/OSchema.h>
#include <Alembic/Abc/ICompoundProperty.h>

namespace Alembic {
namespace Abc {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
//! See Schema Notes in OSchema.h
//! ...
//-*****************************************************************************

//-*****************************************************************************
//! Usually used as a base class, but could also theoretically be used
//! as a standalone
template <class INFO>
class ISchema : public ICompoundProperty
{
public:
    //-*************************************************************************
    // TYPEDEFS AND IDENTIFIERS
    //-*************************************************************************
    typedef INFO info_type;
    typedef ISchema<INFO> this_type;

    //! Return the schema title expected of this
    //! property. An empty title matches everything
    static const std::string &getSchemaTitle()
    {
        static std::string sTitle = INFO::title();
        return sTitle;
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

    //! The default constructor creates an empty ISchema
    //! function set.
    ISchema() {}

    //! Creates a new Compound Property Reader with the schema
    //! information added to the metadata.
    //! arguments count include error handling, strictness matching.
    template <class CPROP_PTR>
    ISchema( CPROP_PTR iParentObject,
             const std::string &iName,

             const Argument &iArg0 = Argument(),
             const Argument &iArg1 = Argument() )
    {
        this_type::init( iParentObject, iName, iArg0, iArg1 );
    }

    //! Creates a new Compound Property Reader with the schema
    //! information and also the default name.
    template <class CPROP_PTR>
    explicit ISchema( CPROP_PTR iParentObject,

                      const Argument &iArg0 = Argument(),
                      const Argument &iArg1 = Argument() )
    {
        this_type::init( iParentObject,
                         INFO::defaultName(),
                         iArg0, iArg1 );
    }

    //! Wrap an existing compound property, checking that it matches
    //! the schema title info, if strict matching has been selected.
    //! Arguments allow selection of error handling and matching strictness
    template<class CPROP_PTR>
    ISchema( CPROP_PTR iProperty,
             WrapExistingFlag iFlag,
             const Argument &iArg0 = Argument(),
             const Argument &iArg1 = Argument() );

    //! Default copy constructor used
    //! Default assignment operator used.
    //
    virtual ~ISchema() {}

private:
    template <class CPROP_PTR>
    void init( CPROP_PTR iParentObject,
               const std::string &iName,
               const Argument &iArg0,
               const Argument &iArg1 );
};

//-*****************************************************************************
// TEMPLATE AND INLINE FUNCTIONS
//-*****************************************************************************
template <class INFO>
template <class CPROP_PTR>
void ISchema<INFO>::init( CPROP_PTR iParent,
                            const std::string &iName,
                            const Argument &iArg0,
                            const Argument &iArg1 )
{
    Arguments args;
    iArg0.setInto( args );
    iArg1.setInto( args );

    getErrorHandler().setPolicy( args.getErrorHandlerPolicy() );

    ALEMBIC_ABC_SAFE_CALL_BEGIN( "ISchema::ISchema::init()" );

    // Get actual reader for parent.
    ABCA_ASSERT( iParent,
                 "NULL parent passed into ISchema ctor" );
    AbcA::CompoundPropertyReaderPtr parent =
        GetCompoundPropertyReaderPtr( iParent );
    ABCA_ASSERT( parent, "NULL CompoundPropertyReaderPtr" );

    const AbcA::PropertyHeader *pheader = parent->getPropertyHeader( iName );

    ABCA_ASSERT( pheader != NULL,
                 "Nonexistent compound property: " << iName );

    // Check metadata for schema.
    ABCA_ASSERT( matches( *pheader, args.getSchemaInterpMatching() ),

                 "Incorrect match of schema: "
                 << pheader->getMetaData().get( "schema" )
                 << " to expected: "
                 << INFO::title() );

    // Get property.
    m_property = parent->getCompoundProperty( iName );

    ALEMBIC_ABC_SAFE_CALL_END_RESET();
}

//-*****************************************************************************
template<class INFO>
template<class COMPOUND_PTR>
inline ISchema<INFO>::ISchema(
    COMPOUND_PTR iProperty,
    WrapExistingFlag iFlag,
    const Argument &iArg0,
    const Argument &iArg1 )
  : ICompoundProperty( iProperty,
                       iFlag,
                       GetErrorHandlerPolicy( iProperty,
                                              iArg0, iArg1 ) )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN(
        "ISchema::ISchema( wrap )" );

    const AbcA::PropertyHeader &pheader = this->getHeader();

    ABCA_ASSERT( matches( pheader,
                          GetSchemaInterpMatching( iArg0, iArg1 ) ),

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
