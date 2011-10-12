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

#ifndef _Alembic_AbcGeom_IGeometrySchema_h_
#define _Alembic_AbcGeom_IGeometrySchema_h_

#include <Alembic/Abc/ISchema.h>
#include <Alembic/AbcGeom/Foundation.h>
#include <Alembic/AbcGeom/IGeomParam.h>
#include <Alembic/AbcGeom/SchemaInfoDeclarations.h>

namespace Alembic {
namespace AbcGeom {
namespace ALEMBIC_VERSION_NS {


//! This class holds properties common to all geometric classes that have a
//! physical volume.
//! - selfBounds
//! - childBounds (optional)
//! - GeomParams (optional)
//! - UserProperties (optional)
//!
//! This class is used to encapsulate common functionality of the
//! real Geometry schema classes, like IPoints and IPolyMesh and so on
template <class INFO>
class IGeomBaseSchema : public Abc::ISchema<INFO>
{
public:
    //-*************************************************************************
    // TYPEDEFS AND IDENTIFIERS
    //-*************************************************************************

    typedef INFO info_type;
    typedef ISchema<INFO> this_type;


    //-*************************************************************************
    // Constructors that pass through to ISchema
    //-*************************************************************************
    //
    //! The default constructor creates an empty ISchema.
    //! Used to create "NULL/invalid" instances.
    IGeomBaseSchema() {}

    //! Delegates to Abc/ISchema, and then creates
    //! properties that are present.
    template <class CPROP_PTR>
    IGeomBaseSchema( CPROP_PTR iParentCompound,
             const std::string &iName,

             const Argument &iArg0 = Argument(),
             const Argument &iArg1 = Argument() )
       : ISchema<info_type>( iParentCompound, iName,
                              iArg0, iArg1 )
    {
        init( iArg0, iArg1 );
    }

    template <class CPROP_PTR>
    explicit IGeomBaseSchema( CPROP_PTR iParentCompound,

                      const Argument &iArg0 = Argument(),
                      const Argument &iArg1 = Argument() )
      : ISchema<info_type>( iParentCompound,
                            iArg0, iArg1 )
    {
        init( iArg0, iArg1 );
    }

    //! Wrap an existing schema object
    template <class CPROP_PTR>
    IGeomBaseSchema( CPROP_PTR iThis,
                   Abc::WrapExistingFlag iFlag,
                   const Abc::Argument &iArg0 = Abc::Argument(),
                   const Abc::Argument &iArg1 = Abc::Argument() )
      : Abc::ISchema<info_type>( iThis, iFlag, iArg0, iArg1 )
    {
        init( iArg0, iArg1 );
    }

    void init( const Abc::Argument &iArg0, const Abc::Argument &iArg1 )
    {
        ALEMBIC_ABC_SAFE_CALL_BEGIN( "IGeomBaseSchema::init()" );

        Abc::Arguments args;
        iArg0.setInto( args );
        iArg1.setInto( args );

        AbcA::CompoundPropertyReaderPtr _this = this->getPtr();

        m_selfBoundsProperty = Abc::IBox3dProperty( _this, ".selfBnds",
            iArg0, iArg1 );
        if ( this->getPropertyHeader( ".childBnds" ) != NULL )
        {
            m_childBoundsProperty = Abc::IBox3dProperty( _this,
                ".childBnds", iArg0, iArg1 );
        }

        if ( this->getPropertyHeader( ".arbGeomParams" ) != NULL )
        {
            m_arbGeomParams = Abc::ICompoundProperty( _this, ".arbGeomParams",
                args.getErrorHandlerPolicy() );
        }
        if ( this->getPropertyHeader( ".userProperties" ) != NULL )
        {
            m_userProperties = Abc::ICompoundProperty( _this, ".userProperties",
                args.getErrorHandlerPolicy() );
        }

        ALEMBIC_ABC_SAFE_CALL_END();
    }

    virtual void reset ()
    {
        m_selfBoundsProperty.reset();
        m_childBoundsProperty.reset();
        m_arbGeomParams.reset();
        m_userProperties.reset();
        Abc::ISchema<info_type>::reset();
    }

    virtual bool valid() const
    {
        // Only selfBounds is required, all others are optional
        return ( Abc::ISchema<info_type>::valid() &&
                m_selfBoundsProperty.valid() );
    }

    Abc::IBox3dProperty getSelfBoundsProperty()
    {
        return m_selfBoundsProperty;
    }

    Abc::IBox3dProperty getChildBoundsProperty()
    {
        return m_childBoundsProperty;
    }

    // compound property to use as parent for any arbitrary GeomParams
    // underneath it
    ICompoundProperty getArbGeomParams() { return m_arbGeomParams; }

    // compound property to use as parent for any user workflow specific
    // properties
    ICompoundProperty getUserProperties() { return m_userProperties; }

protected:
    // Only selfBounds is required, all others are optional
    Abc::IBox3dProperty m_selfBoundsProperty;
    Abc::IBox3dProperty m_childBoundsProperty;

    Abc::ICompoundProperty m_arbGeomParams;
    Abc::ICompoundProperty m_userProperties;

};

//-*****************************************************************************
//! IGeomBase - A generic base set of properties and methods that encapsulate
//! things common to AbcGeom types that have a physical volume.
//! - self bounds
//! - children bounds (optional)
//! - argbGeomParams (optional)
//! - userProperties (optional)
//! This class is a concrete instantiation of IGeomBaseSchema.
//! Your archive might contain PolyMesh and SubD and Curves
//! and Points objects etc. This class, IGeomBase, gives you
//! access to the generic parts of those objects. For example, if you
//! just wish to iterate through an archive's hierarchy to examine bounding
//! regions this class could be helpful to you. Then when you actually
//! need to access the real data in the geometric type you can
//! always create the needed type of I<geom type> object> via kWrapExisting.
class IGeomBase : public IGeomBaseSchema<GeomBaseSchemaInfo>
{
public:
    typedef IGeomBase this_type;

    class Sample
    {
    public:
        typedef Sample this_type;

        // Users don't ever create this data directly.
        Sample() { reset(); }

        Abc::Box3d getSelfBounds() const { return m_selfBounds; }
        Abc::Box3d getChildBounds() const { return m_childBounds; }

        void reset()
        {
            m_selfBounds.makeEmpty();
            m_childBounds.makeEmpty();
        }

    protected:
        friend class IGeomBase;
        Abc::Box3d m_selfBounds;
        Abc::Box3d m_childBounds;
    };

public:
    //! By convention we always define this_type in AbcGeom classes.
    //! Used by unspecified-bool-type conversion below

    //-*************************************************************************
    // CONSTRUCTION, DESTRUCTION, ASSIGNMENT
    //-*************************************************************************

    //! The default constructor creates an empty GeomBase
    IGeomBase() {}

    template <class CPROP_PTR>
    IGeomBase( CPROP_PTR iParent,
               const std::string &iName,
               const Abc::Argument &iArg0 = Abc::Argument(),
               const Abc::Argument &iArg1 = Abc::Argument() )

        // We don't want strict matching of the title because the real schema
        // is going to be something like "AbcGeom_<type>_vX"
      : IGeomBaseSchema<GeomBaseSchemaInfo>( iParent, iName, kNoMatching )
    {
        init( iArg0, iArg1 );
    }

    template <class CPROP_PTR>
    explicit IGeomBase( CPROP_PTR iThis,
                        const Abc::Argument &iArg0 = Abc::Argument(),
                        const Abc::Argument &iArg1 = Abc::Argument() )
        // We don't want strict matching of the title because the real schema
        // is going to be something like "AbcGeom_<type>_vX"
      : IGeomBaseSchema<GeomBaseSchemaInfo>( iThis, kNoMatching )
    {
        init( iArg0, iArg1 );
    }

    template <class CPROP_PTR>
    explicit IGeomBase( CPROP_PTR iThis,
                        Abc::WrapExistingFlag iFlag,
                        const Abc::Argument &iArg0 = Abc::Argument(),
                        const Abc::Argument &iArg1 = Abc::Argument() )
        // We don't want strict matching of the title because the real schema
        // is going to be something like "AbcGeom_<type>_vX"
      : IGeomBaseSchema<GeomBaseSchemaInfo>( iThis, iFlag, kNoMatching )
    {
        init( iArg0, iArg1 );
    }

    //! Copy constructor.
    IGeomBase(const IGeomBase & iCopy)
        : IGeomBaseSchema<GeomBaseSchemaInfo>()
    {
        *this = iCopy;
    }

    //! Default assignment operator used.

    //-*************************************************************************
    // SCALAR PROPERTY READER FUNCTIONALITY
    //-*************************************************************************

    //! Return the number of samples contained in the property.
    //! This can be any number, including zero.
    //! This returns the number of samples that were written, independently
    //! of whether or not they were constant.
    size_t getNumSamples() const
    { return m_selfBoundsProperty.getNumSamples(); }

    //! Ask if we're constant - no change in value amongst samples,
    //! regardless of the time sampling.
    bool isConstant() const
    { return m_selfBoundsProperty.isConstant(); }

    //! Time sampling Information.
    //!
    AbcA::TimeSamplingPtr getTimeSampling()
    {
        if ( m_selfBoundsProperty.valid() )
        {
            return m_selfBoundsProperty.getTimeSampling();
        }
        else
        {
            return getObject().getArchive().getTimeSampling( 0 );
        }
    }

    //-*************************************************************************
    void get( Sample &oSample,
              const Abc::ISampleSelector &iSS = Abc::ISampleSelector() )
    {
        ALEMBIC_ABC_SAFE_CALL_BEGIN( "IGeomBase::get()" );

        m_selfBoundsProperty.get( oSample.m_selfBounds, iSS );

        if ( m_childBoundsProperty &&
             m_childBoundsProperty.getNumSamples() > 0 )
        {
            m_childBoundsProperty.get( oSample.m_childBounds, iSS );
        }

        ALEMBIC_ABC_SAFE_CALL_END();
    }

    //-*************************************************************************
    Sample getValue( const Abc::ISampleSelector &iSS = Abc::ISampleSelector() )
    {
        Sample smp;
        get( smp, iSS );
        return smp;
    }

    //-*************************************************************************
    Abc::ICompoundProperty getArbGeomParams() const { return m_arbGeomParams; }

    //-*************************************************************************
    Abc::ICompoundProperty getUserProperties() const { return m_userProperties; }

    //-*************************************************************************
    //! Reset returns this function set to an empty, default
    //! state.
    void reset()
    {
        IGeomBaseSchema<GeomBaseSchemaInfo>::reset();
    }

    //-*************************************************************************
    //! Valid returns whether this function set is
    //! valid.
    bool valid() const
    {
        return ( IGeomBaseSchema<GeomBaseSchemaInfo>::valid() );
    }

    //-*************************************************************************
    //! unspecified-bool-type operator overload.
    //! ...
    ALEMBIC_OVERRIDE_OPERATOR_BOOL( IGeomBase::valid() );

    //-*************************************************************************
    //! This will check whether or not a given entity (as represented by
    //! metadata) strictly matches the interpretation of this
    //! schema object.
    static bool matches( const AbcA::MetaData &iMetaData,
                         SchemaInterpMatching iMatching = kStrictMatching )
    {
        if ( iMatching == kNoMatching )
        { return true; }

        if ( iMatching == kStrictMatching || iMatching == kSchemaTitleMatching )
        {
            return iMetaData.get( "schemaBaseType" ) ==
                GeomBaseSchemaInfo::title();
        }

        return false;
    }

    //-*************************************************************************
    //! This will check whether or not a given object (as represented by
    //! an object header) strictly matches the interpretation of this
    //! schema object, as well as the data type.
    static bool matches( const AbcA::PropertyHeader &iHeader,
                         SchemaInterpMatching iMatching = kStrictMatching )
    {
        return matches( iHeader.getMetaData(), iMatching );
    }

};

//-*****************************************************************************
typedef Abc::ISchemaObject<IGeomBase> IGeomBaseObject;

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcGeom
} // End namespace Alembic

#endif
