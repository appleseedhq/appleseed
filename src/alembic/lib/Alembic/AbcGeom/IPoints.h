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

#ifndef _Alembic_AbcGeom_IPoints_h_
#define _Alembic_AbcGeom_IPoints_h_

#include <Alembic/AbcGeom/Foundation.h>
#include <Alembic/AbcGeom/SchemaInfoDeclarations.h>
#include <Alembic/AbcGeom/IGeomParam.h>
#include <Alembic/AbcGeom/IGeomBase.h>

namespace Alembic {
namespace AbcGeom {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
class IPointsSchema : public IGeomBaseSchema<PointsSchemaInfo>
{
public:
    class Sample
    {
    public:
        typedef Sample this_type;

        // Users don't ever create this data directly.
        Sample() { reset(); }

        Abc::P3fArraySamplePtr getPositions() const { return m_positions; }
        Abc::UInt64ArraySamplePtr getIds() const { return m_ids; }
        Abc::V3fArraySamplePtr getVelocities() const { return m_velocities; }

        Abc::Box3d getSelfBounds() const { return m_selfBounds; }
        Abc::Box3d getChildBounds() const { return m_childBounds; }

        bool valid() const
        {
            return m_positions && m_ids;
        }

        void reset()
        {
            m_positions.reset();
            m_velocities.reset();
            m_ids.reset();

            m_selfBounds.makeEmpty();
            m_childBounds.makeEmpty();
        }

        ALEMBIC_OPERATOR_BOOL( valid() );

    protected:
        friend class IPointsSchema;
        Abc::P3fArraySamplePtr m_positions;
        Abc::UInt64ArraySamplePtr m_ids;
        Abc::V3fArraySamplePtr m_velocities;

        Abc::Box3d m_selfBounds;
        Abc::Box3d m_childBounds;
    };

    //-*************************************************************************
    // POINTS SCHEMA
    //-*************************************************************************
public:
    //! By convention we always define this_type in AbcGeom classes.
    //! Used by unspecified-bool-type conversion below
    typedef IPointsSchema this_type;

    //-*************************************************************************
    // CONSTRUCTION, DESTRUCTION, ASSIGNMENT
    //-*************************************************************************

    //! The default constructor creates an empty OPointsSchema
    //! ...
    IPointsSchema() {}

    //! This templated, explicit function creates a new scalar property reader.
    //! The first argument is any Abc (or AbcCoreAbstract) object
    //! which can intrusively be converted to an CompoundPropertyReaderPtr
    //! to use as a parent, from which the error handler policy for
    //! inheritance is also derived.  The remaining optional arguments
    //! can be used to override the ErrorHandlerPolicy, to specify
    //! MetaData, and to set TimeSamplingType.
    template <class CPROP_PTR>
    IPointsSchema( CPROP_PTR iParent,
                   const std::string &iName,

                   const Abc::Argument &iArg0 = Abc::Argument(),
                   const Abc::Argument &iArg1 = Abc::Argument() )
      : IGeomBaseSchema<PointsSchemaInfo>( iParent, iName,
                                          iArg0, iArg1 )
    {
        init( iArg0, iArg1 );
    }

    //! This constructor is the same as above, but with default
    //! schema name used.
    template <class CPROP_PTR>
    explicit IPointsSchema( CPROP_PTR iParent,
                            const Abc::Argument &iArg0 = Abc::Argument(),
                            const Abc::Argument &iArg1 = Abc::Argument() )
      : IGeomBaseSchema<PointsSchemaInfo>( iParent,
                                     iArg0, iArg1 )
    {
        init( iArg0, iArg1 );
    }

    //! Copy constructor.
    IPointsSchema(const IPointsSchema& iCopy)
        : IGeomBaseSchema<PointsSchemaInfo>()
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
    { return std::max( m_positionsProperty.getNumSamples(),
                       m_idsProperty.getNumSamples() ); }

    //! Ask if we're constant - no change in value amongst samples,
    //! regardless of the time sampling.
    bool isConstant() const { return m_positionsProperty.isConstant() && m_idsProperty.isConstant(); }

    //! Time sampling Information.
    //!
    AbcA::TimeSamplingPtr getTimeSampling() const
    {
        if ( m_positionsProperty.valid() )
        {
            return m_positionsProperty.getTimeSampling();
        }
        return getObject().getArchive().getTimeSampling(0);
    }

    //-*************************************************************************
    void get( Sample &oSample,
              const Abc::ISampleSelector &iSS = Abc::ISampleSelector() ) const
    {
        ALEMBIC_ABC_SAFE_CALL_BEGIN( "IPointsSchema::get()" );

        m_positionsProperty.get( oSample.m_positions, iSS );
        m_idsProperty.get( oSample.m_ids, iSS );

        m_selfBoundsProperty.get( oSample.m_selfBounds, iSS );

        if ( m_childBoundsProperty && m_childBoundsProperty.getNumSamples() > 0 )
        { m_childBoundsProperty.get( oSample.m_childBounds, iSS ); }

        if ( m_velocitiesProperty && m_velocitiesProperty.getNumSamples() > 0 )
        { m_velocitiesProperty.get( oSample.m_velocities, iSS ); }

        // Could error check here.

        ALEMBIC_ABC_SAFE_CALL_END();
    }

    Sample getValue( const Abc::ISampleSelector &iSS = Abc::ISampleSelector() ) const
    {
        Sample smp;
        get( smp, iSS );
        return smp;
    }

    Abc::IP3fArrayProperty getPositionsProperty() const
    {
        return m_positionsProperty;
    }

    Abc::IV3fArrayProperty getVelocitiesProperty() const
    {
        return m_velocitiesProperty;
    }

    Abc::IUInt64ArrayProperty getIdsProperty() const
    {
        return m_idsProperty;
    }

    IFloatGeomParam getWidthsParam() const
    {
        return m_widthsParam;
    }

    //-*************************************************************************
    // ABC BASE MECHANISMS
    // These functions are used by Abc to deal with errors, rewrapping,
    // and so on.
    //-*************************************************************************

    //! Reset returns this function set to an empty, default
    //! state.
    void reset()
    {
        m_positionsProperty.reset();
        m_velocitiesProperty.reset();
        m_idsProperty.reset();
        m_widthsParam.reset();

        IGeomBaseSchema<PointsSchemaInfo>::reset();
    }

    //! Valid returns whether this function set is
    //! valid.
    bool valid() const
    {
        return ( IGeomBaseSchema<PointsSchemaInfo>::valid() &&
                 m_positionsProperty.valid() &&
                 m_idsProperty.valid() );
    }

    //! unspecified-bool-type operator overload.
    //! ...
    ALEMBIC_OVERRIDE_OPERATOR_BOOL( IPointsSchema::valid() );

protected:
    void init( const Abc::Argument &iArg0,
               const Abc::Argument &iArg1 );

    Abc::IP3fArrayProperty m_positionsProperty;
    Abc::IUInt64ArrayProperty m_idsProperty;
    Abc::IV3fArrayProperty m_velocitiesProperty;
    IFloatGeomParam m_widthsParam;
};

//-*****************************************************************************
typedef Abc::ISchemaObject<IPointsSchema> IPoints;

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcGeom
} // End namespace Alembic

#endif
