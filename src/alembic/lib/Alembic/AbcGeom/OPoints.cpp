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

#include <Alembic/AbcGeom/OPoints.h>
#include <Alembic/AbcGeom/GeometryScope.h>

namespace Alembic {
namespace AbcGeom {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
void OPointsSchema::set( const Sample &iSamp )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OPointsSchema::set()" );

    // do we need to create child bounds?
    if ( iSamp.getChildBounds().hasVolume() && !m_childBoundsProperty)
    {
        m_childBoundsProperty = Abc::OBox3dProperty( *this, ".childBnds",
                                             m_positionsProperty.getTimeSampling() );
        Abc::Box3d emptyBox;
        emptyBox.makeEmpty();

        size_t numSamples = m_positionsProperty.getNumSamples();

        // set all the missing samples
        for ( size_t i = 0; i < numSamples; ++i )
        {
            m_childBoundsProperty.set( emptyBox );
        }
    }

    // do we need to create velocities prop?
    if ( iSamp.getVelocities() && !m_velocitiesProperty )
    {
        m_velocitiesProperty = Abc::OV3fArrayProperty( this->getPtr(), ".velocities",
                                               m_positionsProperty.getTimeSampling() );

        const V3fArraySample empty;
        const size_t numSamps = m_positionsProperty.getNumSamples();
        for ( size_t i = 0 ; i < numSamps ; ++i )
        {
            m_velocitiesProperty.set( empty );
        }
    }

    // do we need to create widths prop?
    if ( iSamp.getWidths() && !m_widthsParam )
    {
        if ( iSamp.getWidths().getIndices() )
        {
            // widths are indexed which is wasteful, but technically ok
            m_widthsParam = OFloatGeomParam( this->getPtr(), ".widths", true,
                                             iSamp.getWidths().getScope(),
                                             1, this->getTimeSampling() );
        }
        else
        {
            // widths are not indexed
            m_widthsParam = OFloatGeomParam( this->getPtr(), ".widths", false,
                                             iSamp.getWidths().getScope(), 1,
                                             this->getTimeSampling() );
        }

        OFloatGeomParam::Sample empty;

        size_t numSamples = m_positionsProperty.getNumSamples();

        // set all the missing samples
        for ( size_t i = 0; i < numSamples; ++i )
        {
            m_widthsParam.set( empty );
        }
    }

    // We could add sample integrity checking here.
    if ( m_positionsProperty.getNumSamples() == 0 )
    {
        // First sample must be valid on all points.
        ABCA_ASSERT( iSamp.getPositions() &&
                     iSamp.getIds(),
                     "Sample 0 must have valid data for points and ids" );
        m_positionsProperty.set( iSamp.getPositions() );
        m_idsProperty.set( iSamp.getIds() );

        if ( m_velocitiesProperty )
        { m_velocitiesProperty.set( iSamp.getVelocities() ); }

        if ( m_widthsParam )
        { m_widthsParam.set( iSamp.getWidths() ); }

        if ( m_childBoundsProperty )
        { m_childBoundsProperty.set( iSamp.getChildBounds() ); }

        if ( iSamp.getSelfBounds().isEmpty() )
        {
            // OTypedScalarProperty::set() is not referentially transparent,
            // so we need a a placeholder variable.
            Abc::Box3d bnds(
                ComputeBoundsFromPositions( iSamp.getPositions() ) );
            m_selfBoundsProperty.set( bnds );
        }
        else { m_selfBoundsProperty.set( iSamp.getSelfBounds() ); }
    }
    else
    {
        SetPropUsePrevIfNull( m_positionsProperty, iSamp.getPositions() );
        SetPropUsePrevIfNull( m_idsProperty, iSamp.getIds() );
        SetPropUsePrevIfNull( m_velocitiesProperty, iSamp.getVelocities() );

        if ( m_childBoundsProperty )
        {
            SetPropUsePrevIfNull( m_childBoundsProperty, iSamp.getChildBounds() );
        }

        if ( iSamp.getSelfBounds().hasVolume() )
        {
            m_selfBoundsProperty.set( iSamp.getSelfBounds() );
        }
        else if ( iSamp.getPositions() )
        {
            Abc::Box3d bnds(
                ComputeBoundsFromPositions( iSamp.getPositions() ) );
            m_selfBoundsProperty.set( bnds );
        }
        else
        {
            m_selfBoundsProperty.setFromPrevious();
        }

        if ( m_widthsParam )
        { m_widthsParam.set( iSamp.getWidths() ); }
    }

    ALEMBIC_ABC_SAFE_CALL_END();
}

//-*****************************************************************************
void OPointsSchema::setFromPrevious()
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OPointsSchema::setFromPrevious" );

    m_positionsProperty.setFromPrevious();
    m_idsProperty.setFromPrevious();

    m_selfBoundsProperty.setFromPrevious();

    if ( m_childBoundsProperty )
    {
        m_childBoundsProperty.setFromPrevious();
    }

    if ( m_widthsParam )
    {
        m_widthsParam.setFromPrevious();
    }

    ALEMBIC_ABC_SAFE_CALL_END();
}

//-*****************************************************************************
void OPointsSchema::setTimeSampling( uint32_t iIndex )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN(
        "OPointsSchema::setTimeSampling( uint32_t )" );

    m_positionsProperty.setTimeSampling( iIndex );
    m_idsProperty.setTimeSampling( iIndex );
    m_selfBoundsProperty.setTimeSampling( iIndex );

    if ( m_childBoundsProperty )
    {
        m_childBoundsProperty.setTimeSampling( iIndex );
    }

    if ( m_widthsParam )
    {
        m_widthsParam.setTimeSampling( iIndex );
    }

    ALEMBIC_ABC_SAFE_CALL_END();
}

//-*****************************************************************************
void OPointsSchema::setTimeSampling( AbcA::TimeSamplingPtr iTime )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN(
        "OPointsSchema::setTimeSampling( TimeSamplingPtr )" );

    if (iTime)
    {
        uint32_t tsIndex = getObject().getArchive().addTimeSampling( *iTime );
        setTimeSampling( tsIndex );
    }

    ALEMBIC_ABC_SAFE_CALL_END();
}

//-*****************************************************************************
void OPointsSchema::init( uint32_t iTsIdx )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OPointsSchema::init()" );

    AbcA::MetaData mdata;
    SetGeometryScope( mdata, kVaryingScope );
    AbcA::CompoundPropertyWriterPtr _this = this->getPtr();

    m_positionsProperty = Abc::OP3fArrayProperty( _this, "P", mdata, iTsIdx );

    m_idsProperty = Abc::OUInt64ArrayProperty( _this, ".pointIds", mdata,
                                               iTsIdx );

    ALEMBIC_ABC_SAFE_CALL_END_RESET();
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace AbcGeom
} // End namespace Alembic
