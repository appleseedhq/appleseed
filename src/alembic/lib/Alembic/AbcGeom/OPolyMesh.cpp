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

#include <Alembic/AbcGeom/OPolyMesh.h>
#include <Alembic/AbcGeom/GeometryScope.h>

namespace Alembic {
namespace AbcGeom {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
void OPolyMeshSchema::set( const Sample &iSamp )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OPolyMeshSchema::set()" );

    // do we need to create child bounds?
    if ( iSamp.getChildBounds().hasVolume() && !m_childBoundsProperty )
    {
        m_childBoundsProperty = Abc::OBox3dProperty( this->getPtr(), ".childBnds",
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

    // We could add sample integrity checking here.
    if ( m_positionsProperty.getNumSamples() == 0 )
    {
        // First sample must be valid on all points.
        ABCA_ASSERT( iSamp.getPositions() &&
                     iSamp.getFaceIndices() &&
                     iSamp.getFaceCounts(),
                     "Sample 0 must have valid data for all mesh components" );

        m_positionsProperty.set( iSamp.getPositions() );
        m_indicesProperty.set( iSamp.getFaceIndices() );
        m_countsProperty.set( iSamp.getFaceCounts() );

        if (m_childBoundsProperty)
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

        if ( iSamp.getUVs().getVals() )
        {
            if ( iSamp.getUVs().getIndices() )
            {
                // UVs are indexed
                m_uvsParam = OV2fGeomParam( this->getPtr(), "uv", true,
                                       iSamp.getUVs().getScope(), 1,
                                       this->getTimeSampling() );
            }
            else
            {
                // UVs are not indexed
                m_uvsParam = OV2fGeomParam( this->getPtr(), "uv", false,
                                       iSamp.getUVs().getScope(), 1,
                                       this->getTimeSampling() );
            }

            m_uvsParam.set( iSamp.getUVs() );
        }
        if ( iSamp.getNormals().getVals() )
        {
            if ( iSamp.getNormals().getIndices() )
            {
                // normals are indexed
                m_normalsParam = ON3fGeomParam( this->getPtr(), "N", true,
                                           iSamp.getNormals().getScope(),
                                           1, this->getTimeSampling() );
            }
            else
            {
                // normals are not indexed
                m_normalsParam = ON3fGeomParam( this->getPtr(), "N", false,
                                           iSamp.getNormals().getScope(), 1,
                                           this->getTimeSampling() );
            }

            m_normalsParam.set( iSamp.getNormals() );
        }
    }
    else
    {
        SetPropUsePrevIfNull( m_positionsProperty, iSamp.getPositions() );
        SetPropUsePrevIfNull( m_indicesProperty, iSamp.getFaceIndices() );
        SetPropUsePrevIfNull( m_countsProperty, iSamp.getFaceCounts() );

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

        // OGeomParam will automatically use SetPropUsePrevIfNull internally
        if ( m_uvsParam ) { m_uvsParam.set( iSamp.getUVs() ); }
        if ( m_normalsParam ) { m_normalsParam.set( iSamp.getNormals() ); }
    }

    ALEMBIC_ABC_SAFE_CALL_END();
}

//-*****************************************************************************
void OPolyMeshSchema::setFromPrevious()
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OPolyMeshSchema::setFromPrevious" );

    m_positionsProperty.setFromPrevious();
    m_indicesProperty.setFromPrevious();
    m_countsProperty.setFromPrevious();

    m_selfBoundsProperty.setFromPrevious();

    if (m_childBoundsProperty) { m_childBoundsProperty.setFromPrevious(); }

    if ( m_uvsParam ) { m_uvsParam.setFromPrevious(); }
    if ( m_normalsParam ) { m_normalsParam.setFromPrevious(); }

    ALEMBIC_ABC_SAFE_CALL_END();
}

//-*****************************************************************************
void OPolyMeshSchema::setTimeSampling( uint32_t iIndex )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN(
        "OPolyMeshSchema::setTimeSampling( uint32_t )" );

    m_positionsProperty.setTimeSampling( iIndex );
    m_indicesProperty.setTimeSampling( iIndex );
    m_countsProperty.setTimeSampling( iIndex );
    m_selfBoundsProperty.setTimeSampling( iIndex );

    if ( m_childBoundsProperty )
    {
        m_childBoundsProperty.setTimeSampling( iIndex );
    }

    if ( m_uvsParam )
    {
        m_uvsParam.setTimeSampling( iIndex );
    }

    if ( m_normalsParam )
    {
        m_normalsParam.setTimeSampling( iIndex );
    }

    ALEMBIC_ABC_SAFE_CALL_END();
}

//-*****************************************************************************
void OPolyMeshSchema::setTimeSampling( AbcA::TimeSamplingPtr iTime )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN(
        "OPolyMeshSchema::setTimeSampling( TimeSamplingPtr )" );

    if ( iTime )
    {
        uint32_t tsIndex = getObject().getArchive().addTimeSampling( *iTime );
        setTimeSampling( tsIndex );
    }

    ALEMBIC_ABC_SAFE_CALL_END();
}

//-*****************************************************************************
void OPolyMeshSchema::init( uint32_t iTsIdx )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OPolyMeshSchema::init()" );

    AbcA::MetaData mdata;
    SetGeometryScope( mdata, kVertexScope );

    AbcA::CompoundPropertyWriterPtr _this = this->getPtr();

    m_positionsProperty = Abc::OP3fArrayProperty( _this, "P", mdata, iTsIdx );

    m_indicesProperty = Abc::OInt32ArrayProperty( _this, ".faceIndices", iTsIdx );

    m_countsProperty = Abc::OInt32ArrayProperty( _this, ".faceCounts", iTsIdx );

    // UVs and Normals are created on first call to set()

    ALEMBIC_ABC_SAFE_CALL_END_RESET();
}

//-*****************************************************************************
bool
OPolyMeshSchema::hasFaceSet( const std::string &iFaceSetName )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OPolyMeshSchema::hasFaceSet ()" );

    return (m_faceSets.find (iFaceSetName) != m_faceSets.end ());

    ALEMBIC_ABC_SAFE_CALL_END();

    return false;
}


//-*****************************************************************************
OFaceSet &
OPolyMeshSchema::createFaceSet( const std::string &iFaceSetName )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OPolyMeshSchema::createFaceSet ()" );

    ABCA_ASSERT( m_faceSets.find (iFaceSetName) == m_faceSets.end (),
                 "faceSet has already been created in polymesh." );

    m_faceSets[iFaceSetName] = OFaceSet( this->getParent().getObject(),
                                          iFaceSetName );

    return m_faceSets[iFaceSetName];

    ALEMBIC_ABC_SAFE_CALL_END();

    static OFaceSet emptyFaceSet;
    return emptyFaceSet;
}

//-*****************************************************************************
void OPolyMeshSchema::getFaceSetNames (std::vector <std::string> & oFaceSetNames)
{

    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OPolyMeshSchema::getFaceSetNames()" );
    for (std::map<std::string, OFaceSet>::const_iterator faceSetIter =
        m_faceSets.begin(); faceSetIter != m_faceSets.end(); ++faceSetIter)
    {
        oFaceSetNames.push_back( faceSetIter->first );
    }

    ALEMBIC_ABC_SAFE_CALL_END();
}

//-*****************************************************************************
OFaceSet
OPolyMeshSchema::getFaceSet( const std::string &iFaceSetName )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OPolyMeshSchema::getFaceSet()" );

    return m_faceSets[iFaceSetName];

    ALEMBIC_ABC_SAFE_CALL_END();

    OFaceSet empty;
    return empty;
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace AbcGeom
} // End namespace Alembic
