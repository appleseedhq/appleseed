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

#include <Alembic/AbcGeom/OSubD.h>
#include <Alembic/AbcGeom/GeometryScope.h>
#include <Alembic/AbcGeom/OFaceSet.h>

namespace Alembic {
namespace AbcGeom {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
inline void SetPropUsePrevIfNull( Abc::OInt32Property iProp, int32_t iVal )
{
    if ( iVal != ABC_GEOM_SUBD_NULL_INT_VALUE )
    {
        iProp.set( iVal );
    }
    else
    {
        iProp.setFromPrevious( );
    }
}

//-*****************************************************************************
void OSubDSchema::set( const Sample &iSamp )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OSubDSchema::set()" );

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
        m_faceIndicesProperty.set( iSamp.getFaceIndices() );
        m_faceCountsProperty.set( iSamp.getFaceCounts() );

        if ( m_childBoundsProperty )
        { m_childBoundsProperty.set( iSamp.getChildBounds() ); }

        if ( iSamp.getSelfBounds().isEmpty() )
        {
            // OTypedScalarProperty::set() is not referentially transparent,
            // so we need a a placeholder variable.
            Abc::Box3d bnds(
                ComputeBoundsFromPositions( iSamp.getPositions() )
                           );
            m_selfBoundsProperty.set( bnds );
        }
        else
        {
            m_selfBoundsProperty.set( iSamp.getSelfBounds() );
        }

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

        if ( iSamp.getFaceVaryingInterpolateBoundary() ==
             ABC_GEOM_SUBD_NULL_INT_VALUE )
        {
            m_faceVaryingInterpolateBoundaryProperty.set( 0 );
        }
        else
        {
            m_faceVaryingInterpolateBoundaryProperty.set(
                iSamp.getFaceVaryingInterpolateBoundary() );
        }
        if ( iSamp.getFaceVaryingPropagateCorners() ==
             ABC_GEOM_SUBD_NULL_INT_VALUE )
        {
            m_faceVaryingPropagateCornersProperty.set( 0 );
        }
        else
        {
            m_faceVaryingPropagateCornersProperty.set(
                iSamp.getFaceVaryingPropagateCorners() );
        }
        if ( iSamp.getInterpolateBoundary() ==
             ABC_GEOM_SUBD_NULL_INT_VALUE )
        {
            m_interpolateBoundaryProperty.set( 0 );
        }
        else
        {
            m_interpolateBoundaryProperty.set( iSamp.getInterpolateBoundary() );
        }

        m_subdSchemeProperty.set( iSamp.getSubdivisionScheme() );

        if ( iSamp.getCreaseIndices() || iSamp.getCreaseLengths() ||
            iSamp.getCreaseSharpnesses() )
        {
            initCreases(0);
        }

        if ( iSamp.getCreaseIndices() )
        {
            m_creaseIndicesProperty.set( iSamp.getCreaseIndices() );
        }

        if ( iSamp.getCreaseLengths() )
        {
            m_creaseLengthsProperty.set( iSamp.getCreaseLengths() );
        }

        if ( iSamp.getCreaseSharpnesses() )
        {
            m_creaseSharpnessesProperty.set( iSamp.getCreaseSharpnesses() );
        }

        if ( iSamp.getCornerIndices() || iSamp.getCornerSharpnesses() )
        {
            initCorners(0);
        }

        if ( iSamp.getCornerIndices() )
        {
            m_cornerIndicesProperty.set( iSamp.getCornerIndices() );
        }

        if ( iSamp.getCornerSharpnesses() )
        {
            m_cornerSharpnessesProperty.set( iSamp.getCornerSharpnesses() );
        }

        if ( iSamp.getHoles() )
        {
            initHoles(0);
        }

        if ( iSamp.getHoles() )
        {
            m_holesProperty.set( iSamp.getHoles() );
        }

    }
    else
    {
        SetPropUsePrevIfNull( m_positionsProperty, iSamp.getPositions() );
        SetPropUsePrevIfNull( m_faceIndicesProperty, iSamp.getFaceIndices() );
        SetPropUsePrevIfNull( m_faceCountsProperty, iSamp.getFaceCounts() );

        SetPropUsePrevIfNull( m_faceVaryingInterpolateBoundaryProperty,
                              iSamp.getFaceVaryingInterpolateBoundary() );
        SetPropUsePrevIfNull( m_faceVaryingPropagateCornersProperty,
                              iSamp.getFaceVaryingPropagateCorners() );
        SetPropUsePrevIfNull( m_interpolateBoundaryProperty,
                              iSamp.getInterpolateBoundary() );

        if ( ( iSamp.getCreaseIndices() || iSamp.getCreaseLengths() ||
               iSamp.getCreaseSharpnesses() ) && !m_creaseIndicesProperty )
        {
            initCreases(m_positionsProperty.getNumSamples() - 1);
        }

        if ( m_creaseIndicesProperty )
        {
            SetPropUsePrevIfNull( m_creaseIndicesProperty,
                              iSamp.getCreaseIndices() );
            SetPropUsePrevIfNull( m_creaseLengthsProperty,
                              iSamp.getCreaseLengths() );
            SetPropUsePrevIfNull( m_creaseSharpnessesProperty,
                              iSamp.getCreaseSharpnesses() );
        }

        if ( ( iSamp.getCornerIndices() || iSamp.getCornerSharpnesses() ) &&
             !m_cornerIndicesProperty )
        {
            initCorners(m_positionsProperty.getNumSamples() - 1);
        }

        if ( m_cornerIndicesProperty )
        {
            SetPropUsePrevIfNull( m_cornerIndicesProperty,
                              iSamp.getCornerIndices() );
            SetPropUsePrevIfNull( m_cornerSharpnessesProperty,
                              iSamp.getCornerSharpnesses() );
        }

        if ( iSamp.getHoles() && !m_holesProperty )
        {
            initHoles(m_positionsProperty.getNumSamples() - 1);
        }

        if ( m_holesProperty )
        {
            SetPropUsePrevIfNull( m_holesProperty, iSamp.getHoles() );
        }

        SetPropUsePrevIfNull( m_subdSchemeProperty, iSamp.getSubdivisionScheme() );

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
                ComputeBoundsFromPositions( iSamp.getPositions() )
                           );
            m_selfBoundsProperty.set( bnds );
        }
        else
        {
            m_selfBoundsProperty.setFromPrevious();
        }

        if ( m_uvsParam ) { m_uvsParam.set( iSamp.getUVs() ); }
    }

    ALEMBIC_ABC_SAFE_CALL_END();
}

//-*****************************************************************************
void OSubDSchema::setFromPrevious()
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OSubDSchema::setFromPrevious" );

    m_positionsProperty.setFromPrevious();
    m_faceIndicesProperty.setFromPrevious();
    m_faceCountsProperty.setFromPrevious();

    m_faceVaryingInterpolateBoundaryProperty.setFromPrevious();
    m_faceVaryingPropagateCornersProperty.setFromPrevious();
    m_interpolateBoundaryProperty.setFromPrevious();

    if ( m_creaseIndicesProperty )
    {
        m_creaseIndicesProperty.setFromPrevious();
        m_creaseLengthsProperty.setFromPrevious();
        m_creaseSharpnessesProperty.setFromPrevious();
    }

    if ( m_cornerIndicesProperty )
    {
        m_cornerIndicesProperty.setFromPrevious();
        m_cornerSharpnessesProperty.setFromPrevious();
    }

    if ( m_holesProperty )
    {
        m_holesProperty.setFromPrevious();
    }

    m_subdSchemeProperty.setFromPrevious();

    m_selfBoundsProperty.setFromPrevious();

    if ( m_childBoundsProperty )
    {
        m_childBoundsProperty.setFromPrevious();
    }

    if ( m_uvsParam ) { m_uvsParam.setFromPrevious(); }

    ALEMBIC_ABC_SAFE_CALL_END();
}

//-*****************************************************************************
void OSubDSchema::setTimeSampling( uint32_t iIndex )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN(
        "OSubDSchema::setTimeSampling( uint32_t )" );

    m_positionsProperty.setTimeSampling( iIndex );
    m_faceIndicesProperty.setTimeSampling( iIndex );
    m_faceCountsProperty.setTimeSampling( iIndex );
    m_faceVaryingInterpolateBoundaryProperty.setTimeSampling( iIndex );
    m_faceVaryingPropagateCornersProperty.setTimeSampling( iIndex );
    m_interpolateBoundaryProperty.setTimeSampling( iIndex );
    m_subdSchemeProperty.setTimeSampling( iIndex );
    m_selfBoundsProperty.setTimeSampling( iIndex );

    if ( m_creaseIndicesProperty )
    {
        m_creaseIndicesProperty.setTimeSampling( iIndex );
    }

    if ( m_creaseLengthsProperty )
    {
        m_creaseLengthsProperty.setTimeSampling( iIndex );
    }

    if ( m_creaseSharpnessesProperty )
    {
        m_creaseSharpnessesProperty.setTimeSampling( iIndex );
    }

    if ( m_cornerIndicesProperty )
    {
        m_cornerIndicesProperty.setTimeSampling( iIndex );
    }

    if ( m_cornerSharpnessesProperty )
    {
        m_cornerSharpnessesProperty.setTimeSampling( iIndex );
    }

    if ( m_holesProperty )
    {
        m_holesProperty.setTimeSampling( iIndex );
    }

    if ( m_childBoundsProperty )
    {
        m_childBoundsProperty.setTimeSampling( iIndex );
    }

    if ( m_uvsParam )
    {
        m_uvsParam.setTimeSampling( iIndex );
    }

    ALEMBIC_ABC_SAFE_CALL_END();
}

//-*****************************************************************************
void OSubDSchema::setTimeSampling( AbcA::TimeSamplingPtr iTime )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN(
        "OSubDSchema::setTimeSampling( TimeSamplingPtr )" );

    if (iTime)
    {
        uint32_t tsIndex = getObject().getArchive().addTimeSampling(*iTime);
        setTimeSampling( tsIndex );
    }

    ALEMBIC_ABC_SAFE_CALL_END();
}

//-*****************************************************************************
OFaceSet &
OSubDSchema::createFaceSet( const std::string &iFaceSetName )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OSubDSchema::createFaceSet ()" );

    ABCA_ASSERT( m_faceSets.find (iFaceSetName) == m_faceSets.end (),
                 "faceSet has already been created in SubD." );

    m_faceSets [iFaceSetName] = OFaceSet (this->getParent ().getObject (),
        iFaceSetName);

    return m_faceSets [iFaceSetName];

    ALEMBIC_ABC_SAFE_CALL_END();

    static OFaceSet empty;
    return empty;
}

//-*****************************************************************************
void OSubDSchema::init( uint32_t iTsIdx )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OSubDSchema::init()" );

    AbcA::MetaData mdata;
    SetGeometryScope( mdata, kVertexScope );

    AbcA::CompoundPropertyWriterPtr _this = this->getPtr();

    m_positionsProperty = Abc::OP3fArrayProperty( _this, "P", mdata, iTsIdx );

    m_faceIndicesProperty = Abc::OInt32ArrayProperty( _this, ".faceIndices", iTsIdx );

    m_faceCountsProperty = Abc::OInt32ArrayProperty( _this, ".faceCounts", iTsIdx );

    m_faceVaryingInterpolateBoundaryProperty =
        Abc::OInt32Property( _this, ".faceVaryingInterpolateBoundary", iTsIdx );

    m_faceVaryingPropagateCornersProperty =
        Abc::OInt32Property( _this, ".faceVaryingPropagateCorners", iTsIdx );

    m_interpolateBoundaryProperty =
        Abc::OInt32Property( _this, ".interpolateBoundary", iTsIdx );

    m_subdSchemeProperty = Abc::OStringProperty( _this, ".scheme", iTsIdx );

    ALEMBIC_ABC_SAFE_CALL_END_RESET();
}

//-*****************************************************************************
void OSubDSchema::initCreases(uint32_t iNumSamples)
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OSubDSchema::initCreases()" );

    AbcA::CompoundPropertyWriterPtr _this = this->getPtr();

    AbcA::TimeSamplingPtr ts = m_positionsProperty.getTimeSampling();

    m_creaseIndicesProperty = Abc::OInt32ArrayProperty( _this, ".creaseIndices", ts );

    m_creaseLengthsProperty = Abc::OInt32ArrayProperty( _this, ".creaseLengths", ts );

    m_creaseSharpnessesProperty = Abc::OFloatArrayProperty( _this, ".creaseSharpnesses",
        ts );

    std::vector<int32_t> emptyInt;
    std::vector<float32_t> emptyFloat;

    // set the appropriate samples to empty
    for (uint32_t i = 0; i < iNumSamples; ++i)
    {
        m_creaseIndicesProperty.set( Abc::Int32ArraySample( emptyInt ) );
        m_creaseLengthsProperty.set( Abc::Int32ArraySample( emptyInt ) );
        m_creaseSharpnessesProperty.set( Abc::FloatArraySample( emptyFloat ) );
    }

    ALEMBIC_ABC_SAFE_CALL_END_RESET();
}

//-*****************************************************************************
void OSubDSchema::initCorners(uint32_t iNumSamples)
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OSubDSchema::initCorners()" );

    AbcA::CompoundPropertyWriterPtr _this = this->getPtr();

    AbcA::TimeSamplingPtr ts = m_positionsProperty.getTimeSampling();

    m_cornerIndicesProperty = Abc::OInt32ArrayProperty( _this, ".cornerIndices", ts );

    m_cornerSharpnessesProperty = Abc::OFloatArrayProperty( _this, ".cornerSharpnesses",
        ts );

    std::vector<int32_t> emptyInt;
    std::vector<float32_t> emptyFloat;

    // set the appropriate samples to empty
    for (uint32_t i = 0; i < iNumSamples; ++i)
    {
        m_cornerIndicesProperty.set( Abc::Int32ArraySample( emptyInt ) );
        m_cornerSharpnessesProperty.set( Abc::FloatArraySample( emptyFloat ) );
    }

    ALEMBIC_ABC_SAFE_CALL_END_RESET();
}


//-*****************************************************************************
void OSubDSchema::initHoles(uint32_t iNumSamples)
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OSubDSchema::initHoles()" );

    AbcA::TimeSamplingPtr ts = m_positionsProperty.getTimeSampling();

    m_holesProperty = Abc::OInt32ArrayProperty( this->getPtr(), ".holes", ts );

    std::vector<int32_t> emptyInt;

    // set the appropriate samples to empty
    for (uint32_t i = 0; i < iNumSamples; ++i)
    {
        m_holesProperty.set( Abc::Int32ArraySample( emptyInt ) );
    }

    ALEMBIC_ABC_SAFE_CALL_END_RESET();
}

//-*****************************************************************************
void OSubDSchema::getFaceSetNames (std::vector <std::string> & oFaceSetNames)
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OSubDSchema::getFaceSetNames()" );

    for (std::map<std::string, OFaceSet>::const_iterator faceSetIter =
        m_faceSets.begin(); faceSetIter != m_faceSets.end(); ++faceSetIter)
    {
        oFaceSetNames.push_back( faceSetIter->first );
    }

    ALEMBIC_ABC_SAFE_CALL_END();
}

//-*****************************************************************************
bool
OSubDSchema::hasFaceSet( const std::string &iFaceSetName )
{
    return (m_faceSets.find (iFaceSetName) != m_faceSets.end ());
}

//-*****************************************************************************
OFaceSet
OSubDSchema::getFaceSet( const std::string &iFaceSetName )
{

    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OSubDSchema::getFaceSet()" );

    return m_faceSets [iFaceSetName];

    ALEMBIC_ABC_SAFE_CALL_END();

    OFaceSet empty;
    return empty;
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace AbcGeom
} // End namespace Alembic
