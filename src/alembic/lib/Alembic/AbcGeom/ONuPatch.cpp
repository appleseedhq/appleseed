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

#include <Alembic/AbcGeom/ONuPatch.h>
#include <Alembic/AbcGeom/GeometryScope.h>
#include <iostream>

namespace Alembic {
namespace AbcGeom {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
inline void SetPropUsePrevIfNull( Abc::OInt32Property iProp, int32_t iVal )
{
    if ( ! iProp ) { return; }

    if ( iVal != ABC_GEOM_NUPATCH_NULL_INT_VALUE )
    {
        iProp.set( iVal );
    }
    else
    {
        iProp.setFromPrevious();
    }
}

//-*****************************************************************************
void ONuPatchSchema::set( const ONuPatchSchema::Sample &iSamp  )
{

    ALEMBIC_ABC_SAFE_CALL_BEGIN( "ONuPatchSchema::set()" );

    // do we need to create child bounds?
    if ( iSamp.getChildBounds().hasVolume() && !m_childBoundsProperty)
    {
        m_childBoundsProperty = Abc::OBox3dProperty( *this, ".childBnds",
                                             this->getTimeSampling() );
        Abc::Box3d emptyBox;
        emptyBox.makeEmpty();

        size_t numSamples = m_positionsProperty.getNumSamples();

        // set all the missing samples
        for ( size_t i = 0; i < numSamples; ++i )
        {
            m_childBoundsProperty.set( emptyBox );
        }
    }

    // do we need to create uvs?
    if ( iSamp.getUVs() && !m_uvsParam )
    {
        OV2fGeomParam::Sample empty;

        if ( iSamp.getUVs().getIndices() )
        {
            // UVs are indexed
            m_uvsParam = OV2fGeomParam( this->getPtr(), "uv", true,
                                   empty.getScope(), 1,
                                   this->getTimeSampling() );
        }
        else
        {
            // UVs are not indexed
            m_uvsParam = OV2fGeomParam( this->getPtr(), "uv", false,
                                   empty.getScope(), 1,
                                   this->getTimeSampling() );
        }

        size_t numSamples = m_positionsProperty.getNumSamples();

        // set all the missing samples
        for ( size_t i = 0; i < numSamples; ++i )
        {
            m_uvsParam.set( empty );
        }
    }

    // do we need to create normals?
    if ( iSamp.getNormals() && !m_normalsParam )
    {

        ON3fGeomParam::Sample empty;

        if ( iSamp.getNormals().getIndices() )
        {
            // normals are indexed
            m_normalsParam = ON3fGeomParam( this->getPtr(), "N", true,
                empty.getScope(), 1, this->getTimeSampling() );
        }
        else
        {
            // normals are not indexed
            m_normalsParam = ON3fGeomParam( this->getPtr(), "N", false,
                                        empty.getScope(), 1,
                                        this->getTimeSampling() );
        }

        size_t numSamples = m_positionsProperty.getNumSamples();

        // set all the missing samples
        for ( size_t i = 0; i < numSamples; ++i )
        {
            m_normalsParam.set( empty );
        }
    }

    // do we need to create position weights?
    if ( iSamp.getPositionWeights() && !m_positionWeightsProperty)
    {
        m_positionWeightsProperty = Abc::OFloatArrayProperty( *this, "w",
                                                      this->getTimeSampling() );

        Alembic::Abc::FloatArraySample emptySamp;

        size_t numSamples = m_positionsProperty.getNumSamples();

        // set all the missing samples
        for ( size_t i = 0; i < numSamples; ++i )
        {
            m_positionWeightsProperty.set( emptySamp );
        }
    }

    if ( iSamp.hasTrimCurve() && !m_trimNumLoopsProperty )
    {
        AbcA::CompoundPropertyWriterPtr _this = this->getPtr();
        Alembic::Abc::Int32ArraySample emptyIntSamp;
        Alembic::Abc::FloatArraySample emptyFloatSamp;

        AbcA::TimeSamplingPtr tsPtr = this->getTimeSampling();

        // trim curves
        m_trimNumLoopsProperty = Abc::OInt32Property( _this, "trim_nloops", tsPtr );
        m_trimNumCurvesProperty = Abc::OInt32ArrayProperty( _this, "trim_ncurves",
                                                     tsPtr );
        m_trimNumVerticesProperty = Abc::OInt32ArrayProperty( _this, "trim_n", tsPtr );
        m_trimOrderProperty = Abc::OInt32ArrayProperty( _this, "trim_order", tsPtr );
        m_trimKnotProperty = Abc::OFloatArrayProperty( _this, "trim_knot", tsPtr );
        m_trimMinProperty = Abc::OFloatArrayProperty( _this, "trim_min", tsPtr );
        m_trimMaxProperty = Abc::OFloatArrayProperty( _this, "trim_max", tsPtr );
        m_trimUProperty = Abc::OFloatArrayProperty( _this, "trim_u", tsPtr );
        m_trimVProperty = Abc::OFloatArrayProperty( _this, "trim_v", tsPtr );
        m_trimWProperty = Abc::OFloatArrayProperty( _this, "trim_w", tsPtr );

        size_t numSamples = m_positionsProperty.getNumSamples();

        // set all the missing samples
        for ( size_t i = 0; i < numSamples; ++i )
        {
            m_trimNumLoopsProperty.set( 0 );

            m_trimNumCurvesProperty.set( emptyIntSamp );
            m_trimNumVerticesProperty.set( emptyIntSamp );
            m_trimOrderProperty.set( emptyIntSamp );
            m_trimKnotProperty.set( emptyFloatSamp );
            m_trimMinProperty.set( emptyFloatSamp );
            m_trimMaxProperty.set( emptyFloatSamp );
            m_trimUProperty.set( emptyFloatSamp );
            m_trimVProperty.set( emptyFloatSamp );
            m_trimWProperty.set( emptyFloatSamp );
        }
    }


    // We could add sample integrity checking here.
    if ( m_positionsProperty.getNumSamples() == 0 )
    {
        // First sample must be valid on all points.
        ABCA_ASSERT( iSamp.getPositions(),
                     "Sample 0 must have valid data for all mesh components" );

        // set required properties
        m_positionsProperty.set( iSamp.getPositions() );
        m_numUProperty.set( iSamp.getNu() );
        m_numVProperty.set( iSamp.getNv() );
        m_uOrderProperty.set( iSamp.getUOrder() );
        m_vOrderProperty.set( iSamp.getVOrder() );
        m_uKnotProperty.set( iSamp.getUKnot() );
        m_vKnotProperty.set( iSamp.getVKnot() );

        if ( m_trimNumLoopsProperty )
        {
            m_trimNumLoopsProperty.set( iSamp.getTrimNumLoops() );

            m_trimNumCurvesProperty.set( iSamp.getTrimNumCurves() );
            m_trimNumVerticesProperty.set( iSamp.getTrimNumVertices() );
            m_trimOrderProperty.set( iSamp.getTrimOrder() );
            m_trimKnotProperty.set( iSamp.getTrimKnot() );
            m_trimMinProperty.set( iSamp.getTrimMin() );
            m_trimMaxProperty.set( iSamp.getTrimMax() );
            m_trimUProperty.set( iSamp.getTrimU() );
            m_trimVProperty.set( iSamp.getTrimV() );
            m_trimWProperty.set( iSamp.getTrimW() );
        }

        if ( m_positionWeightsProperty )
        {
            m_positionWeightsProperty.set( iSamp.getPositionWeights() );
        }

        if ( m_uvsParam )
        {
            m_uvsParam.set( iSamp.getUVs() );
        }

        if ( m_normalsParam )
        {
            m_normalsParam.set( iSamp.getNormals() );
        }

        // set bounds
        if ( iSamp.getChildBounds().hasVolume() )
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
    }
    else
    {
        // TODO this would all go away, remove the lightweight constructor
        SetPropUsePrevIfNull( m_positionsProperty, iSamp.getPositions() );
        SetPropUsePrevIfNull( m_numUProperty, iSamp.getNu() );
        SetPropUsePrevIfNull( m_numVProperty, iSamp.getNv() );
        SetPropUsePrevIfNull( m_uOrderProperty, iSamp.getUOrder() );
        SetPropUsePrevIfNull( m_vOrderProperty, iSamp.getVOrder() );
        SetPropUsePrevIfNull( m_uKnotProperty, iSamp.getUKnot() );
        SetPropUsePrevIfNull( m_vKnotProperty, iSamp.getVKnot() );

        if ( m_uvsParam )
        {
            m_uvsParam.set( iSamp.getUVs() );
        }

        if ( m_normalsParam )
        {
            m_normalsParam.set( iSamp.getNormals() );
        }

        if ( m_positionWeightsProperty )
        {
            m_positionWeightsProperty.set( iSamp.getPositionWeights() );
        }

        // handle trim curves
        if ( m_trimNumLoopsProperty )
        {
            SetPropUsePrevIfNull( m_trimNumLoopsProperty, iSamp.getTrimNumLoops() );
            SetPropUsePrevIfNull( m_trimNumCurvesProperty, iSamp.getTrimNumCurves() );
            SetPropUsePrevIfNull( m_trimNumVerticesProperty,
                                    iSamp.getTrimNumVertices() );
            SetPropUsePrevIfNull( m_trimOrderProperty, iSamp.getTrimOrder() );
            SetPropUsePrevIfNull( m_trimKnotProperty, iSamp.getTrimKnot() );
            SetPropUsePrevIfNull( m_trimMinProperty, iSamp.getTrimMin() );
            SetPropUsePrevIfNull( m_trimMaxProperty, iSamp.getTrimMax() );
            SetPropUsePrevIfNull( m_trimUProperty, iSamp.getTrimU() );
            SetPropUsePrevIfNull( m_trimVProperty, iSamp.getTrimV() );
            SetPropUsePrevIfNull( m_trimWProperty, iSamp.getTrimW() );
        }

        // update bounds
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
    }

    ALEMBIC_ABC_SAFE_CALL_END();
}

//-*****************************************************************************
void ONuPatchSchema::setFromPrevious( )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "ONuPatchSchema::setFromPrevious" );

    // handle required properites
    m_positionsProperty.setFromPrevious();
    m_numUProperty.setFromPrevious();
    m_numVProperty.setFromPrevious();
    m_uOrderProperty.setFromPrevious();
    m_vOrderProperty.setFromPrevious();
    m_uKnotProperty.setFromPrevious();
    m_vKnotProperty.setFromPrevious();

    m_selfBoundsProperty.setFromPrevious();
    m_childBoundsProperty.setFromPrevious();

    // handle option properties
    if ( m_uvsParam ) { m_uvsParam.setFromPrevious(); }
    if ( m_normalsParam ) { m_normalsParam.setFromPrevious(); }
    if ( m_positionWeightsProperty )
    {
        m_positionWeightsProperty.setFromPrevious();
    }

    // handle trim curves.
    if ( m_trimNumLoopsProperty )
    {
        m_trimNumLoopsProperty.setFromPrevious();
        m_trimNumCurvesProperty.setFromPrevious();
        m_trimNumVerticesProperty.setFromPrevious();
        m_trimOrderProperty.setFromPrevious();
        m_trimKnotProperty.setFromPrevious();
        m_trimMinProperty.setFromPrevious();
        m_trimMaxProperty.setFromPrevious();
        m_trimUProperty.setFromPrevious();
        m_trimVProperty.setFromPrevious();
        m_trimWProperty.setFromPrevious();
    }

    ALEMBIC_ABC_SAFE_CALL_END();
}

//-*****************************************************************************
void ONuPatchSchema::init( const AbcA::index_t iTsIdx )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "ONuPatchSchema::init()" );

    AbcA::MetaData mdata;
    SetGeometryScope( mdata, kVertexScope );

    AbcA::CompoundPropertyWriterPtr _this = this->getPtr();

    // initialize any required properties
    m_positionsProperty = Abc::OP3fArrayProperty( _this, "P", mdata, iTsIdx );
    m_numUProperty = Abc::OInt32Property( _this, "nu", iTsIdx );
    m_numVProperty = Abc::OInt32Property( _this, "nv", iTsIdx );
    m_uOrderProperty = Abc::OInt32Property( _this, "uOrder", iTsIdx );
    m_vOrderProperty = Abc::OInt32Property( _this, "vOrder", iTsIdx );
    m_uKnotProperty = Abc::OFloatArrayProperty( _this, "uKnot", iTsIdx );
    m_vKnotProperty = Abc::OFloatArrayProperty( _this, "vKnot", iTsIdx );

    ALEMBIC_ABC_SAFE_CALL_END_RESET();
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace AbcGeom
} // End namespace Alembic
