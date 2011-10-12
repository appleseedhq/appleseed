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

#include <Alembic/AbcGeom/OCamera.h>
#include <Alembic/AbcGeom/GeometryScope.h>

namespace Alembic {
namespace AbcGeom {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
void OCameraSchema::set( const CameraSample &iSamp )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OCameraSchema::set()" );

    // do we need to create child bounds?
    if ( iSamp.getChildBounds().hasVolume() && !m_childBoundsProperty)
    {
        m_childBoundsProperty = Abc::OBox3dProperty( this->getPtr(), ".childBnds" );
        Abc::Box3d emptyBox;
        emptyBox.makeEmpty();

        // -1 because we just dis an m_positions set above
        size_t numSamples = m_coreProperties.getNumSamples() - 1;

        // set all the missing samples
        for ( size_t i = 0; i < numSamples; ++i )
        {
            m_childBoundsProperty.set( emptyBox );
        }
    }

    double sampleData[16];
    for ( size_t i = 0; i < 16; ++i )
        sampleData[i] = iSamp.getCoreValue( i );

    if ( m_coreProperties.getNumSamples() == 0 )
    {
        m_initialSample = iSamp;

        std::size_t numChannels = iSamp.getNumOpChannels();
        std::size_t numOps = iSamp.getNumOps();

        std::vector < std::string > filmBackOps( numOps );
        std::vector <double> opChannels ( numChannels );

        std::size_t curChannel = 0;
        for ( std::size_t i = 0; i < numOps; ++i )
        {
            const FilmBackXformOp & op = iSamp[i];
            filmBackOps[i] = op.getTypeAndHint();
            for ( std::size_t j = 0; j < op.getNumChannels(); 
                ++j, ++curChannel )
            {
                opChannels[curChannel] = op.getChannelValue( j );
            }
        }

        // we are in scalar territory, write the ops as scalar
        if ( numOps > 0 && numOps < 256 )
        {
            AbcA::DataType dType( Util::kStringPOD, numOps );
            Abc::OScalarProperty filmBackOpsProp( this->getPtr(),
                ".filmBackOps", dType );
            filmBackOpsProp.set( &filmBackOps.front() );
        }
        // too big for scalar, write ops as an array
        else if ( numChannels >= 256 )
        {
            OStringArrayProperty filmBackOpsProp( this->getPtr(),
                ".filmBackOps" );
            StringArraySample ssamp( &filmBackOps.front(), filmBackOps.size() );
            filmBackOpsProp.set( ssamp );
        }

        // do the same thing for the channels
        if ( numChannels > 0 && numChannels < 256 )
        {
            AbcA::DataType dType( Util::kFloat64POD, numChannels );
            m_smallFilmBackChannelsProperty = Abc::OScalarProperty( this->getPtr(),
                ".filmBackChannels", dType );
            m_smallFilmBackChannelsProperty.set( &opChannels.front() );

        }
        else if ( numChannels >= 256 )
        {
            m_bigFilmBackChannelsProperty = Abc::ODoubleArrayProperty( this->getPtr(),
                ".filmBackChannels" );
            DoubleArraySample dsamp( &opChannels.front(), opChannels.size() );
            m_bigFilmBackChannelsProperty.set( dsamp );
        }

        if ( m_childBoundsProperty )
        {
            m_childBoundsProperty.set( iSamp.getChildBounds() );
        }
    }
    else
    {
        std::size_t numOps = iSamp.getNumOps();
        ABCA_ASSERT( numOps == m_initialSample.getNumOps(),
            "Number of Film Back Xform Ops differ expected: " <<
            m_initialSample.getNumOps() << " got: " << numOps );

        std::vector <double> opChannels ( m_initialSample.getNumOpChannels() );
        std::size_t chan = 0;
        for ( std::size_t i = 0; i < numOps; ++i )
        {
            const FilmBackXformOp & op = iSamp[i];
            const FilmBackXformOp & oldOp = m_initialSample[i];

            ABCA_ASSERT( oldOp.getType() == op.getType(),
                "Film Back Xform Operation type differs from initial sample"
                " at index: " << i );

            std::size_t numChannels = op.getNumChannels();
            for ( std::size_t j = 0; j < numChannels; ++j, ++chan )
            {
                opChannels[chan] = op.getChannelValue( j );
            }
        }

        if ( m_smallFilmBackChannelsProperty )
        {
            m_smallFilmBackChannelsProperty.set( &opChannels.front() );
        }
        else if ( m_bigFilmBackChannelsProperty )
        {
            DoubleArraySample dsamp( &opChannels.front(), opChannels.size() );
            m_bigFilmBackChannelsProperty.set( dsamp );
        }
        // else no film back channels

        if ( m_childBoundsProperty )
        {
            SetPropUsePrevIfNull( m_childBoundsProperty, iSamp.getChildBounds() );
        }
    }

    m_coreProperties.set( sampleData );

    ALEMBIC_ABC_SAFE_CALL_END();
}

//-*****************************************************************************
void OCameraSchema::setFromPrevious()
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OCameraSchema::setFromPrevious" );

    m_coreProperties.setFromPrevious();

    if ( m_childBoundsProperty )
        m_childBoundsProperty.setFromPrevious();

    if ( m_smallFilmBackChannelsProperty )
        m_smallFilmBackChannelsProperty.setFromPrevious();

    if ( m_bigFilmBackChannelsProperty )
        m_bigFilmBackChannelsProperty.setFromPrevious();

    ALEMBIC_ABC_SAFE_CALL_END();
}

//-*****************************************************************************
void OCameraSchema::setTimeSampling( uint32_t iIndex )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN(
        "OCameraSchema::setTimeSampling( uint32_t )" );

    m_coreProperties.setTimeSampling( iIndex );

    ALEMBIC_ABC_SAFE_CALL_END();
}

//-*****************************************************************************
void OCameraSchema::setTimeSampling( AbcA::TimeSamplingPtr iTime )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN(
        "OCameraSchema::setTimeSampling( TimeSamplingPtr )" );

    if (iTime)
    {
        uint32_t tsIndex = getObject().getArchive().addTimeSampling(*iTime);
        setTimeSampling( tsIndex );
    }

    ALEMBIC_ABC_SAFE_CALL_END();
}

//-*****************************************************************************
void OCameraSchema::init( uint32_t iTsIdx )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OCameraSchema::init()" );

    AbcA::CompoundPropertyWriterPtr _this = this->getPtr();

    // 14 double values
    AbcA::DataType dType( Util::kFloat64POD, 16 );
    m_coreProperties = Abc::OScalarProperty( _this, ".core", dType, iTsIdx );

    ALEMBIC_ABC_SAFE_CALL_END_RESET();
}

//-*****************************************************************************
Abc::OCompoundProperty OCameraSchema::getArbGeomParams()
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OCameraSchema::getArbGeomParams()" );

    if ( ! m_arbGeomParams )
    {
        m_arbGeomParams = Abc::OCompoundProperty( this->getPtr(),
                                                  ".arbGeomParams" );
    }

    return m_arbGeomParams;

    ALEMBIC_ABC_SAFE_CALL_END();

    Abc::OCompoundProperty ret;
    return ret;
}

//-*****************************************************************************
Abc::OCompoundProperty OCameraSchema::getUserProperties()
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OCameraSchema::getUserProperties()" );

    if ( ! m_userProperties )
    {
        m_userProperties = Abc::OCompoundProperty( this->getPtr(),
                                                  ".userProperties" );
    }

    return m_userProperties;

    ALEMBIC_ABC_SAFE_CALL_END();

    Abc::OCompoundProperty ret;
    return ret;
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace AbcGeom
} // End namespace Alembic
