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

#include <Alembic/AbcGeom/OXform.h>
#include <Alembic/AbcGeom/XformOp.h>

#define MAX_SCALAR_CHANS 256

namespace Alembic {
namespace AbcGeom {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
void OXformSchema::setChannelValues( const std::vector<double> &iVals )
{
    if ( ! m_valsPWPtr ) { return; }

    if ( m_useArrayProp )
    {
        Alembic::Util::Dimensions dims(m_numChannels);
        m_valsPWPtr->asArrayPtr()->setSample(
            AbcA::ArraySample( &(iVals.front()),
                               AbcA::DataType( Alembic::Util::kFloat64POD, 1 ),
                               dims )
                                       );
    }
    else
    {
        m_valsPWPtr->asScalarPtr()->setSample( &(iVals.front()) );
    }
}

//-*****************************************************************************
void OXformSchema::set( XformSample &ioSamp )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OXformSchema::set()" );

    // do we need to create child bounds?
    if ( ioSamp.getChildBounds().hasVolume() && !m_childBoundsProperty )
    {
        m_childBoundsProperty = Abc::OBox3dProperty( this->getPtr(), ".childBnds",
                                             m_inheritsProperty.getTimeSampling() );

        Abc::Box3d emptyBox;
        emptyBox.makeEmpty();

        size_t numSamples = m_inheritsProperty.getNumSamples();

        // set all the missing samples
        for ( size_t i = 0; i < numSamples; ++i )
        {
            m_childBoundsProperty.set( emptyBox );
        }
    }


    if ( m_inheritsProperty.getNumSamples() == 0 )
    {
        // set this to true, so that additional calls to sample's addOp()
        // won't change the topology of the sample, but instead will merely
        // update values.
        ioSamp.freezeTopology();

        m_protoSample = ioSamp;

        m_numChannels = ioSamp.getNumOpChannels();
        m_numOps = ioSamp.getNumOps();

        m_staticChans = std::vector<bool>( m_numChannels, true );

        if ( m_numOps > 0 )
        {
            m_opsPWPtr = this->getPtr()->createScalarProperty(
                ".ops", AbcA::MetaData(),
                AbcA::DataType( Alembic::Util::kUint8POD, m_numOps ), 0
                                                        );
        }

        if ( m_numChannels > 0 )
        {
            uint32_t tsIndex = getObject().getArchive().addTimeSampling(
                                       *(m_inheritsProperty.getTimeSampling()) );
            if ( m_numChannels <= MAX_SCALAR_CHANS )
            {
                m_useArrayProp = false;

                m_valsPWPtr = this->getPtr()->createScalarProperty(
                    ".vals", AbcA::MetaData(),
                    AbcA::DataType( Alembic::Util::kFloat64POD, m_numChannels ),
                    tsIndex
                                                             );
            }
            else
            {
                m_useArrayProp = true;

                m_valsPWPtr = this->getPtr()->createArrayProperty(
                    ".vals", AbcA::MetaData(),
                    // the DataType for an ArrayProperty describes not how big
                    // each Sample is, but how many values constitute a single
                    // "element". What is here is the same as creating an
                    // Abc::ODoubleArrayProperty.
                    AbcA::DataType( Alembic::Util::kFloat64POD, 1 ), tsIndex
                                                            );
            }
        }

    }
    else
    {
        ABCA_ASSERT( m_protoSample.isTopologyEqual(ioSamp),
                     "Invalid sample topology!" );
    }

    if ( ioSamp.m_childBounds.hasVolume() )
    { m_childBoundsProperty.set( ioSamp.getChildBounds() ); }

    m_inheritsProperty.set( ioSamp.getInheritsXforms() );

    if ( ! m_opsPWPtr ) { return; }

    std::vector<double> chanvals;
    chanvals.reserve( ioSamp.getNumOpChannels() );

    std::vector<Alembic::Util::uint32_t> animchans;
    animchans.reserve( ioSamp.getNumOpChannels() );

    for ( size_t i = 0, ii = 0 ; i < m_numOps ; ++i )
    {
        const XformOp &op = ioSamp[i];

        const XformOp &protop = m_protoSample[i];

        for ( size_t j = 0 ; j < op.getNumChannels() ; ++j )
        {
            chanvals.push_back( op.getChannelValue( j ) );

            m_staticChans[j + ii] = m_staticChans[j + ii] &&
                Imath::equalWithAbsError( op.getChannelValue( j ),
                                          protop.getChannelValue( j ),
                                          kXFORM_DELTA_TOLERANCE );


            m_isIdentity = m_isIdentity &&
                Imath::equalWithAbsError( op.getChannelValue( j ),
                                          op.getDefaultChannelValue( j ),
                                          kXFORM_DELTA_TOLERANCE );
        }

        ii += op.getNumChannels();
    }

    for ( Alembic::Util::uint32_t i = 0 ; i < m_staticChans.size() ; ++i )
    {
        if ( ! m_staticChans[i] )
        {
            animchans.push_back( i );
        }
    }

    this->setChannelValues( chanvals );

    if ( m_opsPWPtr && m_opsPWPtr->getNumSamples() == 0 )
    {
        std::vector < Alembic::Util::uint8_t > opVec(
            m_protoSample.getNumOps() );

        for ( std::size_t i = 0; i < opVec.size(); ++i )
        {
            opVec[i] = m_protoSample[i].getOpEncoding();
        }

        m_opsPWPtr->setSample( &(opVec.front()) );
    }
    else if ( m_opsPWPtr )
    {
        m_opsPWPtr->setFromPreviousSample();
    }

    if ( !m_isNotConstantIdentityProperty && !m_isIdentity )
    {
        m_isNotConstantIdentityProperty = Abc::OBoolProperty( this->getPtr(),
                                                      "isNotConstantIdentity" );

        m_isNotConstantIdentityProperty.set( true );
    }

    m_animChannelsProperty.set( animchans );

    ALEMBIC_ABC_SAFE_CALL_END();
}

//-*****************************************************************************
void OXformSchema::setFromPrevious()
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OXformSchema::setFromPrevious()" );

    m_inheritsProperty.setFromPrevious();

    m_opsPWPtr->setFromPreviousSample();

    if ( m_valsPWPtr )
    {
        if ( m_useArrayProp )
        { m_valsPWPtr->asArrayPtr()->setFromPreviousSample(); }
        else
        { m_valsPWPtr->asScalarPtr()->setFromPreviousSample(); }
    }

    m_animChannelsProperty.setFromPrevious();

    if ( m_childBoundsProperty && m_childBoundsProperty.getNumSamples() > 0 )
    { m_childBoundsProperty.setFromPrevious(); }

    ALEMBIC_ABC_SAFE_CALL_END();
}

//-*****************************************************************************
size_t OXformSchema::getNumSamples() const
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OXformSchema::getNumSamples()" );

    if ( m_opsPWPtr )
    {
        return m_opsPWPtr->getNumSamples();
    }
    else
    {
        return 0;
    }

    ALEMBIC_ABC_SAFE_CALL_END();

    return 0;
}

//-*****************************************************************************
void OXformSchema::init( const AbcA::index_t iTsIdx )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OXformSchema::init()" );

    m_inheritsProperty = Abc::OBoolProperty( this->getPtr(), ".inherits",
                                     iTsIdx );

    m_animChannelsProperty = Abc::OUInt32ArrayProperty( this->getPtr(),
                                                ".animChans", iTsIdx );

    m_isIdentity = true;

    m_numOps = 0;
    m_numChannels = 0;

    ALEMBIC_ABC_SAFE_CALL_END_RESET();
}

//-*****************************************************************************
Abc::OCompoundProperty OXformSchema::getArbGeomParams()
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OXformSchema::getArbGeomParams()" );

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
Abc::OCompoundProperty OXformSchema::getUserProperties()
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OXformSchema::getUserProperties()" );

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

//-*****************************************************************************
void OXformSchema::setTimeSampling( uint32_t iIndex )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN(
        "OXformSchema::setTimeSampling( uint32_t )" );

    m_animChannelsProperty.setTimeSampling( iIndex );
    m_inheritsProperty.setTimeSampling( iIndex );

    if ( m_valsPWPtr )
    {
        if ( m_useArrayProp )
        { m_valsPWPtr->asArrayPtr()->setTimeSamplingIndex( iIndex ); }
        else
        { m_valsPWPtr->asScalarPtr()->setTimeSamplingIndex( iIndex ); }
    }

    if ( m_childBoundsProperty )
    {
        m_childBoundsProperty.setTimeSampling( iIndex );
    }

    ALEMBIC_ABC_SAFE_CALL_END();
}

//-*****************************************************************************
void OXformSchema::setTimeSampling( AbcA::TimeSamplingPtr iTime )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN(
        "OXformSchema::setTimeSampling( TimeSamplingPtr )" );

    if ( iTime )
    {
        uint32_t tsIndex = getObject().getArchive().addTimeSampling( *iTime );
        setTimeSampling( tsIndex );
    }

    ALEMBIC_ABC_SAFE_CALL_END();
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace AbcGeom
} // End namespace Alembic
