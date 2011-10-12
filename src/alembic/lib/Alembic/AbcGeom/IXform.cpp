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

#include <Alembic/AbcGeom/IXform.h>
#include <Alembic/AbcGeom/XformOp.h>

namespace Alembic {
namespace AbcGeom {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
void IXformSchema::init( Abc::SchemaInterpMatching iMatching )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "IXformSchema::init()" );

    AbcA::CompoundPropertyReaderPtr ptr = this->getPtr();

    if ( ptr->getPropertyHeader( ".childBnds" ) )
    {
        m_childBoundsProperty = Abc::IBox3dProperty( ptr, ".childBnds", iMatching );
    }

    if ( ptr->getPropertyHeader( ".inherits" ) )
    {
        m_inheritsProperty = Abc::IBoolProperty( ptr, ".inherits", iMatching );
    }

    AbcA::ScalarPropertyReaderPtr ops = ptr->getScalarProperty(  ".ops" );

    m_useArrayProp = false;

    const AbcA::PropertyHeader *valsPH = ptr->getPropertyHeader( ".vals" );
    if ( valsPH != NULL )
    {
        if ( valsPH->isScalar() )
        {
            m_valsProperty = ptr->getScalarProperty( valsPH->getName() );
        }
        else
        {
            m_useArrayProp = true;
            m_valsProperty = ptr->getArrayProperty( valsPH->getName() );
        }
    }

    m_isConstantIdentity = true;

    if ( ptr->getPropertyHeader( "isNotConstantIdentity" ) )
    {
        // that it's here at all means we're not constant identity.
        m_isConstantIdentity = false;
    }

    m_isConstant = true;

    if ( m_valsProperty )
    {

        if ( m_useArrayProp )
        { m_isConstant = m_valsProperty->asArrayPtr()->isConstant(); }
        else
        { m_isConstant = m_valsProperty->asScalarPtr()->isConstant(); }
    }

    m_isConstant = m_isConstant && m_inheritsProperty.isConstant();

    std::set < Alembic::Util::uint32_t > animChannels;

    if ( ptr->getPropertyHeader( ".animChans" ) )
    {
        Abc::IUInt32ArrayProperty p( ptr, ".animChans" );
        if ( p.getNumSamples() > 0 )
        {
            Abc::UInt32ArraySamplePtr animSamp;
            p.get( animSamp, p.getNumSamples() - 1 );
            for ( std::size_t i = 0; i < animSamp->size(); ++i )
            {
                animChannels.insert( (*animSamp)[i] );
            }
        }
    }

    if ( ops && ops->getNumSamples() > 0 )
    {

        std::size_t numOps = ops->getHeader().getDataType().getExtent();
        std::vector<Alembic::Util::uint8_t> opVec( numOps );
        ops->getSample( 0, &(opVec.front()) );

        for ( std::size_t i = 0; i < numOps; ++i )
        {
            XformOp op( opVec[i] );
            m_sample.addOp( op );
        }

        std::set < Alembic::Util::uint32_t >::iterator it, itEnd;
        std::vector< XformOp >::iterator op = m_sample.m_ops.begin();
        std::vector< XformOp >::iterator opEnd = m_sample.m_ops.end();
        std::size_t curChan = 0;
        std::size_t chanPos = 0;

        for ( it = animChannels.begin(), itEnd = animChannels.end();
            it != itEnd; ++it )
        {
            Alembic::Util::uint32_t animChan = *it;
            while ( op != opEnd )
            {
                std::size_t numChans = op->getNumChannels();
                bool foundChan = false;
                while ( curChan < numChans )
                {
                    if ( animChan == chanPos )
                    {
                        op->m_animChannels.insert( curChan );
                        foundChan = true;
                        break;
                    }

                    ++curChan;
                    ++chanPos;
                }

                // move on to the next animChan, because we found the current one
                if ( foundChan == true )
                {
                    ++curChan;
                    ++chanPos;
                    break;
                }

                ++op;
                curChan = 0;
            }
        }
    }

    if ( ptr->getPropertyHeader( ".arbGeomParams" ) != NULL )
    {
        m_arbGeomParams = Abc::ICompoundProperty( ptr, ".arbGeomParams",
                                                  getErrorHandlerPolicy()
                                                );
    }

    if ( ptr->getPropertyHeader( ".userProperties" ) != NULL )
    {
        m_userProperties = Abc::ICompoundProperty( ptr, ".userProperties",
                                                  getErrorHandlerPolicy()
                                                );
    }
    ALEMBIC_ABC_SAFE_CALL_END_RESET();
}

//-*****************************************************************************
AbcA::TimeSamplingPtr IXformSchema::getTimeSampling() const
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "IXformSchema::getTimeSampling()" );

    if ( m_inheritsProperty )
    {
        return m_inheritsProperty.getTimeSampling();
    }
    else
    {
        AbcA::TimeSamplingPtr ret;
        return ret;
    }

    ALEMBIC_ABC_SAFE_CALL_END();

    AbcA::TimeSamplingPtr ret;
    return ret;
}

//-*****************************************************************************
std::size_t IXformSchema::getNumSamples() const
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "IXformSchema::getNumSamples()" );

    if ( m_inheritsProperty )
    {
        return m_inheritsProperty.getNumSamples();
    }
    else { return 0; }

    ALEMBIC_ABC_SAFE_CALL_END();

    return 0;
}

//-*****************************************************************************
void IXformSchema::getChannelValues( const AbcA::index_t iSampleIndex,
    XformSample & oSamp ) const
{
    std::vector<Alembic::Util::float64_t> dataVec;

    if ( m_useArrayProp )
    {
        AbcA::ArraySamplePtr sptr;
        m_valsProperty->asArrayPtr()->getSample( iSampleIndex, sptr );

        dataVec.assign(
            static_cast<const Alembic::Util::float64_t*>( sptr->getData() ),
            static_cast<const Alembic::Util::float64_t*>( sptr->getData() ) +
            sptr->size() );
    }
    else
    {
        dataVec.resize( m_valsProperty->asScalarPtr()->getDataType().getExtent() );
        m_valsProperty->asScalarPtr()->getSample( iSampleIndex, &(dataVec.front()) );
    }

    std::vector< XformOp >::iterator op = oSamp.m_ops.begin();
    std::vector< XformOp >::iterator opEnd = oSamp.m_ops.end();
    std::size_t chanPos = 0;
    while ( op != opEnd )
    {
        for ( std::size_t j = 0; j < op->getNumChannels();
            ++j, ++chanPos )
        {
            op->setChannelValue( j, dataVec[chanPos] );
        }
        ++op;
    }
}

//-*****************************************************************************
void IXformSchema::get( XformSample &oSamp, const Abc::ISampleSelector &iSS ) const
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "IXformSchema::get()" );

    oSamp.reset();

    if ( ! valid() ) { return; }

    oSamp = m_sample;

    if ( m_inheritsProperty.getNumSamples() )
    {
        oSamp.setInheritsXforms( m_inheritsProperty.getValue( iSS ) );
    }

    if ( m_childBoundsProperty && m_childBoundsProperty.getNumSamples() > 0 )
    {
        oSamp.setChildBounds( m_childBoundsProperty.getValue( iSS ) );
    }

    if ( ! m_valsProperty ) { return; }

    AbcA::index_t numSamples = 0;
    if ( m_useArrayProp )
    {
        numSamples = m_valsProperty->asArrayPtr()->getNumSamples();
    }
    else
    {
        numSamples = m_valsProperty->asScalarPtr()->getNumSamples();
    }

    if ( numSamples == 0 ) { return; }

    AbcA::index_t sampIdx = iSS.getIndex( m_valsProperty->getTimeSampling(),
                                          numSamples );

    if ( sampIdx < 0 ) { return; }

    this->getChannelValues( sampIdx, oSamp );

    ALEMBIC_ABC_SAFE_CALL_END();
}

//-*****************************************************************************
XformSample IXformSchema::getValue( const Abc::ISampleSelector &iSS ) const
{
    XformSample ret;
    this->get( ret, iSS );
    return ret;
}

//-*****************************************************************************
bool IXformSchema::getInheritsXforms( const Abc::ISampleSelector &iSS ) const
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "IXformSchema::getInheritsXforms()" );

    if ( ! m_inheritsProperty ) { return true; }

    AbcA::index_t sampIdx = iSS.getIndex( m_inheritsProperty.getTimeSampling(),
                                          m_inheritsProperty.getNumSamples() );

    if ( sampIdx < 0 ) { return true; }

    return m_inheritsProperty.getValue( sampIdx );

    ALEMBIC_ABC_SAFE_CALL_END();

    return true;
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace AbcGeom
} // End namespace Alembic
