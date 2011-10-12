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

#include <Alembic/AbcCoreAbstract/TimeSampling.h>

#include <Alembic/AbcCoreAbstract/DataType.h>

#include <ImathMath.h>

#include <limits>

#include <algorithm>

namespace Alembic {
namespace AbcCoreAbstract {
namespace ALEMBIC_VERSION_NS {

static const DataType kChrono_TDataType( kChrono_TPOD, 1 );

//! Work around the imprecision of comparing floating values.
static const chrono_t kCHRONO_EPSILON = \
    std::numeric_limits<chrono_t>::epsilon() * 32.0;

static const chrono_t kCHRONO_TOLERANCE = kCHRONO_EPSILON * 32.0;

//-*****************************************************************************
TimeSampling::TimeSampling( const TimeSamplingType &iTimeSamplingType,
                            const std::vector< chrono_t > & iSampleTimes )
  : m_timeSamplingType( iTimeSamplingType )
  , m_sampleTimes( iSampleTimes )
{
    init();
}

TimeSampling::TimeSampling( chrono_t iTimePerCycle,
                            chrono_t iStartTime )
  : m_timeSamplingType( iTimePerCycle )
{
    m_sampleTimes.resize(1);
    m_sampleTimes[0] = iStartTime;
    init();
}

void TimeSampling::init()
{
    size_t numSamples = m_sampleTimes.size();
    ABCA_ASSERT ( m_timeSamplingType.isAcyclic() || numSamples == 
        m_timeSamplingType.getNumSamplesPerCycle(),
        "Incorrect number of time samples specified, expected " << 
        m_timeSamplingType.getNumSamplesPerCycle() << ", got: " << 
        numSamples );

    // we need to check to make sure we are strictly increasing
    if ( numSamples > 1 )
    {
        chrono_t curVal = m_sampleTimes[0];
        for ( size_t i = 1; i < numSamples; ++i )
        {
            chrono_t newVal = m_sampleTimes[i];
            ABCA_ASSERT( curVal < newVal, "Sample " << i << " value: " <<
                newVal << " is not greater than the previous sample: " << 
                curVal );
            curVal = newVal;
        }

        // make sure cyclic samples fall within the time per cycle
        if ( m_timeSamplingType.isCyclic() )
        {
            curVal = m_sampleTimes[m_sampleTimes.size() - 1] - m_sampleTimes[0];
            ABCA_ASSERT ( curVal <= m_timeSamplingType.getTimePerCycle(),
                "Cyclic samples provided are greater than the time per cycle."
                " Expected: " << m_timeSamplingType.getTimePerCycle() <<
                " Found: " << curVal );
        }
    }

}

//-*****************************************************************************
TimeSampling::TimeSampling()
  : m_timeSamplingType( TimeSamplingType() )
{
    m_sampleTimes.resize(1);
    m_sampleTimes[0] = 0.0;
}

//-*****************************************************************************
TimeSampling::TimeSampling( const TimeSampling & copy)
  : m_timeSamplingType( copy.m_timeSamplingType )
  , m_sampleTimes( copy.m_sampleTimes )
{
    // nothing else
}

//-*****************************************************************************
chrono_t TimeSampling::getSampleTime( index_t iIndex ) const
{
    if ( m_timeSamplingType.isUniform() )
    {
        return m_sampleTimes[0] +
            ( m_timeSamplingType.getTimePerCycle() * ( chrono_t )iIndex );
    }
    else if ( m_timeSamplingType.isAcyclic() )
    {
        ABCA_ASSERT( ( size_t )iIndex < m_sampleTimes.size(),
            "Out-of-range acyclic index: " << iIndex
            << ", range [0-" <<  m_sampleTimes.size() << "]" );
        return m_sampleTimes[iIndex];
    }
    else
    {
        ABCA_ASSERT( m_timeSamplingType.isCyclic(), "should be cyclic" );

        // CJH: Yes, I know I could mod this. Being pedantic.
        const size_t N = m_timeSamplingType.getNumSamplesPerCycle();
        const size_t numCycles = iIndex / N;
        const size_t cycleBlock = numCycles * N;
        assert( ( index_t )cycleBlock <= iIndex );
        const size_t rem = iIndex - cycleBlock;
        assert( rem < N );

        return m_sampleTimes[rem] +
            ( m_timeSamplingType.getTimePerCycle() * ( chrono_t )numCycles );
    }
}

//-*****************************************************************************
std::pair<index_t, chrono_t>
TimeSampling::getFloorIndex( chrono_t iTime, index_t iNumSamples ) const
{
    //! Return the index of the sampled time that is <= iTime
    iTime += kCHRONO_EPSILON;

    const chrono_t minTime = this->getSampleTime( 0 );
    if ( iTime <= minTime )
    {
        return std::pair<index_t, chrono_t>( 0, minTime );
    }

    const chrono_t maxTime = this->getSampleTime( iNumSamples - 1 );
    if ( iTime >= maxTime )
    {
        return std::pair<index_t, chrono_t>( iNumSamples-1, maxTime );
    }

    if ( m_timeSamplingType.isAcyclic() )
    {
        // For now, just loop since we need the index and time
        // Later, use binary search of sorted array.

        assert( iTime >= minTime );
        chrono_t prevTime = minTime;
        index_t numSamples = m_sampleTimes.size();

        for ( index_t idx = 1; idx < numSamples; ++idx )
        {
            chrono_t thisTime = m_sampleTimes[idx];
            assert( iTime >= prevTime );
            if ( iTime < thisTime )
            {
                // Okay, tIdx is greater than us.
                // iIdx-1 is less than or equal to us.
                // Yes, inefficient here too, will optimize later.
                return std::pair<index_t, chrono_t>( idx-1,
                                                     prevTime );
            }

            prevTime = thisTime;
        }

        // Dang, we got all the way to the end, and no thing was ever
        // greater than us.
        // This is troublesome, because we should have picked it up
        // on maxTime.
        ABCA_THROW( "Corrupt acyclic time samples, iTime = "
                    << iTime << ", maxTime = " << maxTime );
        return std::pair<index_t, chrono_t>( m_sampleTimes.size()-1, maxTime );
    }
    else if ( m_timeSamplingType.isUniform() )
    {
        // Get sample index
        size_t sampIdx = ( size_t )floor( ( iTime - minTime ) /
                                          m_timeSamplingType.getTimePerCycle() );

        // Clamp it.
        sampIdx = ( ( index_t )(sampIdx) >= iNumSamples ) ? 
                    iNumSamples-1 : sampIdx;

        // Get the samp time again.
        chrono_t sampTime = minTime +
            ( m_timeSamplingType.getTimePerCycle() * sampIdx );

        // Because of roundoff error, this sampTime could actually
        // be greater than the given time.
        if ( sampTime > iTime )
        {
            assert( sampIdx > 0 );
            sampIdx -= 1;
            sampTime -= ( m_timeSamplingType.getTimePerCycle() );
            assert( sampTime < iTime );
        }

        return std::pair<index_t, chrono_t>( sampIdx, sampTime );
    }
    else
    {
        ABCA_ASSERT( m_timeSamplingType.isCyclic(), "should be cyclic" );

        const size_t N = m_timeSamplingType.getNumSamplesPerCycle();
        const chrono_t period = m_timeSamplingType.getTimePerCycle();
        const chrono_t elapsedTime = iTime - minTime;

        double rawNumCycles = elapsedTime / period;
        double rawNumCyclesIntregal;
        double rawNumCyclesFractional = modf( rawNumCycles,
                                              &rawNumCyclesIntregal );

        if ( Imath::equalWithAbsError( 1.0 - rawNumCyclesFractional, 0.0,
                                       kCHRONO_TOLERANCE ) )
        {
            rawNumCyclesIntregal += 1;
        }

        const size_t numCycles = ( size_t )rawNumCyclesIntregal;
        const chrono_t cycleBlockTime = ( numCycles * period );


        assert( elapsedTime >= cycleBlockTime ||
                Imath::equalWithAbsError( cycleBlockTime - elapsedTime, 0.0,
                                          kCHRONO_TOLERANCE ) );

        const chrono_t rem = iTime - cycleBlockTime;

        assert( rem < period + minTime );
        const size_t cycleBlockIndex = N * numCycles;

        index_t sampIdx = 0;

        for ( index_t i = 0 ; i < ( index_t )N ; ++i )
        {
            chrono_t sampleTime = m_sampleTimes[i];

            if ( sampleTime > rem )
            {
                sampIdx = i - 1;
                break;
            }

            sampIdx = i;
        }

        if ( sampIdx < 0 ) { sampIdx = 0; }

        const chrono_t sampTime = cycleBlockTime +
            m_sampleTimes[sampIdx];

        return std::pair<index_t, chrono_t>( cycleBlockIndex + sampIdx,
                                             sampTime );
    }
}

//-*****************************************************************************
static std::pair<index_t, chrono_t>
getCeilIndexHelper( const TimeSampling *iThat, const chrono_t iTime,
                    const size_t iFloorIndex, const chrono_t iFloorTime,
                    const size_t iMaxIndex )
{
    if ( iFloorIndex == iMaxIndex ||
         Imath::equalWithAbsError( iFloorTime, iTime,
                                   kCHRONO_TOLERANCE ) )
    {
        return std::pair<index_t, chrono_t>( iFloorIndex, iFloorTime );
    }

    assert( iFloorIndex < iMaxIndex );
    chrono_t ceilTime = iThat->getSampleTime( iFloorIndex + 1 );
    assert( ceilTime >= iTime );
    return std::pair<index_t, chrono_t>( iFloorIndex + 1, ceilTime );
}

//-*****************************************************************************
std::pair<index_t, chrono_t>
TimeSampling::getCeilIndex( chrono_t iTime, index_t iNumSamples ) const
{
    //! Return the index of the sampled time that is >= iTime

    if ( iNumSamples < 1 ) { return std::pair<index_t, chrono_t>( 0, 0.0 ); }

    iTime -= kCHRONO_EPSILON;

    const index_t _maxind = iNumSamples - 1;
    const size_t maxIndex = _maxind > -1 ? _maxind : 0;

    const chrono_t minTime = this->getSampleTime( 0 );
    if ( iTime <= minTime )
    {
        return std::pair<index_t, chrono_t>( 0, minTime );
    }

    const chrono_t maxTime = this->getSampleTime( maxIndex );
    if ( iTime >= maxTime )
    {
        return std::pair<index_t, chrono_t>( maxIndex, maxTime );
    }

    std::pair<index_t, chrono_t> floorPair = this->getFloorIndex( iTime,
        iNumSamples );

    return getCeilIndexHelper( this, iTime, floorPair.first, floorPair.second,
                               maxIndex );
}

//-*****************************************************************************
std::pair<index_t, chrono_t>
TimeSampling::getNearIndex( chrono_t iTime, index_t iNumSamples ) const
{
    //! Return the index of the sampled time that is:
    //! (iTime - floorTime < ceilTime - iTime) ? getFloorIndex( iTime )
    //! : getCeilIndex( iTime );

    if ( iNumSamples < 1 ) { return std::pair<index_t, chrono_t>( 0, 0.0 ); }

    const index_t _maxind = iNumSamples - 1;
    const size_t maxIndex = _maxind > -1 ? _maxind : 0;

    const chrono_t minTime = this->getSampleTime( 0 );
    if ( iTime <= minTime )
    {
        return std::pair<index_t, chrono_t>( 0, minTime );
    }

    const chrono_t maxTime = this->getSampleTime( maxIndex );
    if ( iTime >= maxTime )
    {
        return std::pair<index_t, chrono_t>( maxIndex, maxTime );
    }

    std::pair<index_t, chrono_t> floorPair =
        this->getFloorIndex( iTime, iNumSamples );
    std::pair<index_t, chrono_t> ceilPair =
        this->getCeilIndex( iTime, iNumSamples );

    assert( ( floorPair.second <= iTime ||
              Imath::equalWithAbsError( iTime, floorPair.second,
                                        kCHRONO_TOLERANCE ) ) &&
            ( iTime <= ceilPair.second ||
              Imath::equalWithAbsError( iTime, ceilPair.second,
                                        kCHRONO_TOLERANCE ) ) );

    chrono_t deltaFloor = fabs( iTime - floorPair.second );
    chrono_t deltaCeil = fabs( ceilPair.second - iTime );

    if ( deltaFloor <= deltaCeil ) { return floorPair; }
    else { return ceilPair; }
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace AbcCoreAbstract
} // End namespace Alembic
