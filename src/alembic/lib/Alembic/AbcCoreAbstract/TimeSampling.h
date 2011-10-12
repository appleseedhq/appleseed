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

#ifndef _Alembic_AbcCoreAbstract_TimeSampling_h_
#define _Alembic_AbcCoreAbstract_TimeSampling_h_

#include <Alembic/AbcCoreAbstract/Foundation.h>
#include <Alembic/AbcCoreAbstract/TimeSamplingType.h>
#include <Alembic/AbcCoreAbstract/ArraySample.h>

namespace Alembic {
namespace AbcCoreAbstract {
namespace ALEMBIC_VERSION_NS {


//-*****************************************************************************
//! The TimeSampling class's whole job is to report information about the
//! time values that are associated with the samples that were written
//! to a property. Most of the time, the sampling will be uniform or
//! cyclic, in which case this is mostly an algorithmic interface to
//! the small sample times buffer. In the case where the sampling is
//! truly acyclic, this class acts as an accessor to that array of times.
class TimeSampling
{
public:
    //! The TimeSampling class is really a portable interface, usable
    //! via aggregation, that the SimplePropertyReaders and Writers (and
    //! their derived classes) can use to provide consistent time-sampling
    //! introspection to clients.
    TimeSampling( const TimeSamplingType & iTimeSamplingType,

                  //! The number of time samples per cycle
                  const std::vector < chrono_t > & iSampleTimes );

    //! Convenience constructor which creates uniform time sampling with
    //! the specified time per cycle and the specified start time.
    TimeSampling( chrono_t iTimePerCycle, chrono_t iStartTime );

    TimeSampling( const TimeSampling &copy );

    TimeSampling();

    bool operator==( const TimeSampling & iRhs ) const
    {
        return (m_timeSamplingType == iRhs.m_timeSamplingType && 
            m_sampleTimes == iRhs.m_sampleTimes);
    }

    //! Get the number of stored times.
    //! This is same as the samples per cycle in the time sampling type except
    //! for acyclic time sampling.  There will always be at least one sample
    //! because a start time is always needed.
    size_t getNumStoredTimes() const
    {
        return m_sampleTimes.size();
    }

    const std::vector < chrono_t > & getStoredTimes() const
    {
        return m_sampleTimes;
    }

    TimeSamplingType getTimeSamplingType() const
    {
        return m_timeSamplingType;
    }

    //! Get the time of any sample
    //! it is invalid to call this for out-of-range indices.
    chrono_t getSampleTime( index_t iIndex ) const;

    //! Find the largest valid index that has a time less than or equal
    //! to the given time. Invalid to call this with zero samples.
    //! If the minimum sample time is greater than iTime, index
    //! 0 will be returned.
    std::pair<index_t, chrono_t> getFloorIndex( chrono_t iTime,
        index_t iNumSamples ) const;

    //! Find the smallest valid index that has a time greater
    //! than the given time. Invalid to call this with zero samples.
    //! If the maximum sample time is less than iTime, index
    //! numSamples-1 will be returned.
    std::pair<index_t, chrono_t> getCeilIndex( chrono_t iTime, 
        index_t iNumSamples ) const;

    //! Find the valid index with the closest time to the given
    //! time. Invalid to call this with zero samples.
    std::pair<index_t, chrono_t> getNearIndex( chrono_t iTime,
        index_t iNumSamples ) const;

protected:
    //! A TimeSamplingType
    //! This is "Uniform", "Cyclic", or "Acyclic".
    TimeSamplingType m_timeSamplingType;

    std::vector < chrono_t > m_sampleTimes;

private:
    // sanity checks the data coming in
    void init();
};

typedef boost::shared_ptr<TimeSampling> TimeSamplingPtr;

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcCoreAbstract
} // End namespace Alembic

#endif

