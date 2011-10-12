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

#ifndef _Alembic_AbcCoreAbstract_ScalarPropertyReader_h_
#define _Alembic_AbcCoreAbstract_ScalarPropertyReader_h_

#include <Alembic/AbcCoreAbstract/Foundation.h>
#include <Alembic/AbcCoreAbstract/BasePropertyReader.h>

namespace Alembic {
namespace AbcCoreAbstract {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
//! A Scalar Property is a Rank 0 property which has a single value for each
//! sample. This is distinguished from an Array Property, which has a
//! variable number of elements per sample, and requires more sophisticated
//! resource management.
class ScalarPropertyReader : public BasePropertyReader
{
public:
    //! Virtual destructor
    //! ...
    virtual ~ScalarPropertyReader();

    //-*************************************************************************
    // NEW FUNCTIONS
    //-*************************************************************************

    //! Return the number of samples contained in the property.
    //! This can be any number, including zero.
    //! This returns the number of samples that were written, independently
    //! of whether or not they were constant. Implementations may (and should)
    //! choose to condense identical samples.
    virtual size_t getNumSamples() = 0;

    //! Ask if we're constant - no change in value amongst samples,
    //! regardless of the time sampling.
    virtual bool isConstant() = 0;

    //! Returns the single sample value for the requested sample
    //! by reference. Out-of-range indices will cause an
    //! exception to be thrown.
    //! It will copy the scalar value directly into the memory location
    //! specified by iIntoLocation
    //!
    //! In all cases EXCEPT String and Wstring, the DataType for this
    //! property can be used to determine the size of the memory buffer
    //! which iIntoLocation must point to.  In the case of String and Wstring,
    //! iIntoLocation should be ( void * )&std::string and
    //! ( void * )&std::wstring, respectively.
    //!
    //! This is one of the only places where we break from POD types at
    //! the base, and we're making an explicit decision to use std::string
    //! and std::wstring as core language-level primitives.
    virtual void getSample( index_t iSample,
                            void *iIntoLocation ) = 0;

    //! Find the largest valid index that has a time less than or equal
    //! to the given time. Invalid to call this with zero samples.
    //! If the minimum sample time is greater than iTime, index
    //! 0 will be returned.
    virtual std::pair<index_t, chrono_t> getFloorIndex( chrono_t iTime ) = 0;

    //! Find the smallest valid index that has a time greater
    //! than the given time. Invalid to call this with zero samples.
    //! If the maximum sample time is less than iTime, index
    //! numSamples-1 will be returned.
    virtual std::pair<index_t, chrono_t> getCeilIndex( chrono_t iTime ) = 0;

    //! Find the valid index with the closest time to the given
    //! time. Invalid to call this with zero samples.
    virtual std::pair<index_t, chrono_t> getNearIndex( chrono_t iTime ) = 0;
};

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcCoreAbstract
} // End namespace Alembic



#endif
