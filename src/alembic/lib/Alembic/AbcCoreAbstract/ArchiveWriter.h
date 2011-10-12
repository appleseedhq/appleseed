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

#ifndef _Alembic_AbcCoreAbstract_ArchiveWriter_h_
#define _Alembic_AbcCoreAbstract_ArchiveWriter_h_

#include <Alembic/AbcCoreAbstract/Foundation.h>
#include <Alembic/AbcCoreAbstract/MetaData.h>
#include <Alembic/AbcCoreAbstract/ForwardDeclarations.h>

namespace Alembic {
namespace AbcCoreAbstract {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
//! In order for an implementation to concretely provide a starting point
//! for using the abstract API, a single "explicitly linked" function to
//! start from is necessary. That function will return, in the case of writing,
//! an ArchiveWriterPtr. This typedef provides a signature that such a function
//! should adhere to. This idiom is a workaround for the lack of virtual
//! constructors in C++, and to avoid creating another class (such as
//! "Implementation" or something similar)
namespace IllustrationOnly {
struct ArchiveWriterConstructor
{
    // Create an archive writer with the given file name and meta data.
    ArchiveWriterPtr operator()( const std::string &iFileName,
                                 const MetaData &iMetaData );
};
} // End namespace IllustrationOnly

//-*****************************************************************************
//! The Archive is "the file". It has a single object, it's top object.
//! It has no properties, but does have metadata.
class ArchiveWriter
    : private boost::noncopyable
{
protected:
    ArchiveWriter()
      : m_compressionHint( -1 ) {}
    
public:
    //! Virtual destructor
    //! ...
    virtual ~ArchiveWriter();

    //-*************************************************************************
    // NEW FUNCTIONS
    //-*************************************************************************
    
    //! Return the archive (file) name. This is the name of the file
    //! which the archive writer is associated with.
    virtual const std::string &getName() const = 0;
    
    //! The meta data of the archive is the same as the meta data
    //! of the top-level object writer.
    virtual const MetaData &getMetaData() const = 0;

    //! Get the top object writer.
    //! Archives always have this single top-level object created
    //! automatically.
    virtual ObjectWriterPtr getTop() = 0;

    //! Get the compression applied to array properties.
    //! Implementations are free to disregard this.
    int8_t getCompressionHint() const { return m_compressionHint; }

    //! Set the compression applied to array properties.
    //! Implementations are free to disregard this.
    //! -1 means off, 0 means "litte", where 9 means "alot"
    void setCompressionHint( int8_t iCh )
    {
        m_compressionHint = iCh < -1 ? -1 :
            ( iCh > 9 ? 9 : iCh );
    }

    //! Return self
    //! May sometimes be spoofed.
    virtual ArchiveWriterPtr asArchivePtr() = 0;

    //! Adds the TimeSampling to the Archive TimeSampling pool.
    //! If the TimeSampling already exists in the pool, the index for the match
    //! should be returned.
    //! index 0 is automatically reserved for uniform time sampling with a start
    //! time of 0 and time per cycle of 1 (aka identity sampling)
    virtual uint32_t addTimeSampling( const TimeSampling & iTs ) = 0;

    //! Returns the TimeSampling at a given index.
    //! index 0 is automatically reserved for uniform time sampling with a start
    //! time of 0 and time per cycle of 1 (aka identity sampling)
    virtual TimeSamplingPtr getTimeSampling( uint32_t iIndex ) = 0;

    //! Returns the total number of TimeSamplingPtrs in the Archive
    //! TimeSampling pool.
    virtual uint32_t getNumTimeSamplings() = 0;

private:
    int8_t m_compressionHint;
};

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcCoreAbstract
} // End namespace Alembic

#endif

