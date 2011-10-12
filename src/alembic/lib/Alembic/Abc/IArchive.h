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

#ifndef _Alembic_Abc_IArchive_h_
#define _Alembic_Abc_IArchive_h_

#include <Alembic/Abc/Foundation.h>
#include <Alembic/Abc/Base.h>
#include <Alembic/Abc/Argument.h>

namespace Alembic {
namespace Abc {
namespace ALEMBIC_VERSION_NS {

class IObject;

//-*****************************************************************************
class IArchive : public Base
{
public:
    //! By convention, we always define "this_type" in every Abc
    //! class. This convention is relied upon by the unspecified-bool-type
    //! cast
    typedef IArchive this_type;

    //-*************************************************************************
    // CONSTRUCTION, DESTRUCTION, ASSIGNMENT
    //-*************************************************************************

    //! The default constructor creates an empty IArchive function set.
    //! ...
    IArchive() {}

    //! The explicit constructor opens an existing archive with the
    //! given file name. Additional arguments that may be passed are the
    //! error handler policy and a pointer to a cache instance. By
    //! default, an archive-local cache will be created.
    template <class ARCHIVE_CTOR>
    IArchive(
        //! We need to pass in a constructor which provides
        //! an explicit link to the concrete implementation of
        //! AbcCoreAbstract that we're using.
        ARCHIVE_CTOR iCtor,

        //! The file name.
        const std::string &iFileName,

        ErrorHandler::Policy iPolicy = ErrorHandler::kThrowPolicy,
        AbcA::ReadArraySampleCachePtr iCachePtr = AbcA::ReadArraySampleCachePtr());

    //! This attaches an IArchive wrapper around an existing
    //! ArchiveReaderPtr, with an optional error handling policy.
    IArchive(

        //! The pointer
        //! ...
        AbcA::ArchiveReaderPtr iPtr,

        //! Wrap existing
        //! ...
        WrapExistingFlag iWrap,

        //! Optional error handling policy
        //! ...
        ErrorHandler::Policy iPolicy = ErrorHandler::kThrowPolicy )
      : m_archive( iPtr )
    {
        // Set the error handling policy.
        getErrorHandler().setPolicy( iPolicy );
    }

    //! Destructor
    //! ...
    ~IArchive();

    //! Default copy constructor
    //! Default assignment operator

    //-*************************************************************************
    // ARCHIVE WRITER FUNCTIONALITY
    //-*************************************************************************

    //! Returns the file name.
    //! It is an error to do so with an invalid object.
    std::string getName() const;

    //! This returns the single top-level IObject that exists
    //! automatically as part of the archive.
    IObject getTop();

    //! Get the read array sample cache. It may be a NULL pointer.
    //! Caches can be shared amongst separate archives, and caching
    //! will is disabled if a NULL cache is returned here.
    AbcA::ReadArraySampleCachePtr getReadArraySampleCachePtr();

    //! Set the read array sample cache. It may also be a NULL pointer.
    //! Caches can be shared amongst separate archives, and caching
    //! will be disabled if a NULL cache is passed here.
    void setReadArraySampleCachePtr( AbcA::ReadArraySampleCachePtr iPtr );

    //-*************************************************************************
    // ABC BASE MECHANISMS
    // These functions are used by Abc to deal with errors, rewrapping,
    // and so on.
    //-*************************************************************************

    //! getPtr, as usual, returns a shared ptr to the
    //! underlying AbcCoreAbstract object, in this case the
    //! ArchiveReaderPtr.
    AbcA::ArchiveReaderPtr getPtr() { return m_archive; }

    //! Reset returns this function et to an empty, default
    //! state.
    void reset() { m_archive.reset(); Base::reset(); }

    //! Returns the TimeSampling at a given index.
    AbcA::TimeSamplingPtr getTimeSampling( uint32_t iIndex );

    //! Returns the total number of TimeSamplingPtrs in the Archive
    //! TimeSampling pool.
    uint32_t getNumTimeSamplings();

    //! Valid returns whether this function set is
    //! valid.
    bool valid() const
    {
        return ( Base::valid() && m_archive );
    }

    //! Returns the Alembic library numeric version (see Foundation.h)
    //! of this archive file.
    int32_t getArchiveVersion();

    //! The unspecified-bool-type operator casts the object to "true"
    //! if it is valid, and "false" otherwise.
    ALEMBIC_OPERATOR_BOOL( valid() );

private:
    AbcA::ArchiveReaderPtr m_archive;
};

//-*****************************************************************************
//! This function is used by the intrusive templated constructors
//! for IObjects.
inline AbcA::ArchiveReaderPtr
GetArchiveReaderPtr( IArchive& iPrp ) { return iPrp.getPtr(); }

//-*****************************************************************************
//-*****************************************************************************
template <class ARCHIVE_CTOR>
IArchive::IArchive( ARCHIVE_CTOR iCtor,
                     const std::string &iFileName,
                     ErrorHandler::Policy iPolicy,
                     AbcA::ReadArraySampleCachePtr iCachePtr )
{
    // Set the error handling policy.
    getErrorHandler().setPolicy( iPolicy );

    ALEMBIC_ABC_SAFE_CALL_BEGIN( "IArchive::IArchive( iFileName )" );

    m_archive = iCtor( iFileName, iCachePtr );

    ALEMBIC_ABC_SAFE_CALL_END_RESET();

}

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace Abc
} // End namespace Alembic



#endif
