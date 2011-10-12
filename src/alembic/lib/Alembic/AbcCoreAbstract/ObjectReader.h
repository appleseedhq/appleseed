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

#ifndef _Alembic_AbcCoreAbstract_ObjectReader_h_
#define _Alembic_AbcCoreAbstract_ObjectReader_h_

#include <Alembic/AbcCoreAbstract/Foundation.h>
#include <Alembic/AbcCoreAbstract/ForwardDeclarations.h>
#include <Alembic/AbcCoreAbstract/ObjectHeader.h>

namespace Alembic {
namespace AbcCoreAbstract {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
//! An Object consists of a list of children objects, which may be empty,
//! and a single compound property which is the root of any properties
//! which the object may contain.
//! Objects have MetaData, which is identical to the MetaData of the root
//! Compound Property.
class ObjectReader
    : private boost::noncopyable
{
public:
    //! Virtual destructor
    //! ...
    virtual ~ObjectReader();

    //-*************************************************************************
    // NEW FUNCTIONS
    //-*************************************************************************

    //! All objects have a header, which contains all the MetaData that was
    //! specified upon their creation.
    //! This function returns a constant reference to that Header.
    virtual const ObjectHeader &getHeader() const = 0;

    //! All objects have a name. This name is unique amongst their siblings
    //! Returned by reference, since it is guaranteed to exist and be
    //! unchanging.
    //! This is a convenience function which returns the header's name.
    const std::string &getName() const
    { return getHeader().getName(); }
    
    //! The full name of an object is the complete path name all the way
    //! to the root object of the archive. It is guaranteed to be fully
    //! unique within the entire archive.
    //! This is a convenience function which returns the header's full name.
    const std::string &getFullName() const
    { return getHeader().getFullName(); }

    //! All objects have metadata. This metadata is identical to the
    //! Metadata of the top level compoundProperty "properties".
    //! Because the metadata must exist and be initialized in order to
    //! bootstrap the object, it is guaranteed to exist and is returned
    //! by reference.
    //! This is a convenience function which returns the header's MetaData.
    const MetaData &getMetaData() const
    { return getHeader().getMetaData(); }

    //! All objects have a shared link to the root. This may seem
    //! wasteful, but it is essential in order to allow for the flexible,
    //! reference-counted autonomy of the reader objects. Alembic
    //! allows you to keep references to readers wherever you want,
    //! without requiring you to keep track of (or store) the parental
    //! hierarchy directly. In order to make this possible, we have
    //! the ability to walk upwards. This may be stored as a direct
    //! link, or retrieved by walking up the parent chain, which is
    //! a feature of the individual implementations. (it might not be
    //! cheap, basically).
    //! In order to prevent shared_ptr cycles, it is important
    //! that objects only store their children via weak ptrs.
    virtual ArchiveReaderPtr getArchive() = 0;

    //! All objects have a shared link to their parent. This may seem
    //! wasteful, but it is essential in order to allow for the flexible,
    //! reference-counted autonomy of the reader objects. Alembic
    //! allows you to keep references to readers wherever you want,
    //! without requiring you to keep track of (or store) the parental
    //! hierarchy directly. In order to make this possible, we have
    //! the ability to walk upwards.
    //! In order to prevent shared_ptr cycles, it is important
    //! that objects only store their children via weak ptrs.
    virtual ObjectReaderPtr getParent() = 0;
    
    //! All objects have one and only one compound property which
    //! is the root for any properties which are associated with this object.
    //! If no properties were written to the object, this may return an
    //! empty pointer.
    virtual CompoundPropertyReaderPtr getProperties() = 0;

    //-*************************************************************************
    // Children!
    //-*************************************************************************

    //! Returns the number of objects that are contained as children.
    //! Objects do not have to have children, this may return zero.
    virtual size_t getNumChildren() = 0;

    //! Return the header of an object by index.
    //! This will throw an exception on out-of-range access.
    virtual const ObjectHeader & getChildHeader( size_t i ) = 0;

    //! Return the header of an object by name.
    //! This will return a NULL pointer if no header by that name is found.
    virtual const ObjectHeader *
    getChildHeader( const std::string &iName ) = 0;
    
    //! Get a child object by name.
    //! This is a convenience function that uses getChildHeader and
    //! the various named "get" functions here.
    virtual ObjectReaderPtr getChild( const std::string &iName ) = 0;

    //! Get a base property by index.
    //! It is an error to call with out-of-range indices.
    //! This is a convenience function that uses getChildHeader and
    //! the various named "get" functions here.
    ObjectReaderPtr getChild( size_t i );

    //-*************************************************************************
    // YUP
    //-*************************************************************************
    
    //! Returns shared pointer to myself.
    //! This is non-virtual
    virtual ObjectReaderPtr asObjectPtr() = 0;
};

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcCoreAbstract
} // End namespace Alembic

#endif


