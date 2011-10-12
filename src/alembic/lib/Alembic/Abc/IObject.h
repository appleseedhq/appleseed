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

#ifndef _Alembic_Abc_IObject_h_
#define _Alembic_Abc_IObject_h_

#include <Alembic/Abc/Foundation.h>
#include <Alembic/Abc/Base.h>
#include <Alembic/Abc/Argument.h>

namespace Alembic {
namespace Abc {
namespace ALEMBIC_VERSION_NS {

class IArchive;
class ICompoundProperty;

//-*****************************************************************************
class IObject : public Base
{
public:
    //! By convention, we always define "this_type" in every Abc
    //! class. This convention is relied upon by the unspecified-bool-type
    //! conversion.
    typedef IObject this_type;
    typedef IObject operator_bool_base_type;

    //-*************************************************************************
    // CONSTRUCTION, DESTRUCTION, ASSIGNMENT
    //-*************************************************************************

    //! The default constructor creates an empty IObject function set.
    //! ...
    IObject() {}

    //! This templated, explicit function creates a new object reader.
    //! The first argument is any Abc (or AbcCoreAbstract) object
    //! which can intrusively be converted to an AbcA::ObjectReaderPtr
    //! to use as a parent, from which the error handler policy for
    //! inheritance is also derived.  The remaining optional arguments
    //! can be used to override the ErrorHandlerPolicy.
    template <class OBJECT_PTR>
    IObject( OBJECT_PTR iParentObject,
             const std::string &iName,
             ErrorHandler::Policy iPcy = ErrorHandler::kThrowPolicy
           );

    //! This attaches an IObject wrapper around an existing
    //! ObjectReaderPtr, with an optional error handling policy.
    template <class OBJECT_PTR>
    IObject( OBJECT_PTR iPtr,
             WrapExistingFlag iFlag,
             ErrorHandler::Policy iPcy = ErrorHandler::kThrowPolicy )
      : m_object( GetObjectReaderPtr( iPtr ) )
    {
        // Set the error handling policy
        getErrorHandler().setPolicy(
            GetErrorHandlerPolicy( iPtr, iPcy ) );
    }

    //! This attaches an IObject wrapper around the top
    //! object of an archive.
    template <class ARCHIVE_PTR>
    IObject( ARCHIVE_PTR iPtr,
             TopFlag iFlag,
             ErrorHandler::Policy iPcy = ErrorHandler::kThrowPolicy )
    {
        // Set the error handling policy
        getErrorHandler().setPolicy(
            GetErrorHandlerPolicy( iPtr, iPcy ) );

        ALEMBIC_ABC_SAFE_CALL_BEGIN( "IObject::IObject( top )" );

        m_object = GetArchiveReaderPtr( iPtr )->getTop();

        ALEMBIC_ABC_SAFE_CALL_END_RESET();
    }

    //! Default copy constructor used
    //! Default assignment operator used.

    //! Destructor
    //! ...
    ~IObject();

    //-*************************************************************************
    // OBJECT READER FUNCTIONALITY
    //-*************************************************************************

    //! All objects have a header, which contains all the MetaData that was
    //! specified upon their creation.
    //! This function returns a constant reference to that Header.
    const AbcA::ObjectHeader &getHeader() const;

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
    const AbcA::MetaData &getMetaData() const
    { return getHeader().getMetaData(); }

    //! This function returns the object's archive, handily
    //! wrapped in an IArchive wrapper.
    IArchive getArchive();

    //! This function returns the object's parent, handily
    //! wrapped in an IObject wrapper. If the object is the top
    //! level object, the IObject returned will be NULL.
    IObject getParent();

    //! This function returns the number of child objects that
    //! this object has.
    size_t getNumChildren() const;

    //! This function returns the headers of each of the child
    //! objects that were written as children of this object.
    const AbcA::ObjectHeader & getChildHeader( size_t i ) const;

    //! Return the header of an object by name.
    //! This will return a NULL pointer if no header by that name is found.
    const AbcA::ObjectHeader *
    getChildHeader( const std::string &iName ) const;

    //! This returns the single top-level CompoundPropertyReader that exists
    //! automatically as part of the object.
    ICompoundProperty getProperties();

    //-*************************************************************************
    // ADVANCED TOOLS
    // Unless you really know why you need to be using these next few
    // functions, they're probably best left alone. The right way to create
    // an IObject is to actually call its constructor.
    //-*************************************************************************

    //! This function returns an IObject constructed from the indexed
    //! object.
    IObject getChild( size_t iChildIndex ) const;

    //! This function returns an IObject wrapped constructed from the
    //! header referenced by the name. If the child of the given name
    //! does not exist, this will fail in the same way as if the
    //! equivalent constructor was called.
    IObject getChild( const std::string &iChildName ) const;

    //-*************************************************************************
    // ABC BASE MECHANISMS
    // These functions are used by Abc to deal with errors, rewrapping,
    // and so on.
    //-*************************************************************************

    //! getPtr, as usual, returns a shared ptr to the
    //! underlying AbcCoreAbstract object, in this case the
    //! ObjectReaderPtr.
    AbcA::ObjectReaderPtr getPtr() { return m_object; }

    //! Reset returns this function set to an empty, default
    //! state.
    void reset() { m_object.reset(); Base::reset(); }

    //! Valid returns whether this function set is
    //! valid.
    bool valid() const
    {
        return ( Base::valid() && m_object );
    }

    //! The unspecified-bool-type operator casts the object to "true"
    //! if it is valid, and "false" otherwise.
    ALEMBIC_OPERATOR_BOOL( valid() );

private:
    void init( AbcA::ObjectReaderPtr iParentObject,
               const std::string &iName,
               ErrorHandler::Policy iParentPolicy,
               ErrorHandler::Policy iChildPolicy );

public:
    AbcA::ObjectReaderPtr m_object;
};

//-*****************************************************************************
inline AbcA::ObjectReaderPtr
GetObjectReaderPtr( IObject& iPrp ) { return iPrp.getPtr(); }

//-*****************************************************************************
// TEMPLATE AND INLINE FUNCTIONS
//-*****************************************************************************

template <class OBJ>
inline ErrorHandler::Policy GetErrorHandlerPolicy( OBJ iObj,
                                                   ErrorHandler::Policy iPcy )
{
    Argument arg( iPcy );
    return GetErrorHandlerPolicy( iObj, arg );
}

//-*****************************************************************************
template <class OBJECT_PTR>
inline IObject::IObject( OBJECT_PTR iParentObject,
                         const std::string &iName,
                         ErrorHandler::Policy iPcy )
{
    init( GetObjectReaderPtr( iParentObject ),
          iName,

          GetErrorHandlerPolicy( iParentObject ),
          iPcy );
}

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace Abc
} // End namespace Alembic

#endif
