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

#ifndef _Alembic_Abc_OBaseProperty_h_
#define _Alembic_Abc_OBaseProperty_h_

#include <Alembic/Abc/Foundation.h>
#include <Alembic/Abc/Base.h>
#include <Alembic/Abc/Argument.h>
#include <Alembic/Abc/OObject.h>
#include <Alembic/Abc/OArchive.h>

namespace Alembic {
namespace Abc {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
//! Most of the functionality of properties (getting information about the
//! properties and so on) is common to all property types, so we create
//! a base class to contain all that functionality.
//! This is purely a base class for other properties to derive from,
//! it will never be created directly.
template <class PROP_PTR>
class OBasePropertyT : public Base
{
public:
    //! By convention we always define this_type in Abc classes
    //! Used by unspecified-bool-type conversion below
    typedef OBasePropertyT<PROP_PTR> this_type;
    typedef OBasePropertyT<PROP_PTR> operator_bool_base_type;

protected:
    friend class OCompoundProperty;

    //-*************************************************************************
    // CONSTRUCTION, DESTRUCTION, ASSIGNMENT
    //-*************************************************************************

    //! The default constructor creates an empty OBaseProperty function set.
    //! ...
    OBasePropertyT() {}

    //! This attaches an OBaseProperty wrapper around an existing
    //! PROP_PTR, with the given error handler policy
    OBasePropertyT(

        //! The pointer
        //! ...
        PROP_PTR iPtr,

        //! The flag indicating that the intention
        //! is to wrap the existing pointer.
        WrapExistingFlag iWrapFlag,

        //! The error handling.
        //! ...
        ErrorHandler::Policy iPolicy );

    //! Default copy constructor used
    //! Default assignment operator used.

public:
    //-*************************************************************************
    // PROPERTY WRITER FUNCTIONALITY
    //-*************************************************************************

    //! Return the property's header.
    //! ...
    const AbcA::PropertyHeader & getHeader() const;

    //! This function returns the property's local name
    //! ...
    const std::string &getName() const
    { return getHeader().getName(); }

    //! This function returns the property's type
    //! ...
    AbcA::PropertyType getPropertyType() const
    { return getHeader().getPropertyType(); }

    //! Convenience to return whether the property is scalar.
    //! Same as getPropertyType() == kScalarProperty
    bool isScalar() const { return getPropertyType() == AbcA::kScalarProperty; }

    //! Convenience to return whether the property is array.
    //! Same as getPropertyType() == kArrayProperty
    bool isArray() const { return getPropertyType() == AbcA::kArrayProperty; }

    //! Convenience to return whether the property is compound.
    //! Same as getPropertyType() == kCompoundProperty
    bool isCompound() const { return getPropertyType() ==
            AbcA::kCompoundProperty; }

    //! Convenience to return whether the property is simple (non-compound)
    //! Same as getPropertyType() != kCompoundProperty
    bool isSimple() const { return !isCompound(); }

    //! All properties have MetaData. This just returns the
    //! MetaData portion of the header that was used in creation.
    const AbcA::MetaData &getMetaData() const
    { return getHeader().getMetaData(); }

    //! Non-compound properties have a DataType. It is an error
    //! to call this function for CompoundProperties, and an exception will
    //! be thrown. This is a convenience function which just returns the
    //! DataType from the header that was used in creation.
    const AbcA::DataType &getDataType() const
    { return getHeader().getDataType(); }

    //! Non-compound properties have a TimeSamplingPtr. It is an error
    //! to call this function for CompoundProperties, and an exception will
    //! be thrown. This is a convenience function which just returns the
    //! TimeSamplingPtr from the header that was used in creation.
    AbcA::TimeSamplingPtr getTimeSampling() const
    { return getHeader().getTimeSampling(); }

    //! This function returns the property's object, handily
    //! wrapped in an OObject wrapper.
    OObject getObject();

    //! Can't wrap
    //! OCompoundProperty getParent();

    //-*************************************************************************
    // ABC BASE MECHANISMS
    // These functions are used by Abc to deal with errors, rewrapping,
    // and so on.
    //-*************************************************************************

    //! getPtr, as usual, returns a shared ptr to the
    //! underlying AbcCoreAbstract object, in this case the
    //! PROP_PTR.
    PROP_PTR getPtr() { return m_property; }

    //! Reset returns this function set to an empty, default
    //! state.
    void reset() { m_property.reset(); Base::reset(); }

    //! Valid returns whether this function set is
    //! valid.
    bool valid() const
    {
        return ( Base::valid() && m_property );
    }

    //! The unspecified-bool-type operator casts the object to "true"
    //! if it is valid, and "false" otherwise.
    ALEMBIC_OPERATOR_BOOL( valid() );

protected:
    PROP_PTR m_property;
};

//-*****************************************************************************
typedef OBasePropertyT<AbcA::BasePropertyWriterPtr> OBaseProperty;

//-*****************************************************************************
// TEMPLATE AND INLINE FUNCTIONS
//-*****************************************************************************

//-*****************************************************************************
template <class PROP_PTR>
inline OBasePropertyT<PROP_PTR>::OBasePropertyT
(
    PROP_PTR iPtr,
    WrapExistingFlag,
    ErrorHandler::Policy iPolicy )
  : m_property( iPtr )
{
    getErrorHandler().setPolicy( iPolicy );
}

//-*****************************************************************************
//-*****************************************************************************
//-*****************************************************************************
template <class PROP_PTR>
const AbcA::PropertyHeader &OBasePropertyT<PROP_PTR>::getHeader() const
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OBaseProperty::getHeader()" );

    return m_property->getHeader();

    ALEMBIC_ABC_SAFE_CALL_END();

    // Not all error handlers throw, so have a default behavior.
    static const AbcA::PropertyHeader phd;
    return phd;
};

//-*****************************************************************************
template <class PROP_PTR>
OObject OBasePropertyT<PROP_PTR>::getObject()
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "OBaseProperty::getObject()" );

    return OObject( m_property->getObject(),
                    kWrapExisting,
                    getErrorHandlerPolicy() );

    ALEMBIC_ABC_SAFE_CALL_END();

    // Not all error handlers throw. Have a default.
    return OObject();
}

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace Abc
} // End namespace Alembic

#endif
