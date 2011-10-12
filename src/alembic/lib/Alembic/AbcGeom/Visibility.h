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

#ifndef _Alembic_AbcGeom_Visibility_h_
#define _Alembic_AbcGeom_Visibility_h_

#include <string.h>
#include <Alembic/AbcGeom/Foundation.h>
#include <Alembic/Abc/OSchemaObject.h>



namespace Alembic {
namespace AbcGeom {
namespace ALEMBIC_VERSION_NS {

//! \brief Values for the visibility property
//! The top-compound object of AbcGeom Schema objects can
//! include an optional visibility property. 
//! The value can express
//! - isDeferrred - the property is present but whether the object
//!                 should be visible is determined by parents of the
//!                 object.
//! - isVisible -   The object should be visible.
//! - isHidden -    The object is explicitly hidden. 
enum ObjectVisibility 
{
    //! When evaluating whether this object is visible we will
    //! walk up the hierarchy of objects looking for an object that
    //! has the visibility property and is either explcitly 
    //! isHidden or is explicitly isVisible. If we reach the top of 
    //! the archive, then the object is visible.
    kVisibilityDeferred = -1,

    //! The object has been explicitly made hidden.
    kVisibilityHidden = 0,

    //! The object has been explicitly made visible.
    kVisibilityVisible = 1,
};

//! The name used for the CharProperty (Int8) that we use to 
//! store visibility.
static const std::string kVisibilityPropertyName = "visible";

typedef Abc::ICharProperty IVisibilityProperty;
typedef Abc::OCharProperty OVisibilityProperty;


// For Writer code:

//! Creates the visibility property for the indicated object. As a convention,
//! if the visibility of your object is animated, for frames that
//! the object is visible you can write kVisibilityDeferred.
OVisibilityProperty CreateVisibilityProperty( OObject & iObject, 
    uint32_t iTimeSamplingID );

//! Creates the visibility property for the indicated object. As a convention,
//! if the visibility of your object is animated, for frames that
//! the object is visible you can write kVisibilityDeferred.
OVisibilityProperty CreateVisibilityProperty( OObject & iObject, 
    AbcA::TimeSamplingPtr iTimeSampling );

// For Reader code:

//! If the object doesn't have a visibility property this will 
//! return a property that evaluates to false (aka a reset property)
IVisibilityProperty GetVisibilityProperty( IObject & schemaObject );

//! If the object doesn't have a visibility property this will 
//! return kVisibilityDeferred. This function is provided as 
//! a convenience. It's equally valid for you to call 
//! GetVisibilityProperty () and access the property's values
//! directly.
ObjectVisibility GetVisibility( IObject & schemaObject,
                                const Abc::ISampleSelector &iSS =
                                Abc::ISampleSelector () );

//! Returns true if the passed in object explicitly specifies
//! a visibility of kVisibilityHidden, or if it doesn't have
//! a visibility property or has a value of Deferred this
//! function will traverse upward through the object hierarchy
//! until finding a object that does. If the top is reached, true
//! will be returned.
bool IsAncestorInvisible( IObject schemaObject, 
                          const Abc::ISampleSelector &iSS =
                          Abc::ISampleSelector () );

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcGeom
} // End namespace Alembic

#endif
