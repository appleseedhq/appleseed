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

#include <Alembic/AbcGeom/Visibility.h>


namespace Alembic {
namespace AbcGeom {
namespace ALEMBIC_VERSION_NS {

OVisibilityProperty
CreateVisibilityProperty( OObject & iObject,
    AbcA::TimeSamplingPtr iTimeSampling )
{
    OVisibilityProperty emptyProperty;
    if ( iTimeSampling )
    {
        uint32_t iTimeSamplingID;
        iTimeSamplingID = iObject.getArchive().addTimeSampling(
            *iTimeSampling );
        return CreateVisibilityProperty( iObject, iTimeSamplingID );
    }
    return emptyProperty;
}

OVisibilityProperty
CreateVisibilityProperty( OObject & iObject, uint32_t iTimeSamplingID)
{
    // If client code calls this function twice they'll get an exception.
    // We aren't currently being nice and returning the existing property.
    OVisibilityProperty visibilityProperty( iObject.getProperties(),
                                            kVisibilityPropertyName,
                                            iTimeSamplingID );

    return visibilityProperty;
}


// Read side --------------------------------------
IVisibilityProperty
GetVisibilityProperty ( IObject & iObject )
{
    ICompoundProperty prop = iObject.getProperties();
    if ( prop.getPropertyHeader (kVisibilityPropertyName) )
    {
        IVisibilityProperty visibilityProperty ( prop,
            kVisibilityPropertyName );
        return visibilityProperty;
    }
    return IVisibilityProperty();
}

ObjectVisibility
GetVisibility( IObject & iObject,
               const Abc::ISampleSelector &iSS )
{
    IVisibilityProperty visibilityProperty;
    visibilityProperty = GetVisibilityProperty ( iObject );
    if ( ! visibilityProperty.valid() )
    {
        return kVisibilityDeferred;
    }

    int8_t rawVisibilityValue;
    rawVisibilityValue = visibilityProperty.getValue ( iSS );
    ObjectVisibility visibilityValue = ObjectVisibility ( rawVisibilityValue );
    return visibilityValue;
}


bool IsAncestorInvisible( IObject iObject,
                         const Abc::ISampleSelector &iSS )
{
    ABCA_ASSERT ( iObject,
                 "IsAncestorInvisible (): object passed in isn't valid.");

    IVisibilityProperty visibilityProperty = 
        GetVisibilityProperty ( iObject );
    ObjectVisibility visibilityValue = kVisibilityDeferred;
    if ( visibilityProperty )
    {
        int8_t rawVisibilityValue;
        rawVisibilityValue = visibilityProperty.getValue( iSS );
        visibilityValue = ObjectVisibility ( rawVisibilityValue );
    }

    IObject currentObject = iObject;
    while ( (visibilityValue == kVisibilityDeferred) )
    {
        // go up a level
        currentObject = currentObject.getParent();
        if (! currentObject )
        {
            return true;
        }

        visibilityProperty = GetVisibilityProperty ( currentObject );
        if ( visibilityProperty && visibilityProperty.valid() )
        {
            int8_t rawVisibilityValue;
            rawVisibilityValue = visibilityProperty.getValue( iSS );
            visibilityValue = ObjectVisibility ( rawVisibilityValue );
        }

        // At this point if we didn't find the visiblilty
        // property OR if the value was deferred we'll 
        // continue up a level (so only if this object
        // says hidden OR explicitly says visible do we stop.
    }

    if ( visibilityValue == kVisibilityHidden )
        return false;

    return true;
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace AbcGeom
} // End namespace Alembic

