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

#include <Alembic/AbcGeom/IFaceSet.h>

namespace Alembic {
namespace AbcGeom {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
size_t IFaceSetSchema::getNumSamples()
{
    size_t max = 0;

    for ( size_t i = 0 ; i < this->getNumProperties() ; i++ )
    {
        const AbcA::PropertyHeader& ph = this->getPropertyHeader( i );
        if ( ph.isArray() )
        {
            max = std::max( max,
                            Abc::IArrayProperty(
                                this->getPtr(),
                                ph.getName() ).getNumSamples() );
        }
        else if ( ph.isScalar() )
        {
            max = std::max( max,
                            Abc::IScalarProperty(
                                this->getPtr(),
                                ph.getName() ).getNumSamples() );
        }
    }

    return max;
}

//-*****************************************************************************
void IFaceSetSchema::get( IFaceSetSchema::Sample &oSample,
                       const Abc::ISampleSelector &iSS )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "IFaceSetSchema::get()" );

    m_facesProperty.get( oSample.m_faces, iSS );

    // HERE NYI - templated getFaceIndices
    //m_selfBoundsProperty.get( oSample.m_selfBounds, iSS );

    if ( m_childBoundsProperty )
    {
        if ( m_childBoundsProperty.getNumSamples() > 0 )
        {
            m_childBoundsProperty.get( oSample.m_childBounds, iSS );
        }
    }

    ALEMBIC_ABC_SAFE_CALL_END();
}

//-*****************************************************************************
void IFaceSetSchema::init( const Abc::Argument &iArg0,
                        const Abc::Argument &iArg1 )
{
    // Only callable by ctors (mt-safety)
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "IFaceSetSchema::init()" );

    Abc::Arguments args;
    iArg0.setInto( args );
    iArg1.setInto( args );

    AbcA::CompoundPropertyReaderPtr _this = this->getPtr();

    m_facesProperty = Abc::IInt32ArrayProperty( _this, ".faces",
        args.getSchemaInterpMatching() );

    ALEMBIC_ABC_SAFE_CALL_END_RESET();
}

//-*****************************************************************************
FaceSetExclusivity IFaceSetSchema::getFaceExclusivity()
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "IFaceSetSchema::getFaceExclusivity" );

    Abc::IUInt32Property facesExclusiveProperty( this->getPtr(),
        ".facesExclusive");
    if ( facesExclusiveProperty )
    {
        size_t numSamples = facesExclusiveProperty.getNumSamples();
        uint32_t asInt = FaceSetExclusivity( 
            facesExclusiveProperty.getValue( numSamples - 1 ) );
        FaceSetExclusivity exclusivity  = FaceSetExclusivity( asInt );
        return exclusivity;
    }

    ALEMBIC_ABC_SAFE_CALL_END();

    // If no property was written we know the FaceSet has the default value
    // for exclusivity - NonExclusive.
    return kFaceSetNonExclusive;
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace AbcGeom
} // End namespace Alembic
