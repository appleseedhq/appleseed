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

#include <Alembic/AbcGeom/IPolyMesh.h>

namespace Alembic {
namespace AbcGeom {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
MeshTopologyVariance IPolyMeshSchema::getTopologyVariance() const
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "IPolyMeshSchema::getTopologyVariance()" );

    if ( m_indicesProperty.isConstant() && m_countsProperty.isConstant() )
    {
        if ( m_positionsProperty.isConstant() )
        {
            return kConstantTopology;
        }
        else
        {
            return kHomogenousTopology;
        }
    }
    else
    {
        return kHeterogenousTopology;
    }

    ALEMBIC_ABC_SAFE_CALL_END();

    // Not all error handlers throw
    return kConstantTopology;
}

//-*****************************************************************************
void IPolyMeshSchema::init( const Abc::Argument &iArg0,
                            const Abc::Argument &iArg1 )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "IPolyMeshSchema::init()" );

    Abc::Arguments args;
    iArg0.setInto( args );
    iArg1.setInto( args );

    AbcA::CompoundPropertyReaderPtr _this = this->getPtr();

    // no matching so we pick up old assets written as V3f
    m_positionsProperty = Abc::IP3fArrayProperty( _this, "P", kNoMatching );

    m_indicesProperty = Abc::IInt32ArrayProperty( _this, ".faceIndices",
                                        args.getSchemaInterpMatching() );
    m_countsProperty = Abc::IInt32ArrayProperty( _this, ".faceCounts",
                                       args.getSchemaInterpMatching() );

    // none of the things below here are guaranteed to exist
    if ( this->getPropertyHeader( "uv" ) != NULL )
    {
        m_uvsParam = IV2fGeomParam( _this, "uv", iArg0, iArg1 );
    }

    if ( this->getPropertyHeader( "N" ) != NULL )
    {
        m_normalsParam = IN3fGeomParam( _this, "N", iArg0, iArg1 );
    }


    IObject _thisObject = this->getParent().getObject();
    size_t numChildren = _thisObject.getNumChildren();
    for ( size_t childIndex = 0 ; childIndex < numChildren; childIndex++ )
    {
        ObjectHeader const & header = _thisObject.getChildHeader (childIndex);
        if ( IFaceSet::matches( header ) )
        {
            // start out with an empty (invalid IFaceSet)
            // accessor later on will create real IFaceSet object.
            m_faceSets [header.getName ()] = IFaceSet ();
        }
    }

    m_faceSetsLoaded = false;

    ALEMBIC_ABC_SAFE_CALL_END_RESET();
}

//-*****************************************************************************
const IPolyMeshSchema &
IPolyMeshSchema::operator=(const IPolyMeshSchema & rhs)
{
    IGeomBaseSchema<PolyMeshSchemaInfo>::operator=(rhs);

    m_positionsProperty = rhs.m_positionsProperty;
    m_indicesProperty   = rhs.m_indicesProperty;
    m_countsProperty    = rhs.m_countsProperty;

    m_uvsParam          = rhs.m_uvsParam;
    m_normalsParam      = rhs.m_normalsParam;

    // lock, reset
    boost::mutex::scoped_lock l(m_faceSetsMutex);
    m_faceSetsLoaded = false;
    m_faceSets.clear ();

    return *this;
}

//-*****************************************************************************
void IPolyMeshSchema::loadFaceSetNames()
{
    // Caller must ensure they have locked m_faceSetsMutex.
    // (allows us to use non-recursive mutex)
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "IPolyMeshSchema::loadFaceSetNames()" );

    if (!m_faceSetsLoaded)
    {
        // iterate over childHeaders, and if header matches
        // FaceSet add to our vec
        IObject _thisObject = this->getParent().getObject();

        size_t numChildren = _thisObject.getNumChildren();
        for ( size_t childIndex = 0 ; childIndex < numChildren; childIndex++ )
        {
            ObjectHeader const & header = _thisObject.getChildHeader (childIndex);
            if ( IFaceSet::matches( header ) )
            {
                // start out with an empty (invalid IFaceSet)
                // accessor later on will create real IFaceSet object.
                m_faceSets [header.getName ()] = IFaceSet ();
            }
        }
        m_faceSetsLoaded = true;
    }

    ALEMBIC_ABC_SAFE_CALL_END();
}

//-*****************************************************************************
void IPolyMeshSchema::getFaceSetNames( std::vector<std::string> &oFaceSetNames )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "IPolyMeshSchema::getFaceSetNames()" );

    boost::mutex::scoped_lock l(m_faceSetsMutex);
    loadFaceSetNames();

    for ( std::map<std::string, IFaceSet>::const_iterator faceSetIter =
              m_faceSets.begin(); faceSetIter != m_faceSets.end();
          ++faceSetIter )
    {
        oFaceSetNames.push_back( faceSetIter->first );
    }

    ALEMBIC_ABC_SAFE_CALL_END();
}

//-*****************************************************************************
bool
IPolyMeshSchema::hasFaceSet( const std::string &iFaceSetName )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "IPolyMeshSchema::hasFaceSet (iFaceSetName)" );

    boost::mutex::scoped_lock l(m_faceSetsMutex);
    if (!m_faceSetsLoaded)
    {
        loadFaceSetNames();
    }

    return (m_faceSets.find (iFaceSetName) != m_faceSets.end ());

    ALEMBIC_ABC_SAFE_CALL_END();

    return false;
}

//-*****************************************************************************
IFaceSet
IPolyMeshSchema::getFaceSet ( const std::string &iFaceSetName )
{
    ALEMBIC_ABC_SAFE_CALL_BEGIN( "IPolyMeshSchema::getFaceSet()" );
    boost::mutex::scoped_lock l(m_faceSetsMutex);
    if (!m_faceSetsLoaded)
    {
        loadFaceSetNames();
    }

    ABCA_ASSERT( m_faceSets.find (iFaceSetName) != m_faceSets.end (),
        "The requested FaceSet name can't be found in PolyMesh.");

    if (!m_faceSets [iFaceSetName])
    {
        // We haven't yet loaded the faceSet, so create/load it
        m_faceSets [iFaceSetName] = IFaceSet ( this->getParent().getObject(),
                                               iFaceSetName );
    }

    return m_faceSets [iFaceSetName];

    ALEMBIC_ABC_SAFE_CALL_END();

    IFaceSet emptyFaceSet;
    return emptyFaceSet;
}


} // End namespace ALEMBIC_VERSION_NS
} // End namespace AbcGeom
} // End namespace Alembic
