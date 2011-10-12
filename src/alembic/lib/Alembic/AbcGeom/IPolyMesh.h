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

#ifndef _Alembic_AbcGeom_IPolyMesh_h_
#define _Alembic_AbcGeom_IPolyMesh_h_

#include <boost/thread/mutex.hpp>
#include <Alembic/AbcGeom/Foundation.h>
#include <Alembic/AbcGeom/SchemaInfoDeclarations.h>
#include <Alembic/AbcGeom/IFaceSet.h>
#include <Alembic/AbcGeom/IGeomParam.h>
#include <Alembic/AbcGeom/IGeomBase.h>

namespace Alembic {
namespace AbcGeom {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
class IPolyMeshSchema : public IGeomBaseSchema<PolyMeshSchemaInfo>
{
public:
    class Sample
    {
    public:
        typedef Sample this_type;

        // Users don't ever create this data directly.
        Sample() { reset(); }

        Abc::P3fArraySamplePtr getPositions() const { return m_positions; }
        Abc::Int32ArraySamplePtr getFaceIndices() const { return m_indices; }
        Abc::Int32ArraySamplePtr getFaceCounts() const { return m_counts; }
        Abc::Box3d getSelfBounds() const { return m_selfBounds; }
        Abc::Box3d getChildBounds() const { return m_childBounds; }

        bool valid() const
        {
            return m_positions && m_indices && m_counts;
        }

        void reset()
        {
            m_positions.reset();
            m_indices.reset();
            m_counts.reset();

            m_selfBounds.makeEmpty();
            m_childBounds.makeEmpty();
        }

        ALEMBIC_OPERATOR_BOOL( valid() );

    protected:
        friend class IPolyMeshSchema;
        Abc::P3fArraySamplePtr m_positions;
        Abc::Int32ArraySamplePtr m_indices;
        Abc::Int32ArraySamplePtr m_counts;

        Abc::Box3d m_selfBounds;
        Abc::Box3d m_childBounds;
    };

    //-*************************************************************************
    // POLY MESH SCHEMA
    //-*************************************************************************
public:
    //! By convention we always define this_type in AbcGeom classes.
    //! Used by unspecified-bool-type conversion below
    typedef IPolyMeshSchema this_type;

    //-*************************************************************************
    // CONSTRUCTION, DESTRUCTION, ASSIGNMENT
    //-*************************************************************************

    //! The default constructor creates an empty OPolyMeshSchema
    //! ...
    IPolyMeshSchema() {}

    //! This templated, explicit function creates a new scalar property reader.
    //! The first argument is any Abc (or AbcCoreAbstract) object
    //! which can intrusively be converted to an CompoundPropertyReaderPtr
    //! to use as a parent, from which the error handler policy for
    //! inheritance is also derived.  The remaining optional arguments
    //! can be used to override the ErrorHandlerPolicy and to specify
    //! schema interpretation matching.
    template <class CPROP_PTR>
    IPolyMeshSchema( CPROP_PTR iParent,
                     const std::string &iName,

                     const Abc::Argument &iArg0 = Abc::Argument(),
                     const Abc::Argument &iArg1 = Abc::Argument() )
      : IGeomBaseSchema<PolyMeshSchemaInfo>( iParent, iName,
                                            iArg0, iArg1 )
    {
        init( iArg0, iArg1 );
    }

    //! This constructor is the same as above, but with default
    //! schema name used.
    template <class CPROP_PTR>
    explicit IPolyMeshSchema( CPROP_PTR iParent,
                              const Abc::Argument &iArg0 = Abc::Argument(),
                              const Abc::Argument &iArg1 = Abc::Argument() )
      : IGeomBaseSchema<PolyMeshSchemaInfo>( iParent,
                                            iArg0, iArg1 )
    {
        init( iArg0, iArg1 );
    }

    //! Wrap an existing schema object
    template <class CPROP_PTR>
    IPolyMeshSchema( CPROP_PTR iThis,
                     Abc::WrapExistingFlag iFlag,

                     const Abc::Argument &iArg0 = Abc::Argument(),
                     const Abc::Argument &iArg1 = Abc::Argument() )
      : IGeomBaseSchema<PolyMeshSchemaInfo>( iThis, iFlag, iArg0, iArg1 )
    {
        init( iArg0, iArg1 );
    }

    //! Copy constructor.
    IPolyMeshSchema(const IPolyMeshSchema& iCopy)
        : IGeomBaseSchema<PolyMeshSchemaInfo>()
    {
        *this = iCopy;
    }
    const IPolyMeshSchema & operator=(const IPolyMeshSchema & rhs);


    //! Return the number of samples contained in the property.
    //! This can be any number, including zero.
    //! This returns the number of samples that were written, independently
    //! of whether or not they were constant.
    size_t getNumSamples() const
    { return  m_positionsProperty.getNumSamples(); }

    //! Return the topological variance.
    //! This indicates how the mesh may change.
    MeshTopologyVariance getTopologyVariance() const;

    //! Ask if we're constant - no change in value amongst samples,
    //! regardless of the time sampling.
    bool isConstant() const { return getTopologyVariance() == kConstantTopology; }

    //! Time information.
    //! Any of the properties could be the bearer of the time
    //! sampling information, which otherwise defaults to Identity.
    AbcA::TimeSamplingPtr getTimeSampling() const
    {
        if ( m_positionsProperty.valid() )
        {
            return m_positionsProperty.getTimeSampling();
        }
        else
        {
            return getObject().getArchive().getTimeSampling( 0 );
        }
    }

    //-*************************************************************************
    void get( Sample &oSample,
              const Abc::ISampleSelector &iSS = Abc::ISampleSelector() ) const
    {
        ALEMBIC_ABC_SAFE_CALL_BEGIN( "IPolyMeshSchema::get()" );

        m_positionsProperty.get( oSample.m_positions, iSS );
        m_indicesProperty.get( oSample.m_indices, iSS );
        m_countsProperty.get( oSample.m_counts, iSS );

        m_selfBoundsProperty.get( oSample.m_selfBounds, iSS );

        if ( m_childBoundsProperty && m_childBoundsProperty.getNumSamples() > 0 )
        {
            m_childBoundsProperty.get( oSample.m_childBounds, iSS );
        }
        // Could error check here.

        ALEMBIC_ABC_SAFE_CALL_END();
    }

    Sample getValue( const Abc::ISampleSelector &iSS = Abc::ISampleSelector() ) const
    {
        Sample smp;
        get( smp, iSS );
        return smp;
    }

    IV2fGeomParam &getUVsParam() { return m_uvsParam; }
    const IV2fGeomParam &getUVsParam() const { return m_uvsParam; }

    IN3fGeomParam &getNormalsParam() { return m_normalsParam; }
    const IN3fGeomParam &getNormalsParam() const { return m_normalsParam; }

    Abc::IInt32ArrayProperty getFaceCountsProperty() const { return m_countsProperty; }
    Abc::IInt32ArrayProperty getFaceIndicesProperty() const { return m_indicesProperty; }
    Abc::IP3fArrayProperty getPositionsProperty() const { return m_positionsProperty; }

    //-*************************************************************************
    // ABC BASE MECHANISMS
    // These functions are used by Abc to deal with errors, rewrapping,
    // and so on.
    //-*************************************************************************

    //! Reset returns this function set to an empty, default
    //! state.
    void reset()
    {
        m_positionsProperty.reset();
        m_indicesProperty.reset();
        m_countsProperty.reset();

        m_uvsParam.reset();
        m_normalsParam.reset();

        IGeomBaseSchema<PolyMeshSchemaInfo>::reset();
    }

    //! Valid returns whether this function set is
    //! valid.
    bool valid() const
    {
        return ( IGeomBaseSchema<PolyMeshSchemaInfo>::valid() &&
                 m_positionsProperty.valid() &&
                 m_indicesProperty.valid() &&
                 m_countsProperty.valid() );
    }

    // FaceSet related
    //! Appends the names of any FaceSets for this PolyMesh.
    void getFaceSetNames (std::vector <std::string> & oFaceSetNames);
    IFaceSet getFaceSet( const std::string &iFaceSetName );
    bool hasFaceSet( const std::string &iFaceSetName );

    //! unspecified-bool-type operator overload.
    //! ...
    ALEMBIC_OVERRIDE_OPERATOR_BOOL( IPolyMeshSchema::valid() );

protected:
    void init( const Abc::Argument &iArg0,
               const Abc::Argument &iArg1 );

    Abc::IP3fArrayProperty m_positionsProperty;
    Abc::IInt32ArrayProperty m_indicesProperty;
    Abc::IInt32ArrayProperty m_countsProperty;

    IV2fGeomParam m_uvsParam;
    IN3fGeomParam m_normalsParam;

    // FaceSets, this starts as empty until client
    // code attempts to access facesets.
    bool                              m_faceSetsLoaded;
    std::map <std::string, IFaceSet>  m_faceSets;
    boost::mutex                      m_faceSetsMutex;
    void loadFaceSetNames();
};

//-*****************************************************************************
typedef Abc::ISchemaObject<IPolyMeshSchema> IPolyMesh;

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcGeom
} // End namespace Alembic

#endif
