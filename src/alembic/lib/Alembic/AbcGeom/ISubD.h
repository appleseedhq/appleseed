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

#ifndef _Alembic_AbcGeom_ISubD_h_
#define _Alembic_AbcGeom_ISubD_h_

#include <boost/thread/mutex.hpp>
#include <Alembic/AbcGeom/Foundation.h>
#include <Alembic/AbcGeom/SchemaInfoDeclarations.h>
#include <Alembic/AbcGeom/IGeomParam.h>
#include <Alembic/AbcGeom/IFaceSet.h>
#include <Alembic/AbcGeom/IGeomBase.h>

namespace Alembic {
namespace AbcGeom {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
class ISubDSchema : public IGeomBaseSchema<SubDSchemaInfo>
{
public:
    //-*************************************************************************
    // SUBD SCHEMA SAMPLE TYPE
    //-*************************************************************************
    class Sample
    {
    public:
        typedef Sample this_type;

        //! Users never create this data directly
        Sample() { reset(); }

        // main stuff
        Abc::P3fArraySamplePtr getPositions() const { return m_positions; }
        Abc::Int32ArraySamplePtr getFaceIndices() const { return m_faceIndices; }
        Abc::Int32ArraySamplePtr getFaceCounts() const { return m_faceCounts; }

        // misc subd stuff
        int32_t getFaceVaryingInterpolateBoundary() const
        { return m_faceVaryingInterpolateBoundary; }

        int32_t getFaceVaryingPropagateCorners() const
        { return m_faceVaryingPropagateCorners; }

        int32_t getInterpolateBoundary() const
        { return m_interpolateBoundary; }

        // creases
        Abc::Int32ArraySamplePtr getCreaseIndices() const
        { return m_creaseIndices; }

        Abc::Int32ArraySamplePtr getCreaseLengths() const
        { return m_creaseLengths; }

        Abc::FloatArraySamplePtr getCreaseSharpnesses() const
        { return m_creaseSharpnesses; }

        // corners
        Abc::Int32ArraySamplePtr getCornerIndices() const
        { return m_cornerIndices; }

        Abc::FloatArraySamplePtr getCornerSharpnesses() const
        { return m_cornerSharpnesses; }

        // Holes
        Abc::Int32ArraySamplePtr getHoles() const { return m_holes; }

        // subdivision scheme
        std::string getSubdivisionScheme() const
        { return m_subdScheme; }

        // bounds
        Abc::Box3d getSelfBounds() const { return m_selfBounds; }
        Abc::Box3d getChildBounds() const { return m_childBounds; }


        bool valid() const
        {
            return m_positions && m_faceIndices && m_faceCounts;
        }

        void reset()
        {
            m_positions.reset();
            m_faceIndices.reset();
            m_faceCounts.reset();

            m_faceVaryingInterpolateBoundary = 0;
            m_faceVaryingPropagateCorners = 0;
            m_interpolateBoundary = 0;

            m_creaseIndices.reset();
            m_creaseLengths.reset();
            m_creaseSharpnesses.reset();

            m_cornerIndices.reset();
            m_cornerSharpnesses.reset();

            m_holes.reset();

            m_subdScheme = "catmull-clark";

            m_selfBounds.makeEmpty();
            m_childBounds.makeEmpty();
        }

        ALEMBIC_OPERATOR_BOOL( valid() );

    protected:
        friend class ISubDSchema;

        Abc::P3fArraySamplePtr m_positions;
        Abc::Int32ArraySamplePtr m_faceIndices;
        Abc::Int32ArraySamplePtr m_faceCounts;

        int32_t m_faceVaryingInterpolateBoundary;
        int32_t m_faceVaryingPropagateCorners;
        int32_t m_interpolateBoundary;

        // Creases
        Abc::Int32ArraySamplePtr    m_creaseIndices;
        Abc::Int32ArraySamplePtr    m_creaseLengths;
        Abc::FloatArraySamplePtr  m_creaseSharpnesses;

        // Corners
        Abc::Int32ArraySamplePtr    m_cornerIndices;
        Abc::FloatArraySamplePtr  m_cornerSharpnesses;

        // Holes
        Abc::Int32ArraySamplePtr    m_holes;

        // subdivision scheme
        std::string m_subdScheme;

        // bounds
        Abc::Box3d m_selfBounds;
        Abc::Box3d m_childBounds;

    }; // end ISubDSchema::Sample

    //-*************************************************************************
    // SUBD SCHEMA
    //-*************************************************************************
public:
    //! By convention we always define this_type in AbcGeom classes.
    //! Used by unspecified-bool-type conversion below
    typedef ISubDSchema this_type;

    //-*************************************************************************
    // CONSTRUCTION, DESTRUCTION, ASSIGNMENT
    //-*************************************************************************

    //! The default constructor creates an empty ISubDSchema
    //! ...
    ISubDSchema() {}

    //! This templated, primary constructor creates a new subd reader.
    //! The first argument is any Abc (or AbcCoreAbstract) object
    //! which can intrusively be converted to an CompoundPropertyWriterPtr
    //! to use as a parent, from which the error handler policy for
    //! inheritance is also derived.  The remaining optional arguments
    //! can be used to override the ErrorHandlerPolicy and to specify
    //! schema interpretation matching.
    template <class CPROP_PTR>
    ISubDSchema( CPROP_PTR iParent,
                 const std::string &iName,

                 const Abc::Argument &iArg0 = Abc::Argument(),
                 const Abc::Argument &iArg1 = Abc::Argument() )
      : IGeomBaseSchema<SubDSchemaInfo>( iParent, iName,
                                      iArg0, iArg1 )
    {
        init(  iArg0, iArg1 );
    }

    //! Same constructor as above, but use the default schema name, ie,
    //! ".geom".
    template <class CPROP_PTR>
    explicit ISubDSchema( CPROP_PTR iParent,
                          const Abc::Argument &iArg0 = Abc::Argument(),
                          const Abc::Argument &iArg1 = Abc::Argument() )
      : IGeomBaseSchema<SubDSchemaInfo>( iParent,
                                      iArg0, iArg1 )
    {
        init( iArg0, iArg1 );
    }

    //! wrap an existing schema object
    template <class CPROP_PTR>
    ISubDSchema( CPROP_PTR iThis,
                 Abc::WrapExistingFlag iFlag,

                 const Abc::Argument &iArg0 = Abc::Argument(),
                 const Abc::Argument &iArg1 = Abc::Argument() )
      : IGeomBaseSchema<SubDSchemaInfo>( iThis, iFlag, iArg0, iArg1 )
    {
        init( iArg0, iArg1 );
    }


    //! Default assignment operator used.

    //-*************************************************************************
    // SCHEMA STUFF
    //-*************************************************************************


    MeshTopologyVariance getTopologyVariance() const;

    //! if isConstant() is true, the mesh contains no time-varying values
    bool isConstant() const { return getTopologyVariance() == kConstantTopology; }

    //-*************************************************************************
    // SAMPLE STUFF
    //-*************************************************************************

    //! Get number of samples written so far.
    //! ...
    size_t getNumSamples() const;

    //! Return the time sampling
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

    void get( Sample &iSamp,
              const Abc::ISampleSelector &iSS = Abc::ISampleSelector() ) const;

    Sample getValue( const Abc::ISampleSelector &iSS = Abc::ISampleSelector() ) const
    {
        Sample smp;
        get( smp, iSS );
        return smp;
    }

    Abc::IInt32ArrayProperty getFaceCountsProperty() const
    { return m_faceCountsProperty; }
    Abc::IInt32ArrayProperty getFaceIndicesProperty() const
    { return m_faceIndicesProperty; }
    Abc::IP3fArrayProperty getPositionsProperty() const
    { return m_positionsProperty; }

    Abc::IInt32Property getFaceVaryingInterpolateBoundaryProperty() const
    { return m_faceVaryingInterpolateBoundaryProperty; }

    Abc::IInt32Property getFaceVaryingPropagateCornersProperty() const
    { return m_faceVaryingPropagateCornersProperty; }

    Abc::IInt32Property getInterpolateBoundaryProperty() const
    { return m_interpolateBoundaryProperty; }

    Abc::IInt32ArrayProperty getCreaseIndicesProperty() const
    { return m_creaseIndicesProperty; }
    Abc::IInt32ArrayProperty getCreaseLengthsProperty() const
    { return m_creaseLengthsProperty; }
    Abc::IFloatArrayProperty getCreaseSharpnessesProperty() const
    { return m_creaseSharpnessesProperty; }

    Abc::IInt32ArrayProperty getCornerIndicesProperty() const
    { return m_cornerIndicesProperty; }
    Abc::IFloatArrayProperty getCornerSharpnessesProperty() const
    { return m_cornerSharpnessesProperty; }

    Abc::IInt32ArrayProperty getHolesProperty() const { return m_holesProperty; }

    Abc::IStringProperty getSubdivisionSchemeProperty() const
    { return m_subdSchemeProperty; }

    IV2fGeomParam &getUVsParam() { return m_uvsParam; }
    const IV2fGeomParam &getUVsParam() const { return m_uvsParam; }

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
        m_faceIndicesProperty.reset();
        m_faceCountsProperty.reset();

        m_faceVaryingInterpolateBoundaryProperty.reset();
        m_faceVaryingPropagateCornersProperty.reset();
        m_interpolateBoundaryProperty.reset();

        m_creaseIndicesProperty.reset();
        m_creaseLengthsProperty.reset();
        m_creaseSharpnessesProperty.reset();

        m_cornerIndicesProperty.reset();
        m_cornerSharpnessesProperty.reset();

        m_holesProperty.reset();

        m_subdSchemeProperty.reset();

        m_uvsParam.reset();

        IGeomBaseSchema<SubDSchemaInfo>::reset();
    }

    //! Valid returns whether this function set is
    //! valid.
    bool valid() const
    {
        return ( IGeomBaseSchema<SubDSchemaInfo>::valid() &&
                 m_positionsProperty.valid() &&
                 m_faceIndicesProperty.valid() &&
                 m_faceCountsProperty.valid() );
    }

    // FaceSet related
    //! Appends the names of any FaceSets for this SubD.
    void getFaceSetNames( std::vector <std::string> &oFaceSetNames );
    IFaceSet getFaceSet( const std::string &iFaceSetName );
    bool hasFaceSet( const std::string &iFaceSetName );

    //! unspecified-bool-type operator overload.
    //! ...
    ALEMBIC_OVERRIDE_OPERATOR_BOOL( ISubDSchema::valid() );

    // Copy constructors
    ISubDSchema(const ISubDSchema& iCopy)
        : IGeomBaseSchema<SubDSchemaInfo>()
    {
        *this = iCopy;
    }
    const ISubDSchema & operator=(const ISubDSchema & rhs);

protected:
    void init( const Abc::Argument &iArg0, const Abc::Argument &iArg1 );

    Abc::IP3fArrayProperty   m_positionsProperty;
    Abc::IInt32ArrayProperty m_faceIndicesProperty;
    Abc::IInt32ArrayProperty m_faceCountsProperty;

    // misc
    Abc::IInt32Property m_faceVaryingInterpolateBoundaryProperty;
    Abc::IInt32Property m_faceVaryingPropagateCornersProperty;
    Abc::IInt32Property m_interpolateBoundaryProperty;

    // Creases
    Abc::IInt32ArrayProperty  m_creaseIndicesProperty;
    Abc::IInt32ArrayProperty  m_creaseLengthsProperty;
    Abc::IFloatArrayProperty  m_creaseSharpnessesProperty;

    // Corners
    Abc::IInt32ArrayProperty  m_cornerIndicesProperty;
    Abc::IFloatArrayProperty  m_cornerSharpnessesProperty;

    // Holes
    Abc::IInt32ArrayProperty  m_holesProperty;

    // subdivision scheme
    Abc::IStringProperty      m_subdSchemeProperty;

    // UVs
    IV2fGeomParam m_uvsParam;

    // FaceSets, this starts as empty until client
    // code attempts to access facesets.
    bool                              m_faceSetsLoaded;
    std::map <std::string, IFaceSet>  m_faceSets;
    boost::mutex                      m_faceSetsMutex;
    void loadFaceSetNames();

};

//-*****************************************************************************
// SCHEMA OBJECT
//-*****************************************************************************
typedef Abc::ISchemaObject<ISubDSchema> ISubD;

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcGeom
} // End namespace Alembic

#endif
