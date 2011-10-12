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

#ifndef _Alembic_AbcGeom_OSubD_h_
#define _Alembic_AbcGeom_OSubD_h_

#include <map>
#include <Alembic/AbcGeom/Foundation.h>
#include <Alembic/AbcGeom/SchemaInfoDeclarations.h>
#include <Alembic/AbcGeom/OFaceSet.h>
#include <Alembic/AbcGeom/OGeomParam.h>
#include <Alembic/AbcGeom/OGeomBase.h>

namespace Alembic {
namespace AbcGeom {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
// for default values for int scalar properties here
static const int32_t ABC_GEOM_SUBD_NULL_INT_VALUE( INT_MIN / 2 );

//-*****************************************************************************
class OSubDSchema : public OGeomBaseSchema<SubDSchemaInfo>
{
public:
    //-*************************************************************************
    // SUBD SCHEMA SAMPLE TYPE
    //-*************************************************************************
    class Sample
    {
    public:
        //! Creates a default sample with no data in it.
        //! ...
        Sample() { reset(); }

        //! Creates a sample with position data, index data, and count data.
        //! For specifying samples with an explicit topology. The first
        //! sample must be full like this. Subsequent samples may also
        //! be full like this, which would indicate a change of topology
        Sample( const Abc::P3fArraySample &iPositions,
                const Abc::Int32ArraySample &iFaceIndices,
                const Abc::Int32ArraySample &iFaceCounts,

                const Abc::Int32ArraySample &iCreaseIndices =
                Abc::Int32ArraySample(),
                const Abc::Int32ArraySample &iCreaseLengths =
                Abc::Int32ArraySample(),
                const Abc::FloatArraySample &iCreaseSharpnesses =
                Abc::FloatArraySample(),

                const Abc::Int32ArraySample &iCornerIndices =
                Abc::Int32ArraySample(),
                const Abc::FloatArraySample &iCornerSharpnesses =
                Abc::FloatArraySample(),

                const Abc::Int32ArraySample &iHoles = Abc::Int32ArraySample()
              )

          : m_positions( iPositions )
          , m_faceIndices( iFaceIndices )
          , m_faceCounts( iFaceCounts )
          , m_faceVaryingInterpolateBoundary( ABC_GEOM_SUBD_NULL_INT_VALUE )
          , m_faceVaryingPropagateCorners( ABC_GEOM_SUBD_NULL_INT_VALUE )
          , m_interpolateBoundary( ABC_GEOM_SUBD_NULL_INT_VALUE )
          , m_creaseIndices     ( iCreaseIndices )
          , m_creaseLengths     ( iCreaseLengths )
          , m_creaseSharpnesses ( iCreaseSharpnesses )
          , m_cornerIndices     ( iCornerIndices )
          , m_cornerSharpnesses ( iCornerSharpnesses )
          , m_holes             ( iHoles )
          , m_subdScheme        ( "catmull-clark" )
        {}

        // main stuff
        const Abc::P3fArraySample &getPositions() const { return m_positions; }
        void setPositions( const Abc::P3fArraySample &iSmp )
        { m_positions = iSmp; }

        const Abc::Int32ArraySample &getFaceIndices() const { return m_faceIndices; }
        void setFaceIndices( const Abc::Int32ArraySample &iSmp )
        { m_faceIndices = iSmp; }

        const Abc::Int32ArraySample &getFaceCounts() const { return m_faceCounts; }
        void setFaceCounts( const Abc::Int32ArraySample &iCnt )
        { m_faceCounts = iCnt; }


        // misc subd stuff
        int32_t getFaceVaryingInterpolateBoundary() const
        { return m_faceVaryingInterpolateBoundary; }
        void setFaceVaryingInterpolateBoundary( int32_t i )
        { m_faceVaryingInterpolateBoundary = i; }

        int32_t getFaceVaryingPropagateCorners() const
        { return m_faceVaryingPropagateCorners; }
        void setFaceVaryingPropagateCorners( int32_t i )
        { m_faceVaryingPropagateCorners = i; }

        int32_t getInterpolateBoundary() const
        { return m_interpolateBoundary; }
        void setInterpolateBoundary( int32_t i )
        { m_interpolateBoundary = i; }

        // creases
        const Abc::Int32ArraySample &getCreaseIndices() const
        { return m_creaseIndices; }
        void setCreaseIndices( const Abc::Int32ArraySample &iCreaseIndices )
        { m_creaseIndices = iCreaseIndices; }

        const Abc::Int32ArraySample &getCreaseLengths() const
        { return m_creaseLengths; }
        void setCreaseLengths( const Abc::Int32ArraySample &iCreaseLengths )
        { m_creaseLengths = iCreaseLengths; }

        const Abc::FloatArraySample &getCreaseSharpnesses() const
        { return m_creaseSharpnesses; }
        void setCreaseSharpnesses( const Abc::FloatArraySample
                                   &iCreaseSharpnesses )
        { m_creaseSharpnesses = iCreaseSharpnesses; }

        void setCreases( const Abc::Int32ArraySample &iCreaseIndices,
                         const Abc::Int32ArraySample &iCreaseLengths )
        {
            m_creaseIndices = iCreaseIndices;
            m_creaseLengths = iCreaseLengths;
        }

        void setCreases( const Abc::Int32ArraySample   &iCreaseIndices,
                         const Abc::Int32ArraySample   &iCreaseLengths,
                         const Abc::FloatArraySample &iCreaseSharpnesses )
        {
            m_creaseIndices = iCreaseIndices;
            m_creaseLengths = iCreaseLengths;
            m_creaseSharpnesses = iCreaseSharpnesses;
        }

        // corners
        const Abc::Int32ArraySample &getCornerIndices() const
        { return m_cornerIndices; }
        void setCornerIndices( const Abc::Int32ArraySample &iCornerIndices )
        { m_cornerIndices = iCornerIndices; }

        const Abc::FloatArraySample &getCornerSharpnesses() const
        { return m_cornerSharpnesses; }
        void setCornerSharpnesses( const Abc::FloatArraySample
                                   &iCornerSharpnesses )
        { m_cornerSharpnesses = iCornerSharpnesses; }

        void setCorners( const Abc::Int32ArraySample &iCornerIndices,
                         const Abc::FloatArraySample &iCornerSharpnesses )
        {
            m_cornerIndices = iCornerIndices;
            m_cornerSharpnesses = iCornerSharpnesses;
        }

        // Holes
        const Abc::Int32ArraySample &getHoles() const
        { return m_holes; }
        void setHoles( const Abc::Int32ArraySample &iHoles )
        { m_cornerIndices = iHoles; }

        // subdivision scheme
        std::string getSubdivisionScheme() const
        { return m_subdScheme; }
        void setSubdivisionScheme( const std::string &iScheme )
        { m_subdScheme = iScheme; }

        // bounding boxes
        const Abc::Box3d &getSelfBounds() const { return m_selfBounds; }
        void setSelfBounds( const Abc::Box3d &iBnds )
        { m_selfBounds = iBnds; }

        const Abc::Box3d &getChildBounds() const { return m_childBounds; }
        void setChildBounds( const Abc::Box3d &iBnds )
        { m_childBounds = iBnds; }

        // UVs; need to set these outside the Sample constructor
        const OV2fGeomParam::Sample &getUVs() const { return m_uvs; }
        void setUVs( const OV2fGeomParam::Sample &iUVs )
        { m_uvs = iUVs; }

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

            m_uvs.reset();
        }

    protected:
        friend class OSubDSchema;

        Abc::P3fArraySample m_positions;
        Abc::Int32ArraySample m_faceIndices;
        Abc::Int32ArraySample m_faceCounts;

        int32_t m_faceVaryingInterpolateBoundary;
        int32_t m_faceVaryingPropagateCorners;
        int32_t m_interpolateBoundary;

        // Creases
        Abc::Int32ArraySample m_creaseIndices;
        Abc::Int32ArraySample m_creaseLengths;
        Abc::FloatArraySample m_creaseSharpnesses;

        // Corners
        Abc::Int32ArraySample m_cornerIndices;
        Abc::FloatArraySample m_cornerSharpnesses;

        // Holes
        Abc::Int32ArraySample m_holes;

        // subdivision scheme
        std::string m_subdScheme;

        // bounds
        Abc::Box3d m_selfBounds;
        Abc::Box3d m_childBounds;

        // UVs
        OV2fGeomParam::Sample m_uvs;

    }; // end OSubDSchema::Sample

    //-*************************************************************************
    // SUBD SCHEMA
    //-*************************************************************************
public:
    //! By convention we always define this_type in AbcGeom classes.
    //! Used by unspecified-bool-type conversion below
    typedef OSubDSchema this_type;

    //-*************************************************************************
    // CONSTRUCTION, DESTRUCTION, ASSIGNMENT
    //-*************************************************************************

    //! The default constructor creates an empty OSubDSchema
    //! ...
    OSubDSchema() {}

    //! This templated, primary constructor creates a new poly mesh writer.
    //! The first argument is any Abc (or AbcCoreAbstract) object
    //! which can intrusively be converted to an CompoundPropertyWriterPtr
    //! to use as a parent, from which the error handler policy for
    //! inheritance is also derived.  The remaining optional arguments
    //! can be used to override the ErrorHandlerPolicy, to specify
    //! MetaData, and to set TimeSamplingType.
    template <class CPROP_PTR>
    OSubDSchema( CPROP_PTR iParent,
                     const std::string &iName,

                     const Abc::Argument &iArg0 = Abc::Argument(),
                     const Abc::Argument &iArg1 = Abc::Argument(),
                     const Abc::Argument &iArg2 = Abc::Argument() )
      : OGeomBaseSchema<SubDSchemaInfo>( iParent, iName,
                                      iArg0, iArg1, iArg2 )
    {

        AbcA::TimeSamplingPtr tsPtr =
            Abc::GetTimeSampling( iArg0, iArg1, iArg2 );
        uint32_t tsIndex =
            Abc::GetTimeSamplingIndex( iArg0, iArg1, iArg2 );

        // if we specified a valid TimeSamplingPtr, use it to determine the
        // index otherwise we'll use the index, which defaults to the intrinsic
        // 0 index
        if (tsPtr)
        {
            tsIndex = iParent->getObject()->getArchive(
                )->addTimeSampling(*tsPtr);
        }

        // Meta data and error handling are eaten up by
        // the super type, so all that's left is time sampling.
        init( tsIndex );
    }

    template <class CPROP_PTR>
    explicit OSubDSchema( CPROP_PTR iParent,
                          const Abc::Argument &iArg0 = Abc::Argument(),
                          const Abc::Argument &iArg1 = Abc::Argument(),
                          const Abc::Argument &iArg2 = Abc::Argument() )
      : OGeomBaseSchema<SubDSchemaInfo>( iParent,
                                      iArg0, iArg1, iArg2 )
    {
        AbcA::TimeSamplingPtr tsPtr =
            Abc::GetTimeSampling( iArg0, iArg1, iArg2 );
        uint32_t tsIndex =
            Abc::GetTimeSamplingIndex( iArg0, iArg1, iArg2 );

        // if we specified a valid TimeSamplingPtr, use it to determine the
        // index otherwise we'll use the index, which defaults to the intrinsic
        // 0 index
        if (tsPtr)
        {
            tsIndex = iParent->getObject()->getArchive(
                )->addTimeSampling(*tsPtr);
        }

        // Meta data and error handling are eaten up by
        // the super type, so all that's left is time sampling.
        init( tsIndex );
    }

    //! Copy constructor.
    OSubDSchema(const OSubDSchema& iCopy)
        : OGeomBaseSchema<SubDSchemaInfo>()
    {
        *this = iCopy;
    }

    //! Default assignment operator used.

    //-*************************************************************************
    // SCHEMA STUFF
    //-*************************************************************************

    //! Return the time sampling, which is stored on each of the
    //! sub properties.
    AbcA::TimeSamplingPtr getTimeSampling() const
    { return m_positionsProperty.getTimeSampling(); }

    //-*************************************************************************
    // SAMPLE STUFF
    //-*************************************************************************

    //! Get number of samples written so far.
    //! ...
    size_t getNumSamples()
    { return m_positionsProperty.getNumSamples(); }

    //! Set a sample! Sample zero has to have non-degenerate
    //! positions, indices and counts.
    void set( const Sample &iSamp );

    //! Set from previous sample. Will apply to each of positions,
    //! indices, and counts.
    void setFromPrevious( );

    void setTimeSampling( uint32_t iIndex );
    void setTimeSampling( AbcA::TimeSamplingPtr iTime );

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

        m_creaseIndicesProperty.reset();
        m_creaseLengthsProperty.reset();
        m_creaseSharpnessesProperty.reset();

        m_cornerIndicesProperty.reset();
        m_cornerSharpnessesProperty.reset();

        m_holesProperty.reset();

        m_subdSchemeProperty.reset();

        m_uvsParam.reset();

        m_faceSets.clear ();

        OGeomBaseSchema<SubDSchemaInfo>::reset();
    }

    //! Valid returns whether this function set is
    //! valid.
    bool valid() const
    {
        return ( OGeomBaseSchema<SubDSchemaInfo>::valid() &&
                 m_positionsProperty.valid() &&
                 m_faceIndicesProperty.valid() &&
                 m_faceCountsProperty.valid() );
    }

    // FaceSet stuff
    OFaceSet & createFaceSet( const std::string &iFaceSetName );
    //! Appends the names of any FaceSets for this SubD.
    void getFaceSetNames( std::vector <std::string> & oFaceSetNames );
    OFaceSet getFaceSet( const std::string &iFaceSetName );
    bool hasFaceSet( const std::string &iFaceSetName );


    //! unspecified-bool-type operator overload.
    //! ...
    ALEMBIC_OVERRIDE_OPERATOR_BOOL( OSubDSchema::valid() );

protected:
    void init( uint32_t iTsIdx );

    Abc::OP3fArrayProperty m_positionsProperty;
    Abc::OInt32ArrayProperty m_faceIndicesProperty;
    Abc::OInt32ArrayProperty m_faceCountsProperty;

    // misc
    Abc::OInt32Property m_faceVaryingInterpolateBoundaryProperty;
    Abc::OInt32Property m_faceVaryingPropagateCornersProperty;
    Abc::OInt32Property m_interpolateBoundaryProperty;

    // Creases
    Abc::OInt32ArrayProperty m_creaseIndicesProperty;
    Abc::OInt32ArrayProperty m_creaseLengthsProperty;
    Abc::OFloatArrayProperty m_creaseSharpnessesProperty;

    // Corners
    Abc::OInt32ArrayProperty m_cornerIndicesProperty;
    Abc::OFloatArrayProperty m_cornerSharpnessesProperty;

    // Holes
    Abc::OInt32ArrayProperty m_holesProperty;

    // subdivision scheme
    Abc::OStringProperty m_subdSchemeProperty;

    // UVs
    OV2fGeomParam m_uvsParam;

private:
    void initCreases(uint32_t iNumSamples);
    void initCorners(uint32_t iNumSamples);
    void initHoles(uint32_t iNumSamples);

    // FaceSets created on this SubD
    std::map <std::string, OFaceSet>  m_faceSets;

    friend class OFaceSetSchema;;
};

//-*****************************************************************************
// SCHEMA OBJECT
//-*****************************************************************************
typedef Abc::OSchemaObject<OSubDSchema> OSubD;

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcGeom
} // End namespace Alembic

#endif
