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

#ifndef _Alembic_AbcGeom_OCurves_h_
#define _Alembic_AbcGeom_OCurves_h_

#include <Alembic/AbcGeom/Foundation.h>
#include <Alembic/AbcGeom/Basis.h>
#include <Alembic/AbcGeom/CurveType.h>
#include <Alembic/AbcGeom/SchemaInfoDeclarations.h>
#include <Alembic/AbcGeom/OGeomParam.h>
#include <Alembic/AbcGeom/OGeomBase.h>

namespace Alembic {
namespace AbcGeom {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
// Curves definition - Similar in form to the Geometric primitive used to
// specify curves in renderman.
// "type"   - linear or cubic, one type for all curves
// "wrap"   - periodic or nonperiodic, one mode for all curves
// ---
// "P"      - vertexes for the curves being written
// "width"  - can be constant or can vary
// "N"      - (just like PolyMesh, via a geom parameter) Normals
// "uv"     - (just like PolyMesh, via a geom parameter) u-v coordinates
class OCurvesSchema : public OGeomBaseSchema<CurvesSchemaInfo>
{
public:
    //-*************************************************************************
    // CURVE SCHEMA SAMPLE TYPE
    //-*************************************************************************
    class Sample
    {
    public:
        //! Creates a default sample with no data in it.
        //! ...
        Sample()
        {
            // even though this might not be written out
            // (unless curvesNumVertices and points is set) give some reasonable
            // and predictable defaults
            reset();
            m_type = kCubic;
            m_wrap = kNonPeriodic;
            m_basis = kBezierBasis;
        }

        //! Creates a sample with position data but no index
        //! or count data. For specifying samples after the first one
        Sample( const Abc::P3fArraySample &iPos )
          : m_positions( iPos )
        {
            // even though this might not be written out
            // (unless curvesNumVertices is set) give some reasonable
            // and predictable defaults
            m_type = kCubic;
            m_wrap = kNonPeriodic;
            m_basis = kBezierBasis;
        }


        //! Creates a sample with position data, index data, count data,
        //! and optional UV and Normals data.
        //! For specifying samples with an explicit topology. The first
        //! sample must be full like this. Subsequent samples may also
        //! be full like this, which would indicate a change of topology
        Sample(
                const Abc::P3fArraySample &iPos,
                const Abc::Int32ArraySample &iNVertices,
                const CurveType &iType = kCubic,
                const CurvePeriodicity iWrap = kNonPeriodic,
                const OFloatGeomParam::Sample &iWidths = \
                OFloatGeomParam::Sample(),
                const OV2fGeomParam::Sample &iUVs = OV2fGeomParam::Sample(),
                const ON3fGeomParam::Sample &iNormals = ON3fGeomParam::Sample(),
                const BasisType &iBasis = kBezierBasis )
          : m_positions( iPos ),
            m_nVertices( iNVertices ),
            m_type( iType ),
            m_wrap( iWrap ),
            m_widths( iWidths ),
            m_uvs( iUVs ),
            m_normals( iNormals ),
            m_basis( iBasis ) {}

        // widths accessor
        const OFloatGeomParam::Sample &getWidths() const { return m_widths; }
        void setWidths( const OFloatGeomParam::Sample &iWidths )
        { m_widths = iWidths; }

        // positions accessor
        const Abc::P3fArraySample &getPositions() const { return m_positions; }
        void setPositions( const Abc::P3fArraySample &iSmp )
        { m_positions = iSmp; }

        // type accessors
        void setType( const CurveType &iType )
        { m_type = iType; }
        CurveType getType() const { return m_type; }

        // wrap accessors
        void setWrap( const CurvePeriodicity &iWrap )
        { m_wrap = iWrap; }
        CurvePeriodicity getWrap() const { return m_wrap; }

        std::size_t getNumCurves() const { return m_nVertices.size(); }

        //! an array of ints that corresponds to the number
        //! of vertices per curve
        void setCurvesNumVertices( const Abc::Int32ArraySample &iNVertices)
        { m_nVertices = iNVertices; }
        const Abc::Int32ArraySample &getCurvesNumVertices() const
        { return m_nVertices; }

        // UVs
        const OV2fGeomParam::Sample &getUVs() const { return m_uvs; }
        void setUVs( const OV2fGeomParam::Sample &iUVs )
        { m_uvs = iUVs; }

        // bounding box accessors
        const Abc::Box3d &getSelfBounds() const { return m_selfBounds; }
        void setSelfBounds( const Abc::Box3d &iBnds )
        { m_selfBounds = iBnds; }

        const Abc::Box3d &getChildBounds() const { return m_childBounds; }
        void setChildBounds( const Abc::Box3d &iBnds )
        { m_childBounds = iBnds; }

        // normal accessors
        const ON3fGeomParam::Sample &getNormals() const { return m_normals; }
        void setNormals( const ON3fGeomParam::Sample &iNormals )
        { m_normals = iNormals; }

        // basis accessors
        BasisType getBasis() const { return m_basis; }
        void setBasis( const BasisType &iBasis )
        { m_basis = iBasis; }

        void reset()
        {
            m_positions.reset();
            m_uvs.reset();
            m_normals.reset();
            m_widths.reset();

            m_nVertices.reset();

            m_selfBounds.makeEmpty();
            m_childBounds.makeEmpty();

            m_type = kCubic;
            m_wrap = kNonPeriodic;
            m_basis = kBezierBasis;
        }

    protected:

        // properties
        Abc::P3fArraySample m_positions;
        Abc::Int32ArraySample m_nVertices;

        CurveType m_type;
        CurvePeriodicity m_wrap;

        OFloatGeomParam::Sample m_widths;
        OV2fGeomParam::Sample m_uvs;
        ON3fGeomParam::Sample m_normals;

        BasisType m_basis;

        // bounding box attributes
        Abc::Box3d m_selfBounds;
        Abc::Box3d m_childBounds;

    };

    //-*************************************************************************
    // CURVE SCHEMA
    //-*************************************************************************

public:

    //! By convention we always define this_type in AbcGeom classes.
    //! Used by unspecified-bool-type conversion below
    typedef OCurvesSchema this_type;
    typedef OCurvesSchema::Sample sample_type;

    //-*************************************************************************
    // CONSTRUCTION, DESTRUCTION, ASSIGNMENT
    //-*************************************************************************

    //! The default constructor creates an empty OCurvesSchema
    //! ...
    OCurvesSchema() {}

    //! This templated, primary constructor creates a new poly mesh writer.
    //! The first argument is any Abc (or AbcCoreAbstract) object
    //! which can intrusively be converted to an CompoundPropertyWriterPtr
    //! to use as a parent, from which the error handler policy for
    //! inheritance is also derived.  The remaining optional arguments
    //! can be used to override the ErrorHandlerPolicy, to specify
    //! MetaData, and to set TimeSamplingType.
    template <class CPROP_PTR>
    OCurvesSchema( CPROP_PTR iParent,
                   const std::string &iName,
                   const Abc::Argument &iArg0 = Abc::Argument(),
                   const Abc::Argument &iArg1 = Abc::Argument(),
                   const Abc::Argument &iArg2 = Abc::Argument() )
      : OGeomBaseSchema<CurvesSchemaInfo>( iParent, iName,
                                        iArg0, iArg1, iArg2 )
    {
        // Meta data and error handling are eaten up by
        // the super type, so all that's left is time sampling.
        AbcA::TimeSamplingPtr tsPtr =
            Abc::GetTimeSampling( iArg0, iArg1, iArg2 );

        AbcA::index_t tsIndex =
            Abc::GetTimeSamplingIndex( iArg0, iArg1, iArg2 );

        if ( tsPtr )
        {
            tsIndex = iParent->getObject()->getArchive()->
                addTimeSampling( *tsPtr );
        }

        init( tsIndex );
    }

    template <class CPROP_PTR>
    explicit OCurvesSchema( CPROP_PTR iParent,
                            const Abc::Argument &iArg0 = Abc::Argument(),
                            const Abc::Argument &iArg1 = Abc::Argument(),
                            const Abc::Argument &iArg2 = Abc::Argument() )
      : OGeomBaseSchema<CurvesSchemaInfo>( iParent,
                                        iArg0, iArg1, iArg2 )
    {
        // Meta data and error handling are eaten up by
        // the super type, so all that's left is time sampling.
        AbcA::TimeSamplingPtr tsPtr =
            Abc::GetTimeSampling( iArg0, iArg1, iArg2 );

        AbcA::index_t tsIndex =
            Abc::GetTimeSamplingIndex( iArg0, iArg1, iArg2 );

        if ( tsPtr )
        {
            tsIndex = iParent->getObject()->getArchive()->
                addTimeSampling( *tsPtr );
        }

        init( tsIndex );
    }

    OCurvesSchema( const OCurvesSchema& iCopy )
        : OGeomBaseSchema<CurvesSchemaInfo>()
    {
        *this = iCopy;
    }

    //! Default assignment operator used.

    //-*************************************************************************
    // SCHEMA STUFF
    //-*************************************************************************

    //! Return the time sampling type, which is stored on each of the
    //! sub properties.
    AbcA::TimeSamplingPtr getTimeSampling()
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
    void set( const sample_type &iSamp );

    //! Set from previous sample. Will apply to each of positions,
    //! indices, and counts.
    void setFromPrevious();

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
        m_uvsParam.reset();
        m_normalsParam.reset();
        m_widthsParam.reset();
        m_nVerticesProperty.reset();

        m_basisAndTypeProperty.reset();

        OGeomBaseSchema<CurvesSchemaInfo>::reset();
    }

    //! Valid returns whether this function set is
    //! valid.
    bool valid() const
    {
        return ( OGeomBaseSchema<CurvesSchemaInfo>::valid() &&
                 m_positionsProperty.valid() );
    }

    //! unspecified-bool-type operator overload.
    //! ...
    ALEMBIC_OVERRIDE_OPERATOR_BOOL( this_type::valid() );

protected:
    void init( const AbcA::index_t iTsIdx );

    // point data
    Abc::OP3fArrayProperty m_positionsProperty;
    Abc::OInt32ArrayProperty m_nVerticesProperty;

    // per-point data
    OV2fGeomParam m_uvsParam;
    ON3fGeomParam m_normalsParam;
    OFloatGeomParam m_widthsParam;

    Abc::OScalarProperty m_basisAndTypeProperty;
};

//-*****************************************************************************
// SCHEMA OBJECT
//-*****************************************************************************
typedef Abc::OSchemaObject<OCurvesSchema> OCurves;

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcGeom
} // End namespace Alembic

#endif
