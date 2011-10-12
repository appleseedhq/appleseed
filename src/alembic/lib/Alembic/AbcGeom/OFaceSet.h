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

#ifndef _Alembic_AbcGeom_OFaceSet_h_
#define _Alembic_AbcGeom_OFaceSet_h_

#include <Alembic/AbcGeom/Foundation.h>
#include <Alembic/AbcGeom/SchemaInfoDeclarations.h>
#include <Alembic/AbcGeom/OGeomParam.h>
#include <Alembic/AbcGeom/FaceSetExclusivity.h>
#include <Alembic/AbcGeom/OGeomBase.h>

namespace Alembic {
namespace AbcGeom {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************

// Forward declarations of our friend classes
class OSubDSchema;
class OPolyMeshSchema;

//-*****************************************************************************
class OFaceSetSchema : public OGeomBaseSchema<FaceSetSchemaInfo>
{
public:
    //-*************************************************************************
    // Parition SAMPLE
    //-*************************************************************************
    class Sample
    {
    public:
        typedef Sample this_type;

        //! Creates a default sample with no data in it.
        //! ...
        Sample() { reset(); }

        //! Creates a sample with the list of faces that are in this
        //! faceset.
        //! The array of face numbers MUST be ordered by face number.
        //! Code that reads and works with facesets depends on this for efficency.
        //! e.g. call std::sort (myVecOfFaces.begin (), myVecOfFaces.end ());
        //! if you need to.
        //! The sample must be complete like this. Subsequent samples may also
        //! consist of faces which allows you to change of topology
        //! of the faceset.
        Sample( const Abc::Int32ArraySample &iFaceNums)
          : m_faces( iFaceNums )
        {}

        /* main accessors */
        // Faces
        const Abc::Int32ArraySample &getFaces() const { return m_faces; }
        void setFaces( const Abc::Int32ArraySample &iFaces)
        { m_faces = iFaces; }

        // Bounding boxes
        const Abc::Box3d &getSelfBounds() const { return m_selfBounds; }
        void setSelfBounds( const Abc::Box3d &iBnds )
        { m_selfBounds = iBnds; }

        const Abc::Box3d &getChildBounds() const { return m_childBounds; }
        void setChildBounds( const Abc::Box3d &iBnds )
        { m_childBounds = iBnds; }

        void reset()
        {
            m_faces.reset();

            m_selfBounds.makeEmpty();
            m_childBounds.makeEmpty();
        }

    protected:
        Abc::Int32ArraySample   m_faces;

        // bounds
        Abc::Box3d              m_selfBounds;
        Abc::Box3d              m_childBounds;
    }; // end OFaceSetSchema::Sample


    //-*************************************************************************
    // FaceSet SCHEMA
    //-*************************************************************************
public:
    //! By convention we always define this_type in AbcGeom classes.
    //! Used by unspecified-bool-type conversion below
    typedef OFaceSetSchema this_type;

    //-*************************************************************************
    // CONSTRUCTION, DESTRUCTION, ASSIGNMENT
    //-*************************************************************************
    //! The default constructor creates an empty OFaceSetSchema.
    //! OFaceSetSchema instances created this evaluate to a boolean value of false.
    OFaceSetSchema() {}

    //! This templated, primary constructor creates a new faceset writer.
    //! The first argument is any Abc (or AbcCoreAbstract) object
    //! which can intrusively be converted to an CompoundPropertyWriterPtr
    //! to use as a parent, from which the error handler policy for
    //! inheritance is also derived.  The remaining optional arguments
    //! can be used to override the ErrorHandlerPolicy, to specify
    //! MetaData, and to set TimeSamplingType.
    //! Most typically you won't need to use this ctor because the
    //! name argument here is only needed if you need to specially
    //! override the name of the compound property used internally
    //! by Alembic (for example if you needed to created your
    //! own dervied class from OFaceSet that needed to hold multiple
    //! faceset schema compound properties)
    template <class CPROP_PTR>
    OFaceSetSchema( CPROP_PTR iParentCompound,
                     const std::string &iName,
                     const Abc::Argument &iArg0 = Abc::Argument(),
                     const Abc::Argument &iArg1 = Abc::Argument(),
                     const Abc::Argument &iArg2 = Abc::Argument() )
      : OGeomBaseSchema<FaceSetSchemaInfo>( iParentCompound, iName,
                                   iArg0, iArg1, iArg2 )
    {
        _initTimeSampling ( iParentCompound, iArg0, iArg1, iArg2 );
    }

    template <class CPROP_PTR>
    void _initTimeSampling ( CPROP_PTR iParentCompound,
                     const Abc::Argument &iArg0 = Abc::Argument(),
                     const Abc::Argument &iArg1 = Abc::Argument(),
                     const Abc::Argument &iArg2 = Abc::Argument() )
    {
        AbcA::TimeSamplingPtr tsPtr =
            Abc::GetTimeSampling( iArg0, iArg1, iArg2 );
        uint32_t timeSamplingID =
            Abc::GetTimeSamplingIndex( iArg0, iArg1, iArg2 );

        // Add or find the timeSamplingID to use for our properties.
        if (tsPtr)
        {
            timeSamplingID = iParentCompound->getObject()->getArchive(
                )->addTimeSampling(*tsPtr);
        }

        // Meta data and error handling are eaten up by
        // the super type, so all that's left is time sampling.
        init( timeSamplingID );
    }

    template <class CPROP_PTR>
    explicit OFaceSetSchema( CPROP_PTR iParentCompound,
                              const Abc::Argument &iArg0 = Abc::Argument(),
                              const Abc::Argument &iArg1 = Abc::Argument(),
                              const Abc::Argument &iArg2 = Abc::Argument() )
      : OGeomBaseSchema<FaceSetSchemaInfo>( iParentCompound,
                                            iArg0, iArg1, iArg2 )
    {
        _initTimeSampling ( iParentCompound, iArg0, iArg1, iArg2 );
    }

    //! Copy constructor.
    OFaceSetSchema(const OFaceSetSchema& iCopy)
        : OGeomBaseSchema<FaceSetSchemaInfo>()
    {
        *this = iCopy;
    }

    //! Default assignment operator used.

    //-*************************************************************************
    // SCHEMA STUFF
    //-*************************************************************************

    //-*************************************************************************
    // SAMPLE STUFF
    //-*************************************************************************

    //! Get number of samples written so far.
    //! ...
    size_t getNumSamples()
    { return m_facesProperty.getNumSamples(); }

    //! Set a sample! First sample must have the list of faces in the faceset.
    void set( const Sample &iSamp );

    void setTimeSampling( uint32_t iTimeSamplingID );
    void setTimeSampling( AbcA::TimeSamplingPtr iTime );

    void setFaceExclusivity( FaceSetExclusivity iFacesExclusive );
    FaceSetExclusivity getFaceExclusivity() { return m_facesExclusive; }
    //-*************************************************************************
    // ABC BASE MECHANISMS
    // These functions are used by Abc to deal with errors, rewrapping,
    // and so on.
    //-*************************************************************************

    //! Reset returns this function set to an empty, default
    //! state.
    void reset()
    {
        m_selfBoundsProperty.reset();
        m_childBoundsProperty.reset();
        m_facesProperty.reset();

        OGeomBaseSchema<FaceSetSchemaInfo>::reset();
    }

    //! Valid returns whether this instance holds real data.
    bool valid() const
    {
        return ( OGeomBaseSchema<FaceSetSchemaInfo>::valid() &&
                 m_facesProperty.valid()
                 );
    }

    //! unspecified-bool-type operator overload.
    //! ...
    ALEMBIC_OVERRIDE_OPERATOR_BOOL( OFaceSetSchema::valid() );

protected:
    void _recordExclusivityHint();

    void init( uint32_t iTimeSamplingID );

    Abc::OInt32ArrayProperty    m_facesProperty;

    Abc::OUInt32Property        m_facesExclusiveProperty;
    FaceSetExclusivity          m_facesExclusive;

    friend class OSubDSchema;
    friend class OPolyMeshSchema;
};


//-*****************************************************************************
// Nice to use typedef for users of this class.
//-*****************************************************************************
typedef Abc::OSchemaObject<OFaceSetSchema> OFaceSet;

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcGeom
} // End namespace Alembic

#endif
