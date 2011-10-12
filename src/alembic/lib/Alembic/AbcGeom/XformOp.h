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

#ifndef _Alembic_AbcGeom_XformOp_h_
#define _Alembic_AbcGeom_XformOp_h_

#include <Alembic/AbcGeom/Foundation.h>

#include <set>

namespace Alembic {
namespace AbcGeom {
namespace ALEMBIC_VERSION_NS {

//! \brief The Matrix identifier hint.
//! Some 3d packages (like Maya) may have certain transformation operations
//! that aren't supported in other packages.  MatrixHint is meant to
//! help with reading back into applications that natively support the type.
enum MatrixHint
{
    //! Regular Matrix
    kMatrixHint = 0,

    //! Matrix represents Maya's version of Shear
    kMayaShearHint = 1
};

//! \brief The Rotate identifier hint.
//! Some 3d packages (like Maya) have multiple rotation operations
//! that are mathmatically of the same type.  RotateHint is meant to
//! help disambiguate these similiar mathmatical types when reading back
//! into applications that natively support the type.
enum RotateHint
{
    //! Default regular rotation
    kRotateHint = 0,

    //! Rotation that goes along with the rotate pivot to help
    //! orient the local rotation space.
    kRotateOrientationHint = 1
};

//! \brief The Scale identifier hint.
//! Some 3d packages (like Maya) have multiple transformation operations
//! that are mathmatically of the same type.  ScaleHint is meant to
//! help disambiguate these similiar mathmatical types when reading back
//! into applications that natively support that type.
enum ScaleHint
{
    //! Default, regular scale.
    kScaleHint = 0
};


//! \brief The Translation identifier hint.
//! Some 3d packages (like Maya) have multiple transformation operations
//! that are mathmatically of the same type.  TranslateHint is meant to
//! help disambiguate these similiar mathmatical types when reading back
//! into applications that natively support the type.
enum TranslateHint
{
    //! Default, regular translation.
    kTranslateHint = 0,

    //! Translation used for scaling around a pivot point.
    kScalePivotPointHint = 1,

    //! Translation which is used to help preserve existing scale
    //! transformations when moving the pivot.
    kScalePivotTranslationHint = 2,

    //! Translation used for rotating around the pivot point.
    kRotatePivotPointHint = 3,

    //! Translation which is used to help preserve existing rotate
    //! transformations when moving the pivot.
    kRotatePivotTranslationHint = 4
};

//! \brief Xform Operation
//! This class holds the data about a particular transform operation, but does
//! not hold the actual data to calculate a 4x4 matrix.  It holds the type of
//! operation (Translate, Rotate, Scale, Matrix), a hint about the type which
//! can be interpreted by packages like Maya, and what particular parts of the
//! operations can change over time.
class XformOp
{
public:
    XformOp();

    XformOp( const XformOperationType iType,
             const Alembic::Util::uint8_t iHint = 0 );

    XformOp( const Alembic::Util::uint8_t iEncodedOp );

    //! Get the type of transform operation. (Translate, Rotate, Scale, Matrix)
    XformOperationType getType() const;

    //! Set the type of transform operation. (Translate, Rotate, Scale, Matrix)
    //! Setting the type resets the hint, and sets all the channels to static.
    void setType( const XformOperationType iType );

    //! Get the MatrixHint, RotateHint, TranslateHint, or ScaleHint to help
    //! disambiguate certain options that may have the same type.
    Alembic::Util::uint8_t getHint() const;

    //! Set the hint, if it is an illegal value for the type, then the hint
    //! is set to the default, 0.
    void setHint( const Alembic::Util::uint8_t iHint );

    //! Returns whether the x component (index 0) is animated.
    //! Only meaningful on read.
    bool isXAnimated() const;

    //! Returns whether the y component (index 1) is animated.
    //! Only meaningful on read.
    bool isYAnimated() const;

    //! Returns whether the z component (index 2) is animated.
    //! Only meaningful on read.
    bool isZAnimated() const;

    //! Returns whether the angle component (index 3) is animated.
    //! Since Scale and Translate do not have an angle component,
    //! false is returned for those types.
    //! Only meaningful on read.
    bool isAngleAnimated() const;

    //! Returns whether a particular channel is animated.
    //! Scale and Translate only have 3 channels, Rotate has 4, and
    //! Matrix has 16.  Indices greater than the number of channels will
    //! return false.
    //! Only meaningful on read.
    bool isChannelAnimated( std::size_t iIndex ) const;

    //! Get the number of components that this operation has based on the type.
    //! Translate and Scale have 3, Rotate has 4 and Matrix has 16, and
    //! single-axis rotate ops (rotateX, rotateY, rotateZ) have 1.
    std::size_t getNumChannels() const;

    //! For every channel, there's a default value.  Typically, for each op
    //! type, it's the same across channels. But matrix ops have different
    //! defaults to allow the identity matrix to be defaulted (most channels
    //! there are 0.0, the determinant channels are 1.0).
    double getDefaultChannelValue( std::size_t iIndex ) const;

    double getChannelValue( std::size_t iIndex ) const;

    //! Set a single channel; will throw if iIndex is greater than
    //! numchannels - 1.
    void setChannelValue( std::size_t iIndex, double iVal );

    void setVector( const Abc::V3d &iVec );
    void setTranslate( const Abc::V3d &iTrans );
    void setScale( const Abc::V3d &iScale );
    void setAxis( const Abc::V3d &iAxis );
    void setAngle( const double iAngle );
    void setMatrix( const Abc::M44d &iMatrix );

    void setXRotation( const double iAngle );
    void setYRotation( const double iAngle );
    void setZRotation( const double iAngle );

    // synthetic getters return by value
    Abc::V3d getVector() const;
    Abc::V3d getTranslate() const;
    Abc::V3d getScale() const;
    Abc::V3d getAxis() const;
    double getAngle() const;
    Abc::M44d getMatrix() const;

    double getXRotation() const;
    double getYRotation() const;
    double getZRotation() const;

    bool isTranslateOp() const;

    bool isScaleOp() const;

    bool isRotateOp() const;

    bool isMatrixOp() const;

    bool isRotateXOp() const;
    bool isRotateYOp() const;
    bool isRotateZOp() const;

    //! Function for returning the combined encoded type and hint.
    //! The type is in the first four bits, the hint in the second.
    //!
    //! This is not really intended for use by human clients of this class.
    Alembic::Util::uint8_t getOpEncoding() const;


private:
    XformOperationType m_type;
    Alembic::Util::uint8_t m_hint;

    std::vector<double> m_channels;

    std::set<Alembic::Util::uint32_t> m_animChannels;

private:
    //! The IXform can tell the op if its channels are animated
    //! by directly inserting keys into the m_animChannels set.
    friend class IXformSchema;

};

typedef std::vector < XformOp > XformOpVec;

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcGeom
} // End namespace Alembic

#endif
