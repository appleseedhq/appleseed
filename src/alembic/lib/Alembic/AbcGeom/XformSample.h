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

#ifndef _Alembic_AbcGeom_XformSample_h_
#define _Alembic_AbcGeom_XformSample_h_

#include <Alembic/AbcGeom/Foundation.h>

#include <Alembic/AbcGeom/XformOp.h>

namespace Alembic {
namespace AbcGeom {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
class XformSample
{
public:
    XformSample();

    // add translate or scale op
    // returns the index of the op in its op-stack
    std::size_t addOp( XformOp iTransOrScaleOp, const Abc::V3d &iVal );

    // add rotate op
    // returns the index of the op in its op-stack
    std::size_t addOp( XformOp iRotateOp, const Abc::V3d &iAxis,
                       const double iAngleInDegrees );

    // add matrix op
    // returns the index of the op in its op-stack
    std::size_t addOp( XformOp iMatrixOp, const Abc::M44d &iMatrix );

    // add rotateX, rotateY, or rotateZ op
    std::size_t addOp( XformOp iSingleRotateOp,
                       const double iSingleAxisRotationInDegrees );

    // add an op with values already set on the op
    std::size_t addOp( const XformOp &iOp );

    XformOp getOp( const std::size_t iIndex ) const;

    XformOp &operator[]( const std::size_t &iIndex );
    const XformOp &operator[]( const std::size_t &iIndex ) const;

    std::size_t getNumOps() const;
    //! The sum of the number of channels of all the ops in this Sample.
    std::size_t getNumOpChannels() const;

    //! "Inherits xforms" means, "Does this xform concatenate to or ignore the
    //! transforms of its parents?"
    void setInheritsXforms( bool iInherits );
    bool getInheritsXforms() const;

    void setChildBounds( const Abc::Box3d &iBnds );
    const Abc::Box3d &getChildBounds() const;

    // non-op-based methods; the getters will compute their return values
    // from the ops under the hood, hence return-by-value.

    //! Order matters when calling different setFoo() methods;
    //! callstack of methods must be the same for each call to
    //! OXformSchmea::set() with a given Sample.
    //! For example, a Sample with calls to setTranslation() and
    //! setRotation() must make thgose same calls in the same order
    //! between each use of OXforSchema::set().
    void setTranslation( const Abc::V3d &iTrans );
    Abc::V3d getTranslation() const;

    void setRotation( const Abc::V3d &iAxis, const double iAngleInDegress );
    Abc::V3d getAxis() const;
    double getAngle() const;

    void setXRotation( const double iAngleInDegrees );
    double getXRotation() const;

    void setYRotation( const double iAngleInDegrees );
    double getYRotation() const;

    void setZRotation( const double iAngleInDegrees );
    double getZRotation() const;

    void setScale( const Abc::V3d &iScale );
    Abc::V3d getScale() const;

    void setMatrix( const Abc::M44d &iMatrix );
    Abc::M44d getMatrix() const;

    //! Tests whether this sample has the same topology as iSample
    bool isTopologyEqual( const XformSample & iSample );

    //! Has this Sample been used in a call to OXformSchema::set()
    bool getIsTopologyFrozen() const { return m_hasBeenRead; }

    void reset();

private:
    friend class OXformSchema;
    friend class IXformSchema;
    void freezeTopology();
    const std::vector<Alembic::Util::uint8_t> &getOpsArray() const;
    void clear();


private:
    //! 0 is unset; 1 is set via addOp; 2 is set via non-op-based methods
    int32_t m_setWithOpStack;

    std::vector<XformOp> m_ops;

    bool m_inherits;
    Abc::Box3d m_childBounds;

    //! This starts out false, but is set to true by the OXform and controls
    //! whether or not addOp() changes the topology of the Sample, in the form
    //! of the layout of the m_opsArray.
    bool m_hasBeenRead;

    size_t m_opIndex;
};


} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcGeom
} // End namespace Alembic

#endif
