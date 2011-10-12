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

#ifndef _Alembic_AbcGeom_FilmBackXformOp_h_
#define _Alembic_AbcGeom_FilmBackXformOp_h_

#include <Alembic/AbcGeom/Foundation.h>

namespace Alembic {
namespace AbcGeom {
namespace ALEMBIC_VERSION_NS {

//! \brief FilmBack Xform Operation
//! This class holds the data about a particular transform operation, but does
//! not hold the actual data to calculate a 3x3 matrix.  It holds the type of
//! operation (Translate, Scale, Matrix), a hint about the type which
//! can be interpreted by packages like Maya, and what particular parts of the
//! operations can change over time.
class FilmBackXformOp
{
public:
    FilmBackXformOp();

    FilmBackXformOp( const FilmBackXformOperationType iType,
             const std::string & iHint );

    //! Get the type of transform operation. (Translate, Scale, Matrix)
    FilmBackXformOperationType getType() const;

    //! Get the  hint to help disambiguate certain options that may have the
    //! same type.
    std::string getHint() const;

    //! Get the type and hint, where the first character is the type
    //! (t for translate, s for scale, m for matrix) and the rest of the
    //! returned string is the optional hint value.
    std::string getTypeAndHint() const;

    //! Get the number of components that this operation has based on the type.
    //! Translate and Scale have 2, and Matrix has 9.
    std::size_t getNumChannels() const;

    //! Get a single channel, 0 will be returned if iIndex is greater than
    //! numChannels - 1
    double getChannelValue( std::size_t iIndex ) const;

    //! Set a single channel; nothing will be set if iIndex is greater than
    //! numChannels - 1.
    void setChannelValue( std::size_t iIndex, double iVal );

    void setTranslate( const Abc::V2d &iTrans );
    void setScale( const Abc::V2d &iScale );
    void setMatrix( const Abc::M33d &iMatrix );

    // synthetic getters return by value
    Abc::V2d getTranslate() const;
    Abc::V2d getScale() const;
    Abc::M33d getMatrix() const;

    bool isTranslateOp() const;

    bool isScaleOp() const;

    bool isMatrixOp() const;

private:

    // friend to have access to the private constructor
    friend class ICameraSchema;
    FilmBackXformOp( const std::string & iTypeAndHint );

    FilmBackXformOperationType m_type;
    std::string m_hint;

    std::vector<double> m_channels;

};

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcGeom
} // End namespace Alembic

#endif
