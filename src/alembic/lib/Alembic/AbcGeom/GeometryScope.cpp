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

#include <Alembic/AbcGeom/GeometryScope.h>

namespace Alembic {
namespace AbcGeom {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
size_t GeometryScopeNumValuesQuadrics( GeometryScope iScope )
{
    switch ( iScope )
    {
    case kConstantScope:
    case kUniformScope: return 1;
    case kVaryingScope:
    case kVertexScope:
    case kFacevaryingScope: return 4;
    default:
        return 0;
    }
}

//-*****************************************************************************
size_t GeometryScopeNumValuesPolygon( GeometryScope iScope,
                                      size_t iNumVerts )
{
    switch ( iScope )
    {
    case kConstantScope:
    case kUniformScope: return 1;
    case kVaryingScope:
    case kVertexScope:
    case kFacevaryingScope: return iNumVerts;
    default: return 0;
    };
}
        
//-*****************************************************************************
size_t GeometryScopeNumValuesPointsPolygons( GeometryScope iScope,
                                             size_t iNumPolys,
                                             size_t iNumVerts,
                                             size_t iSumOfCounts )
{
    switch ( iScope )
    {
    case kConstantScope: return 1;
    case kUniformScope: return iNumPolys;
    case kVaryingScope:
    case kVertexScope: return iNumVerts;
    case kFacevaryingScope: return iSumOfCounts;
    default: return 0;
    };
}

//-*****************************************************************************
size_t GeometryScopeNumValuesPoints( GeometryScope iScope,
                                     size_t iNumPoints )
{
    switch ( iScope )
    {
    case kConstantScope:
    case kUniformScope: return 1;
    case kVaryingScope:
    case kVertexScope:
    case kFacevaryingScope: return iNumPoints;
    default: return 0;
    };
}

//-*****************************************************************************
size_t GeometryScopeNumValuesSubdivisionMesh( GeometryScope iScope,
                                              size_t iNumFaces,
                                              size_t iNumVerts,
                                              size_t iSumOfCounts )
{
    switch ( iScope )
    {
    case kConstantScope: return 1;
    case kUniformScope: return iNumFaces;
    case kVaryingScope:
    case kVertexScope: return iNumVerts;
    case kFacevaryingScope: return iSumOfCounts;
    default: return 0;
    };
}

//-*****************************************************************************
size_t GeometryScopeNumValuesBilinearPatch( GeometryScope iScope )
{
    switch ( iScope )
    {
    case kConstantScope:
    case kUniformScope: return 1;
    case kVaryingScope:
    case kFacevaryingScope: 
    case kVertexScope: return 4;
    default: return 0;
    };
}

//-*****************************************************************************
size_t GeometryScopeNumValuesBicubicPatch( GeometryScope iScope )

{
    switch ( iScope )
    {
    case kConstantScope:
    case kUniformScope: return 1;
    case kVaryingScope:
    case kFacevaryingScope: return 4;
    case kVertexScope: return 16;
    default: return 0;
    };
}

//-*****************************************************************************
size_t GeometryScopeNumValuesBilinearPatchMesh( GeometryScope iScope,
                                                size_t iNu, bool iUNoWrap,
                                                size_t iNv, bool iVNoWrap )
{
    switch ( iScope )
    {
    case kConstantScope: return 1;
    case kUniformScope: return (iNu-(size_t)iUNoWrap)*(iNv-(size_t)iVNoWrap);
    case kVaryingScope:
    case kFacevaryingScope:
    case kVertexScope: return iNu*iNv;
    default: return 0;
    };
}

//-*****************************************************************************
size_t GeometryScopeNumValuesBicubicPatchMesh( GeometryScope iScope,
                                               size_t iNu, bool iUNoWrap,
                                               size_t iNv, bool iVNoWrap )
{
    assert( iNu >= 4 && iNv >= 4 );
    size_t usegs = iNu-3;
    size_t vsegs = iNv-3;
    
    switch ( iScope )
    {
    case kConstantScope: return 1;
    case kUniformScope: return usegs*vsegs;
    case kVaryingScope:
    case kFacevaryingScope: return (usegs+(size_t)iUNoWrap)*
            (vsegs+(size_t)iVNoWrap);
    case kVertexScope: return iNu*iNv;
    default: return 0;
    };
}

//-*****************************************************************************
size_t GeometryScopeNumValuesNuPatch( GeometryScope iScope,
                                      size_t iNu, size_t iUorder,
                                      size_t iNv, size_t iVorder )
{
    switch ( iScope )
    {
    case kConstantScope: return 1;
    case kUniformScope: return (iNu-iUorder+1)*(iNv-iVorder+1);
    case kVaryingScope:
    case kFacevaryingScope: return (iNu-iUorder+2)*(iNv-iVorder+2);
    case kVertexScope: return iNu*iNv;
    default: return 0;
    };
}

//-*****************************************************************************
size_t GeometryScopeNumValuesLinearCurves( GeometryScope iScope,
                                           size_t iNumCurves, bool iNoWrap,
                                           size_t iSumOfCounts )
{
    switch ( iScope )
    {
    case kConstantScope: return 1;
    case kUniformScope: return iSumOfCounts -
            (iNumCurves * ( 1 + (size_t)iNoWrap));
    case kVaryingScope:
    case kFacevaryingScope: 
    case kVertexScope: return iSumOfCounts;
    default: return 0;
    };
}

//-*****************************************************************************
size_t GeometryScopeNumValuesCubicCurves( GeometryScope iScope,
                                          size_t iNumCurves, bool iNoWrap,
                                          size_t iSumOfCounts )
    
{
    switch ( iScope )
    {
    case kConstantScope: return 1;
    case kUniformScope: return iSumOfCounts - iNumCurves;
    case kVaryingScope:
    case kFacevaryingScope: return iSumOfCounts - iNumCurves +
            ( iNumCurves * ( size_t )iNoWrap );
    case kVertexScope: return iSumOfCounts;
    default: return 0;
    };
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace AbcGeom
} // End namespace Alembic

