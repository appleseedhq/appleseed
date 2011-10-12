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

#ifndef _Alembic_Abc_Foundation_h_
#define _Alembic_Abc_Foundation_h_

#include <Alembic/AbcCoreAbstract/All.h>
#include <Alembic/Util/All.h>

#include <ImathVec.h>
#include <ImathBox.h>
#include <ImathMatrix.h>
#include <ImathQuat.h>
#include <ImathColor.h>

#include <boost/variant.hpp>
#include <boost/smart_ptr.hpp>
#include <boost/format.hpp>
#include <boost/ref.hpp>
#include <boost/type_traits.hpp>

#include <iostream>
#include <string>
#include <exception>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

namespace Alembic {
namespace Abc {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
// Bring 'em ALL in.
namespace AbcA = ::Alembic::AbcCoreAbstract::ALEMBIC_VERSION_NS;
using namespace AbcA;

//-*****************************************************************************
//! Flag for specifying whether to match interpretations or schemas
//! When we create objects that have certain expected properties, data formats
//! or layouts, we use things called "SchemaTitles" and "Interpretations",
//! both of which are just strings, for providing a hint as to the meaning
//! of CompoundProperties and SimpleProperties, respectively.
//! This flag is used by the wrapper classes to indicate how they will
//! assert a match of schemaTitle or interpretation.
//-*****************************************************************************
enum SchemaInterpMatching
{
    kStrictMatching,
    kNoMatching,
    kSchemaTitleMatching
};

//-*****************************************************************************
//! We want to be able to use our wrapper classes to wrap existing writer
//! and reader objects from AbcCoreAbstract. However, the constructors
//! for these wrapper classes have trouble distinguishing between the
//! user request to wrap an existing writer, vs the request to create a new
//! writer.
//! While for some of the properties herein this is actually not ambiguous,
//! we insist on the use of this flag because it makes code and intention
//! more readable.
//-*****************************************************************************
enum WrapExistingFlag
{
    kWrapExisting
};

//-*****************************************************************************
//! This flag exists to indicate that the "top" object or compound property
//! is desired - when getting the top object from the Archive or
//! getting the top compound property from the Object.
//-*****************************************************************************
enum TopFlag
{
    kTop
};

//-*****************************************************************************
//-*****************************************************************************
//-*****************************************************************************
//-*****************************************************************************
// IMPORTED IMATH TYPES
//-*****************************************************************************
//-*****************************************************************************
//-*****************************************************************************
using Imath::V2s;
using Imath::V2i;
using Imath::V2f;
using Imath::V2d;

using Imath::V3s;
using Imath::V3i;
using Imath::V3f;
using Imath::V3d;

using Imath::Box2s;
using Imath::Box2i;
using Imath::Box2f;
using Imath::Box2d;

using Imath::Box3s;
using Imath::Box3i;
using Imath::Box3f;
using Imath::Box3d;

using Imath::M33f;
using Imath::M33d;
using Imath::M44f;
using Imath::M44d;

using Imath::Quatf;
using Imath::Quatd;

using Imath::C3h;
using Imath::C3f;
using Imath::C3c;

using Imath::C4h;
using Imath::C4f;
using Imath::C4c;

typedef V3f N3f;
typedef V3d N3d;

//-*****************************************************************************
//-*****************************************************************************
// OBJECT EXTRACTION FUNCTIONS
// These are intrusive methods used by the Abc constructors.
//-*****************************************************************************
//-*****************************************************************************

//-*****************************************************************************
inline AbcA::CompoundPropertyWriterPtr
GetCompoundPropertyWriterPtr( AbcA::CompoundPropertyWriterPtr iPtr )
{
    return iPtr;
}

//-*****************************************************************************
inline AbcA::CompoundPropertyReaderPtr
GetCompoundPropertyReaderPtr( AbcA::CompoundPropertyReaderPtr iPtr )
{
    return iPtr;
}

//-*****************************************************************************
//-*****************************************************************************
// OBJECT EXTRACTION FUNCTIONS
// These are intrusive methods used by the templated Abc constructors.
//-*****************************************************************************
//-*****************************************************************************

//-*****************************************************************************
inline AbcA::ObjectWriterPtr GetObjectWriterPtr( AbcA::ObjectWriterPtr iPtr )
{
    return iPtr;
}

//-*****************************************************************************
inline AbcA::ObjectReaderPtr GetObjectReaderPtr( AbcA::ObjectReaderPtr iPtr )
{
    return iPtr;
}

//-*****************************************************************************
//-*****************************************************************************
// ARCHIVE EXTRACTION FUNCTIONS
// These are intrusive methods used by the templated Abc constructors.
//-*****************************************************************************
//-*****************************************************************************

//-*****************************************************************************
inline AbcA::ArchiveWriterPtr GetArchiveWriterPtr( AbcA::ArchiveWriterPtr iPtr )
{
    return iPtr;
}

//-*****************************************************************************
inline AbcA::ArchiveReaderPtr GetArchiveReaderPtr( AbcA::ArchiveReaderPtr iPtr )
{
    return iPtr;
}

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace Abc
} // End namespace Alembic

#endif
