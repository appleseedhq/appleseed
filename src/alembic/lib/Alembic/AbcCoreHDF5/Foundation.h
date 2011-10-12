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

#ifndef _Alembic_AbcCoreHDF5_Foundation_h_
#define _Alembic_AbcCoreHDF5_Foundation_h_

#include <Alembic/AbcCoreAbstract/All.h>

#include <Alembic/Util/All.h>

#include <boost/smart_ptr.hpp>
#include <boost/make_shared.hpp>
#include <boost/format.hpp>
#include <boost/enable_shared_from_this.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/operators.hpp>
#include <boost/utility.hpp>

#include <vector>
#include <string>
#include <map>

#include <iostream>

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

#include <hdf5.h>
#include <hdf5_hl.h>

#include <H5LTpublic.h>

#define ALEMBIC_HDF5_FILE_VERSION -8

//-*****************************************************************************

namespace Alembic {
namespace AbcCoreHDF5 {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
namespace AbcA = ::Alembic::AbcCoreAbstract;

using namespace ::Alembic::Util;
using AbcA::index_t;
using AbcA::chrono_t;

//-*****************************************************************************
typedef boost::weak_ptr<AbcA::ObjectWriter> WeakOwPtr;
typedef boost::weak_ptr<AbcA::BasePropertyWriter> WeakBpwPtr;

typedef boost::weak_ptr<AbcA::ObjectReader> WeakOrPtr;
typedef boost::weak_ptr<AbcA::BasePropertyReader> WeakBprPtr;

//-*****************************************************************************
typedef boost::shared_ptr<AbcA::PropertyHeader> PropertyHeaderPtr;
typedef std::vector<PropertyHeaderPtr> PropertyHeaderPtrs;

typedef boost::shared_ptr<AbcA::ObjectHeader> ObjectHeaderPtr;

//-*****************************************************************************
inline std::string getSampleName( const std::string &iName,
                                  index_t iSampleIndex )
{
    if ( iSampleIndex == 0 )
    {
        return iName + ".smp0";
    }
    else
    {
        // could use boost::lexical_cast
        std::ostringstream strm;
        strm.width(4);
        strm.fill('0');
        strm << iSampleIndex;
        return strm.str();
    }
}

} // End namespace ALEMBIC_VERSION_NS

using namespace ALEMBIC_VERSION_NS;

} // End namespace AbcCoreHDF5
} // End namespace Alembic

#endif
