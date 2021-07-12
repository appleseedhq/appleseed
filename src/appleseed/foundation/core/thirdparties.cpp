
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Francois Beaune, The appleseedhq Organization
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//

// Interface header.
#include "thirdparties.h"

// appleseed.foundation headers.
#include "foundation/string/string.h"

// Boost headers.
#include "boost/version.hpp"

// Embree headers.
#ifdef APPLESEED_WITH_EMBREE
#include <embree3/rtcore.h>
#endif

// IlmBase headers.
#include "foundation/platform/_beginexrheaders.h"
#include "foundation/platform/_endexrheaders.h"

// LZ4 headers.
#include <lz4.h>

// OpenColorIO headers.
#ifdef APPLESEED_WITH_OCIO
#include <OpenColorIO/OpenColorABI.h>
#endif

// OpenEXR headers.
#include "foundation/platform/_beginexrheaders.h"
#include <OpenEXR/OpenEXRConfig.h>
#include "foundation/platform/_endexrheaders.h"

// OpenImageIO headers.
#include "foundation/platform/_beginoiioheaders.h"
#include <OpenImageIO/oiioversion.h>
#include "foundation/platform/_endoiioheaders.h"

// OptiX headers.
#ifdef APPLESEED_WITH_GPU
#include <optix.h>
#endif

// OSL headers.
#include "foundation/platform/_beginoslheaders.h"
#include <OSL/oslversion.h>
#include "foundation/platform/_endoslheaders.h"

// Xerces-C++ headers.
#include <xercesc/util/XercesVersion.hpp>

// zlib headers.
#include <zlib.h>

namespace foundation
{

APPLESEED_DEFINE_APIARRAY(LibraryVersionArray);


//
// ThirdParties class implementation.
//

LibraryVersionArray ThirdParties::get_versions()
{
    static const char* BCDVersion = "v1.1";
    static const char* LibJpegTurboVersion = "1.3.1";
    static const char* LibTIFFVersion = "4.0.3";

    LibraryVersionArray versions;

    // Keep entries sorted alphabetically.

    versions.push_back(APIStringPair("BCD", BCDVersion));
    versions.push_back(APIStringPair("Boost", format("{0}.{1}.{2}", BOOST_VERSION / 100000, (BOOST_VERSION / 100) % 1000, BOOST_VERSION % 100)));

#ifdef APPLESEED_WITH_EMBREE
    versions.push_back(APIStringPair("Embree", RTC_VERSION_STRING));
#endif

#ifdef APPLESEED_WITH_GPU
    unsigned int optix_version;
    if (rtGetVersion(&optix_version) == RT_SUCCESS)
    {
        const unsigned int major = optix_version / 10000;
        const unsigned int minor = (optix_version % 10000) / 100;
        const unsigned int micro = optix_version % 100;
        versions.push_back(APIStringPair("OptiX", format("{0}.{1}.{2}", major, minor, micro)));
    }
#endif

#ifdef APPLESEED_WITH_OCIO
    versions.push_back(APIStringPair("OpenColorIO", OCIO_VERSION));
#endif

    versions.push_back(APIStringPair("libjpeg-turbo", LibJpegTurboVersion));
    versions.push_back(APIStringPair("LibTIFF", LibTIFFVersion));
    versions.push_back(APIStringPair("LZ4", format("{0}.{1}.{2}", LZ4_VERSION_MAJOR, LZ4_VERSION_MINOR, LZ4_VERSION_RELEASE)));
    versions.push_back(APIStringPair("OpenEXR", OPENEXR_VERSION_STRING));
    versions.push_back(APIStringPair("OpenImageIO", OIIO_VERSION_STRING));
    versions.push_back(APIStringPair("OpenShadingLanguage", OSL_LIBRARY_VERSION_STRING));
    versions.push_back(APIStringPair("Xerces-C++", XERCES_FULLVERSIONDOT));
    versions.push_back(APIStringPair("zlib", ZLIB_VERSION));

    return versions;
}

}   // namespace foundation
