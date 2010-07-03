/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/*
 * $Id: XercesDefs.hpp 568078 2007-08-21 11:43:25Z amassari $
 */


#if !defined(XERCESDEFS_HPP)
#define XERCESDEFS_HPP

// ---------------------------------------------------------------------------
//  Include the Xerces version information; this is kept in a separate file to
//  make modification simple and obvious. Updates to the version header file
// ---------------------------------------------------------------------------
#include    <xercesc/util/XercesVersion.hpp>


// ---------------------------------------------------------------------------
//  Include the header that does automatic sensing of the current platform
//  and compiler.
// ---------------------------------------------------------------------------
#include    <xercesc/util/AutoSense.hpp>

#define XERCES_Invalid_File_Handle 0

// ---------------------------------------------------------------------------
//  According to the platform we include a platform specific file. This guy
//  will set up any platform specific stuff, such as character mode.
// ---------------------------------------------------------------------------
#if defined(XML_WIN32)
#include    <xercesc/util/Platforms/Win32/Win32Defs.hpp>
#endif

#if defined(XML_CYGWIN)
#include    <xercesc/util/Platforms/Cygwin/CygwinDefs.hpp>
#endif

#if defined(XML_AIX)
#include    <xercesc/util/Platforms/AIX/AIXDefs.hpp>
#endif

#if defined(XML_SOLARIS)
#include    <xercesc/util/Platforms/Solaris/SolarisDefs.hpp>
#endif

#if defined(XML_OPENSERVER)
#include    <xercesc/util/Platforms/OpenServer/OpenServerDefs.hpp>
#endif

#if defined(XML_UNIXWARE)
#include    <xercesc/util/Platforms/UnixWare/UnixWareDefs.hpp>
#endif

#if defined(XML_HPUX)
#include    <xercesc/util/Platforms/HPUX/HPUXDefs.hpp>
#endif

#if defined(XML_IRIX)
#include    <xercesc/util/Platforms/IRIX/IRIXDefs.hpp>
#endif

#if defined(XML_INTERIX)
#include    <xercesc/util/Platforms/Interix/InterixDefs.hpp>
#endif

#if defined(XML_TANDEM)
#include    <xercesc/util/Platforms/Tandem/TandemDefs.hpp>
#endif

#if defined(XML_BEOS)
#include    <xercesc/util/Platforms/BeOS/BeOSDefs.hpp>
#endif

#if defined(XML_LINUX)
#include    <xercesc/util/Platforms/Linux/LinuxDefs.hpp>
#endif

#if defined(XML_FREEBSD)
#include    <xercesc/util/Platforms/FreeBSD/FreeBSDDefs.hpp>
#endif

#if defined(XML_OS390)
#include    <xercesc/util/Platforms/OS390/OS390Defs.hpp>
#endif

#if defined(XML_PTX)
#include    <xercesc/util/Platforms/PTX/PTXDefs.hpp>
#endif

#if defined(XML_OS2)
#include    <xercesc/util/Platforms/OS2/OS2Defs.hpp>
#endif

#if defined(XML_MACOS)
#include	<xercesc/util/Platforms/MacOS/MacOSDefs.hpp>
#endif

#if defined(XML_AS400)
#include	<xercesc/util/Platforms/OS400/OS400Defs.hpp>
#endif

#if defined(XML_TRU64)
#include	<xercesc/util/Platforms/Tru64/Tru64Defs.hpp>
#endif

#if defined(XML_QNX)
#include	<xercesc/util/Platforms/QNX/QNXDefs.hpp>
#endif

// ---------------------------------------------------------------------------
//  And now we subinclude a header according to the development environment
//  we are on. This guy defines for each platform some basic stuff that is
//  specific to the development environment.
// ---------------------------------------------------------------------------
#if defined(XML_VISUALCPP)
#include    <xercesc/util/Compilers/VCPPDefs.hpp>
#endif

#if defined(XML_CSET)
#include    <xercesc/util/Compilers/CSetDefs.hpp>
#endif

#if defined(XML_BORLAND)
#include    <xercesc/util/Compilers/BorlandCDefs.hpp>
#endif

#if defined(XML_SUNCC) || defined(XML_SUNCC5)
#include    <xercesc/util/Compilers/SunCCDefs.hpp>
#endif

#if defined(XML_SCOCC)
#include    <xercesc/util/Compilers/SCOCCDefs.hpp>
#endif

#if defined(XML_SOLARIS_KAICC)
#include    <xercesc/util/Compilers/SunKaiDefs.hpp>
#endif

#if defined(XML_HPUX_CC) || defined(XML_HPUX_aCC) || defined(XML_HPUX_KAICC)
#include    <xercesc/util/Compilers/HPCCDefs.hpp>
#endif

#if defined(XML_MIPSPRO_CC)
#include    <xercesc/util/Compilers/MIPSproDefs.hpp>
#endif

#if defined(XML_TANDEMCC)
#include    <xercesc/util/Compilers/TandemCCDefs.hpp>
#endif

#if defined(XML_GCC)
#include    <xercesc/util/Compilers/GCCDefs.hpp>
#endif

#if defined(XML_MVSCPP)
#include    <xercesc/util/Compilers/MVSCPPDefs.hpp>
#endif

#if defined(XML_IBMVAW32)
#include    <xercesc/util/Compilers/IBMVAW32Defs.hpp>
#endif

#if defined(XML_IBMVAOS2)
#include    <xercesc/util/Compilers/IBMVAOS2Defs.hpp>
#endif

#if defined(XML_METROWERKS)
#include	<xercesc/util/Compilers/CodeWarriorDefs.hpp>
#endif

#if defined(XML_PTX_CC)
#include	<xercesc/util/Compilers/PTXCCDefs.hpp>
#endif

#if defined(XML_AS400)
#include	<xercesc/util/Compilers/OS400SetDefs.hpp>
#endif

#if defined(XML_DECCXX)
#include	<xercesc/util/Compilers/DECCXXDefs.hpp>
#endif

#if defined(XML_QCC)
#include	<xercesc/util/Compilers/QCCDefs.hpp>
#endif

// ---------------------------------------------------------------------------
//  Some general typedefs that are defined for internal flexibility.
//
//  Note  that UTF16Ch is fixed at 16 bits, whereas XMLCh floats in size per
//  platform, to whatever is the native wide char format there. UCS4Ch is
//  fixed at 32 bits. The types we defined them in terms of are defined per
//  compiler, using whatever types are the right ones for them to get these
//  16/32 bit sizes.
//
// ---------------------------------------------------------------------------
typedef unsigned char       XMLByte;
typedef XMLUInt16           UTF16Ch;
typedef XMLUInt32           UCS4Ch;


// ---------------------------------------------------------------------------
//  Handle boolean. If the platform can handle booleans itself, then we
//  map our boolean type to the native type. Otherwise we create a default
//  one as an int and define const values for true and false.
//
//  This flag will be set in the per-development environment stuff above.
// ---------------------------------------------------------------------------
#if defined(NO_NATIVE_BOOL)
  #ifndef bool
    typedef int     bool;
  #endif
  #ifndef true
    #define  true     1
  #endif
  #ifndef false
    #define false 0
  #endif
#endif

#if defined(XML_NETBSD)
#include       <xercesc/util/Platforms/NetBSD/NetBSDDefs.hpp>
#endif

// ---------------------------------------------------------------------------
//  According to whether the compiler suports L"" type strings, we define
//  the XMLStrL() macro one way or another.
// ---------------------------------------------------------------------------
#if defined(XML_LSTRSUPPORT)
#define XMLStrL(str)  L##str
#else
#define XMLStrL(str)  str
#endif


// ---------------------------------------------------------------------------
// Define namespace symbols if the compiler supports it.
// ---------------------------------------------------------------------------
#if defined(XERCES_HAS_CPP_NAMESPACE)
    #define XERCES_CPP_NAMESPACE_BEGIN namespace XERCES_CPP_NAMESPACE {
    #define XERCES_CPP_NAMESPACE_END  }
    #define XERCES_CPP_NAMESPACE_USE using namespace XERCES_CPP_NAMESPACE;
    #define XERCES_CPP_NAMESPACE_QUALIFIER XERCES_CPP_NAMESPACE::

    namespace XERCES_CPP_NAMESPACE { }
    namespace xercesc = XERCES_CPP_NAMESPACE;
#else
    #define XERCES_CPP_NAMESPACE_BEGIN
    #define XERCES_CPP_NAMESPACE_END
    #define XERCES_CPP_NAMESPACE_USE
    #define XERCES_CPP_NAMESPACE_QUALIFIER
#endif

#if defined(XERCES_STD_NAMESPACE)
	#define XERCES_USING_STD(NAME) using std :: NAME;
	#define XERCES_STD_QUALIFIER  std ::
#else
	#define XERCES_USING_STD(NAME)
	#define XERCES_STD_QUALIFIER 
#endif


// ---------------------------------------------------------------------------
//  Set up the import/export keyword  for our core projects. The
//  PLATFORM_XXXX keywords are set in the per-development environment
//  include above.
// ---------------------------------------------------------------------------
#if defined(PROJ_XMLUTIL)
#define XMLUTIL_EXPORT PLATFORM_EXPORT
#else
#define XMLUTIL_EXPORT PLATFORM_IMPORT
#endif

#if defined(PROJ_XMLPARSER)
#define XMLPARSER_EXPORT PLATFORM_EXPORT
#else
#define XMLPARSER_EXPORT PLATFORM_IMPORT
#endif

#if defined(PROJ_SAX4C)
#define SAX_EXPORT PLATFORM_EXPORT
#else
#define SAX_EXPORT PLATFORM_IMPORT
#endif

#if defined(PROJ_SAX2)
#define SAX2_EXPORT PLATFORM_EXPORT
#else
#define SAX2_EXPORT PLATFORM_IMPORT
#endif

#if defined(PROJ_DOM)
#define CDOM_EXPORT PLATFORM_EXPORT
#else
#define CDOM_EXPORT PLATFORM_IMPORT
#endif

#if defined(PROJ_DEPRECATED_DOM)
#define DEPRECATED_DOM_EXPORT PLATFORM_EXPORT
#else
#define DEPRECATED_DOM_EXPORT PLATFORM_IMPORT
#endif

#if defined(PROJ_PARSERS)
#define PARSERS_EXPORT  PLATFORM_EXPORT
#else
#define PARSERS_EXPORT  PLATFORM_IMPORT
#endif

#if defined(PROJ_VALIDATORS)
#define VALIDATORS_EXPORT  PLATFORM_EXPORT
#else
#define VALIDATORS_EXPORT  PLATFORM_IMPORT
#endif

#endif
