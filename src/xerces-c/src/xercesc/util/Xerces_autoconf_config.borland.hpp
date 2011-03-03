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
 * $Id: Xerces_autoconf_config.borland.hpp 834826 2009-11-11 10:03:53Z borisk $
 */

#if !defined(XERCESC_INCLUDE_GUARD_XERCES_AUTOCONFIG_CONFIG_HPP)
#define XERCESC_INCLUDE_GUARD_XERCES_AUTOCONFIG_CONFIG_HPP

//
// There are two primary xerces configuration header files:
//
//	Xerces_autoconf_config.hpp
//
//      For configuration of items that must be accessable
//	through public headers. This file has limited information
//	and carefully works to avoid collision of macro names, etc.
//
//	This file is included by XercesDefs.h.
//	This version of the file is specific for Borland C++
//	family of compilers
//
//	config.h
//
//      This file is not used with Borland C++; the macros
//	it would specify are instead hardcoded in the makefiles
//

#include <basetsd.h>

// ---------------------------------------------------------------------------
//  These defines have been hardcoded for the Borland C++ compilers
// ---------------------------------------------------------------------------
#undef XERCES_AUTOCONF
#undef XERCES_HAVE_SYS_TYPES_H
#undef XERCES_HAVE_INTTYPES_H

#define XERCES_S16BIT_INT   signed short
#define XERCES_U16BIT_INT   unsigned short
#define XERCES_S32BIT_INT   INT32
#define XERCES_U32BIT_INT   UINT32
#define XERCES_S64BIT_INT   INT64
#define XERCES_U64BIT_INT   UINT64

#define XERCES_XMLCH_T      wchar_t

#define XERCES_SIZE_T       SIZE_T
#define XERCES_SSIZE_T      SSIZE_T

#define XERCES_HAS_CPP_NAMESPACE    1
#define XERCES_STD_NAMESPACE        1
#define XERCES_NEW_IOSTREAMS        1
#undef XERCES_NO_NATIVE_BOOL
#define XERCES_LSTRSUPPORT          1

#ifdef XERCES_STATIC_LIBRARY
#define XERCES_PLATFORM_EXPORT
#define XERCES_PLATFORM_IMPORT
#else
#define XERCES_PLATFORM_EXPORT __declspec(dllexport)
#define XERCES_PLATFORM_IMPORT __declspec(dllimport)
#define DLL_EXPORT
#endif

#define XERCES_NO_MATCHING_DELETE_OPERATOR
#define XERCES_NEED_XMEMORY_VIRTUAL_DESTRUCTOR

// ---------------------------------------------------------------------------
//  XMLSize_t is the unsigned integral type.
// ---------------------------------------------------------------------------
typedef XERCES_SIZE_T				XMLSize_t;
typedef XERCES_SSIZE_T				XMLSSize_t;

// ---------------------------------------------------------------------------
//  Define our version of the XML character
// ---------------------------------------------------------------------------
typedef XERCES_XMLCH_T				XMLCh;

// ---------------------------------------------------------------------------
//  Define unsigned 16, 32, and 64 bit integers
// ---------------------------------------------------------------------------
typedef XERCES_U16BIT_INT			XMLUInt16;
typedef XERCES_U32BIT_INT			XMLUInt32;
typedef XERCES_U64BIT_INT			XMLUInt64;

// ---------------------------------------------------------------------------
//  Define signed 16, 32, and 64 bit integers
// ---------------------------------------------------------------------------
typedef XERCES_S16BIT_INT			XMLInt16;
typedef XERCES_S32BIT_INT			XMLInt32;
typedef XERCES_S64BIT_INT			XMLInt64;

// ---------------------------------------------------------------------------
//  XMLFilePos is the type used to represent a file position.
// ---------------------------------------------------------------------------
typedef XMLUInt64			        XMLFilePos;

// ---------------------------------------------------------------------------
//  XMLFileLoc is the type used to represent a file location (line/column).
// ---------------------------------------------------------------------------
typedef XMLUInt64			        XMLFileLoc;

// ---------------------------------------------------------------------------
//  Force on the Xerces debug token if it is on in the build environment
// ---------------------------------------------------------------------------
#if defined(_DEBUG)
#define XERCES_DEBUG
#endif

#endif
