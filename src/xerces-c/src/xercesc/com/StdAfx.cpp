// stdafx.cpp : source file that includes just the standard includes
//  stdafx.pch will be the pre-compiled header
//  stdafx.obj will contain the pre-compiled type information

#include "stdafx.h"

#if defined(_ATL_STATIC_REGISTRY)
#include <statreg.h>
#if defined(_MSC_VER) && _MSC_VER < 1300
#include <statreg.cpp>
#endif
#endif

#if defined(_MSC_VER) && _MSC_VER < 1300
#include <atlimpl.cpp>
#endif

//
//   This macro is defined in MSXML.H's compatible with IE5
//      and not defined in those from IE4.
//
//   To correct, install a IE5 or later version of the Microsoft Platform SDK
//      and add \Program Files\Microsoft Platform SDK\Include as the first entry
//      on the Directories tab on the dialog displayed after selecting Tools Options
//      from the Visual Studio IDE.
//
#ifndef __IXMLDOMNode_INTERFACE_DEFINED__
#error "xerces-dom requires an MSXML.H compatible with IE5 or later.  See http://xerces.apache.org/xerces-c/build.html#BuildCOM for directions to correct this problem."
#endif

