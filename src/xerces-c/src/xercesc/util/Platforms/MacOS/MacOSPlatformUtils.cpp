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
 * $Id: MacOSPlatformUtils.cpp 568078 2007-08-21 11:43:25Z amassari $
 */


// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <cstring>
#include <cstdlib>
#include <cctype>
#include <cstdio>
#include <memory>
#include <algorithm>
#include <unistd.h>

#if defined(__APPLE__)
    //	Include from Frameworks Headers under ProjectBuilder
    #include <Carbon/Carbon.h>
#else
    //	Classic include styles
    #include <Files.h>
    #include <Gestalt.h>
    #include <TextUtils.h>
    #include <TextEncodingConverter.h>
    #include <Multiprocessing.h>
    #include <DriverSynchronization.h>
    #include <DriverServices.h>
    #include <CFString.h>
    #include <URLAccess.h>
#endif

#include <xercesc/util/XercesDefs.hpp>
#include <xercesc/util/Janitor.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/RuntimeException.hpp>
#include <xercesc/util/XMLUniDefs.hpp>
#include <xercesc/util/XMLUni.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/Platforms/MacOS/MacOSPlatformUtils.hpp>
#include <xercesc/util/Platforms/MacOS/MacCarbonFile.hpp>
#include <xercesc/util/Platforms/MacOS/MacPosixFile.hpp>
#include <xercesc/util/PanicHandler.hpp>
#include <xercesc/util/OutOfMemoryException.hpp>

#if (defined(XML_USE_INMEMORY_MSGLOADER) || defined(XML_USE_INMEM_MESSAGELOADER))
   #include <xercesc/util/MsgLoaders/InMemory/InMemMsgLoader.hpp>
#endif

#if defined(XML_USE_ICU_TRANSCODER)
   #include <xercesc/util/Transcoders/ICU/ICUTransService.hpp>
#elif (defined(XML_USE_MACOS_UNICODECONVERTER) || defined(XML_USE_NATIVE_TRANSCODER))
   #include <xercesc/util/Transcoders/MacOSUnicodeConverter/MacOSUnicodeConverter.hpp>
#endif

//	Make up our minds about which netaccessor we'll use
#if (defined(XML_USE_NETACCESSOR_URLACCESSCF) || (defined(XML_USE_NETACCESSOR_NATIVE) && TARGET_API_MAC_CARBON))
    #define USE_URLACCESSCF
#elif (defined(XML_USE_NETACCESSOR_URLACCESS) || (defined(XML_USE_NETACCESSOR_NATIVE) && !TARGET_API_MAC_CARBON))
    #define USE_URLACCESS
#endif

#if defined(USE_URLACCESSCF)
   #include <xercesc/util/NetAccessors/MacOSURLAccessCF/MacOSURLAccessCF.hpp>
#elif defined(USE_URLACCESS)
   #include <xercesc/util/NetAccessors/MacOSURLAccess/MacOSURLAccess.hpp>
#elif defined(XML_USE_NETACCESSOR_SOCKET)
   #include <xercesc/util/NetAccessors/Socket/SocketNetAccessor.hpp>
#endif

XERCES_CPP_NAMESPACE_BEGIN

//----------------------------------------------------------------------------
// Function Prototypes
//----------------------------------------------------------------------------
XMLCh*	ConvertColonToSlash(XMLCh* p, std::size_t charCount);
XMLCh*	ConvertSlashToColon(XMLCh* p, std::size_t charCount);
char*	ConvertSlashToColon(char* p, std::size_t charCount);

XMLCh*	XMLCreateFullPathFromFSRef_X(const FSRef& startingRef, MemoryManager* const manager);
XMLCh*	XMLCreateFullPathFromFSRef_Classic(const FSRef& startingRef, MemoryManager* const manager);
XMLCh*	XMLCreateFullPathFromFSSpec_Classic(const FSSpec& startingSpec,
                                            MemoryManager* const manager);
bool	XMLParsePathToFSRef_X(const XMLCh* const pathName, FSRef& ref, MemoryManager* const manager);
bool	XMLParsePathToFSRef_Classic(const XMLCh* const pathName, FSRef& ref, MemoryManager* const manager);
bool	XMLParsePathToFSSpec_Classic(const XMLCh* const pathName, FSSpec& spec, MemoryManager* const manager);


//----------------------------------------------------------------------------
//  Local Data
//
//  gFileSystemCompatible
//   This flag indicates whether the file system APIs meet our minimum
//   requirements.
//
//  gMacOSXOrBetter
//   The system version is >= 10.
//
// gHasFSSpecAPIs
//   True if the FSSpecAPIs are available. These are required.
//
// gHasF2TBAPIs
//   True if the FS supports 2 terrabyte calls. These are required for
//   use under Carbon.
//
// gHasHFSPlusAPIs
//   True if the FS supports HFSPlus APIs. If this is true, then the
//   new Fork calls are used, and all file name and path handling
//   uses long unicode names. Note that once a file is opened with
//   the fork calls, only fork calls may be used to access it.
//
// gHasFSPathAPIs
//   True if the FS supports path creation APIs FSPathMakeRef and
//	 FSRefMakePath.
//
// gPathAPIsUsePosixPaths
//   True if the path creation APIs FSPathMakeRef and FSRefMakePath
//	 use posix, rather than HFS, style paths. If so, these routines
//	 are used to support path creation and  interpretation.
//
// gHasMPAPIs
//	 True if the Multiprocessing APIs are available.
//
// gUsePosixFiles
//   True if we're using XMLMacPosixFile rather than XMLMacCarbonFile.
//
// gUseGETCWD
//   True if we can rely on getcwd to get the current directory path.
//----------------------------------------------------------------------------
bool gFileSystemCompatible	= false;
bool gMacOSXOrBetter		= false;
bool gHasFSSpecAPIs			= false;
bool gHasFS2TBAPIs			= false;
bool gHasHFSPlusAPIs		= false;
bool gHasFSPathAPIs			= false;
bool gPathAPIsUsePosixPaths	= false;
bool gHasMPAPIs				= false;
bool gUsePosixFiles			= false;
bool gUseGETCWD				= false;


// ---------------------------------------------------------------------------
//  XMLPlatformUtils: The panic method
// ---------------------------------------------------------------------------
void 
XMLPlatformUtils::panic(const PanicHandler::PanicReasons reason)
{
    if (fgUserPanicHandler)
		fgUserPanicHandler->panic(reason);
	else
		fgDefaultPanicHandler->panic(reason);
}


// ---------------------------------------------------------------------------
//  XMLPlatformUtils: File Methods
// ---------------------------------------------------------------------------
unsigned int
XMLPlatformUtils::curFilePos(const FileHandle theFile
                             , MemoryManager* const manager)
{
	return reinterpret_cast<XMLMacAbstractFile*>(theFile)->currPos();
}

void
XMLPlatformUtils::closeFile(const FileHandle theFile
                            , MemoryManager* const manager)
{
    reinterpret_cast<XMLMacAbstractFile*>(theFile)->close();
	delete reinterpret_cast<XMLMacAbstractFile*>(theFile);
}

unsigned int
XMLPlatformUtils::fileSize(const FileHandle theFile
                           , MemoryManager* const manager)
{
    return reinterpret_cast<XMLMacAbstractFile*>(theFile)->size();
}


FileHandle
XMLPlatformUtils::openFile(const char* const fileName
                           , MemoryManager* const manager)
{
    // Check to make sure the file system is in a state where we can use it
    if (!gFileSystemCompatible)
        ThrowXMLwithMemMgr1(XMLPlatformUtilsException, XMLExcepts::File_CouldNotOpenFile, fileName, manager);

    Janitor<XMLMacAbstractFile> file(XMLMakeMacFile(manager));
    
    return (file->open(fileName, false)) ? file.release() : NULL;
}


FileHandle
XMLPlatformUtils::openFile(const XMLCh* const fileName, MemoryManager* const manager)
{
    // Check to make sure the file system is in a state where we can use it
    if (!gFileSystemCompatible)
        ThrowXMLwithMemMgr1(XMLPlatformUtilsException, XMLExcepts::File_CouldNotOpenFile, fileName, manager);

    Janitor<XMLMacAbstractFile> file(XMLMakeMacFile(manager));

    return (file->open(fileName, false)) ? file.release() : NULL;
}


FileHandle
XMLPlatformUtils::openFileToWrite(const char* const fileName
                                  , MemoryManager* const manager)
{
    // Check to make sure the file system is in a state where we can use it
    if (!gFileSystemCompatible)
        ThrowXMLwithMemMgr1(XMLPlatformUtilsException, XMLExcepts::File_CouldNotOpenFile, fileName, manager);

    Janitor<XMLMacAbstractFile> file(XMLMakeMacFile(manager));

    return (file->open(fileName, true)) ? file.release() : NULL;
}


FileHandle
XMLPlatformUtils::openFileToWrite(const XMLCh* const fileName
                                  , MemoryManager* const manager)
{
    // Check to make sure the file system is in a state where we can use it
    if (!gFileSystemCompatible)
        ThrowXMLwithMemMgr1(XMLPlatformUtilsException, XMLExcepts::File_CouldNotOpenFile, fileName, manager);

    Janitor<XMLMacAbstractFile> file(XMLMakeMacFile(manager));

    return (file->open(fileName, true)) ? file.release() : NULL;
}


FileHandle
XMLPlatformUtils::openStdInHandle(MemoryManager* const manager)
{
    XMLCh stdinStr[] = {chLatin_s, chLatin_t, chLatin_d, chLatin_i, chLatin_n, chNull};
    ThrowXMLwithMemMgr1(XMLPlatformUtilsException, XMLExcepts::File_CouldNotOpenFile, stdinStr, manager);
    return NULL;
}


unsigned int
XMLPlatformUtils::readFileBuffer(   const FileHandle      theFile
                                 ,  const unsigned int    toRead
                                 ,        XMLByte* const  toFill
                                 ,  MemoryManager* const  manager)
{
    return reinterpret_cast<XMLMacAbstractFile*>(theFile)->read(toRead, toFill);
}


void
XMLPlatformUtils::writeBufferToFile(   const   FileHandle   theFile
                                    ,  const long		    toWrite
                                    ,  const XMLByte* const toFlush
                                    ,  MemoryManager* const manager)
{
    return reinterpret_cast<XMLMacAbstractFile*>(theFile)->write(toWrite, toFlush);
}


void
XMLPlatformUtils::resetFile(FileHandle theFile
                            , MemoryManager* const manager)
{
    reinterpret_cast<XMLMacAbstractFile*>(theFile)->reset();
}


// ---------------------------------------------------------------------------
//  XMLPlatformUtils: File system methods
// ---------------------------------------------------------------------------
XMLCh*
XMLPlatformUtils::getFullPath(const XMLCh* const srcPath,
                              MemoryManager* const manager)
{
    XMLCh* path = NULL;

    if (gHasHFSPlusAPIs)
    {
        FSRef ref;
        if (   !XMLParsePathToFSRef(srcPath, ref, manager)
			|| (path = XMLCreateFullPathFromFSRef(ref, manager)) == NULL
		   )
            path = XMLString::replicate(srcPath, manager);
    }
    else
    {
        FSSpec spec;
        if (   !XMLParsePathToFSSpec(srcPath, spec, manager)
		    || (path = XMLCreateFullPathFromFSSpec(spec, manager)) == NULL
		   )
            path = XMLString::replicate(srcPath, manager);
    }

    return path;
}


bool
XMLPlatformUtils::isRelative(const XMLCh* const toCheck
                             , MemoryManager* const manager)
{
    return (toCheck[0] != L'/');
}


XMLCh* XMLPlatformUtils::getCurrentDirectory(MemoryManager* const manager)
{
	//	Get a newly allocated path to the current directory
	FSSpec spec;

	//  Parse to path to determine current directory: this allows the
	//  path parsing routines to determine best way to find the current
	//  directory.
	XMLCh curDirPath[] = { '.', 0 };
	XMLCh* path =
		(XMLParsePathToFSSpec(curDirPath, spec, manager))
			? XMLCreateFullPathFromFSSpec(spec, manager)
			: NULL;
			
    if (!path)
		 ThrowXMLwithMemMgr(XMLPlatformUtilsException,
	           XMLExcepts::File_CouldNotGetBasePathName, manager);
	           
	return path;
}


inline bool XMLPlatformUtils::isAnySlash(XMLCh c) 
{
	//	We support only forward slash as a path delimiter
    return (chForwardSlash == c);
}


// ---------------------------------------------------------------------------
//  XMLPlatformUtils: Timing Methods
// ---------------------------------------------------------------------------
unsigned long
XMLPlatformUtils::getCurrentMillis()
{
	if ((void*)kUnresolvedCFragSymbolAddress != UpTime)
	{
		// Use Driver services routines, now in Carbon,
		// to get the elapsed milliseconds.
		AbsoluteTime time = UpTime();
		return AbsoluteToDuration(time);
	}
	else
		return TickCount() * 100 / 6;
}


// ---------------------------------------------------------------------------
//  Mutex methods
//
// There are a number of choices for multi-threading on Mac OS. Traditionally
// there was the Thread Manager, which provided cooperative multitasking on
// 68K and PPC platforms, and preemptive multitasking on 68K platforms only.
// The primary threading model supported under Carbon is the Multiprocessing
// library, which as of version 2.0 provides a nice set of primitives. Under
// Mac OS X, the Multiprocessing library is a thin veneer over pthreads.
//
// For lack of any really universal solutions, I've implemented these mutexes
// atop the Multiprocessing library. The critical regions employed here
// support recursive behavior, which is required by Xerces.
//
// Please note that, despite this implementation, there may be other barriers
// to using Xerces in a multithreaded environment. The memory allocator
// under Mac OS 9, for instance, is not thread safe.
// ---------------------------------------------------------------------------

void*
XMLPlatformUtils::makeMutex(MemoryManager*)
{
	if (gHasMPAPIs)
	{
		MPCriticalRegionID criticalRegion = NULL;
		OSStatus status = MPCreateCriticalRegion(&criticalRegion);
		return (status == noErr) ? (void*)(criticalRegion) : NULL;
	}
	else
		return (void*)1;
}


void
XMLPlatformUtils::closeMutex(void* const mtxHandle)
{
	if (gHasMPAPIs)
	{
		MPCriticalRegionID criticalRegion = reinterpret_cast<MPCriticalRegionID>(mtxHandle);
		OSStatus status = MPDeleteCriticalRegion(criticalRegion);
		status = noErr;	// ignore any error and zap compiler warning
	}
	else
		;
}


void
XMLPlatformUtils::lockMutex(void* const mtxHandle)
{
	if (gHasMPAPIs)
	{
		MPCriticalRegionID criticalRegion = reinterpret_cast<MPCriticalRegionID>(mtxHandle);
		OSStatus status = MPEnterCriticalRegion(criticalRegion, kDurationForever);
		status = noErr;	// ignore any error and zap compiler warning
	}
	else
		;
}


void
XMLPlatformUtils::unlockMutex(void* const mtxHandle)
{
	if (gHasMPAPIs)
	{
		MPCriticalRegionID criticalRegion = reinterpret_cast<MPCriticalRegionID>(mtxHandle);
		OSStatus status = MPExitCriticalRegion(criticalRegion);
		status = noErr;	// ignore any error and zap compiler warning
	}
	else
		;
}


// ---------------------------------------------------------------------------
//  Miscellaneous synchronization methods
//
// Atomic manipulation is implemented atop routines that were traditionally
// part of DriverServices, but are now a part of Carbon.
// ---------------------------------------------------------------------------

void*
XMLPlatformUtils::compareAndSwap(       void**      toFill
                                 , const void* const newValue
                                 , const void* const toCompare)
{
    // Replace *toFill with newValue iff *toFill == toCompare,
    // returning previous value of *toFill

    Boolean success = CompareAndSwap(
        reinterpret_cast<UInt32>(toCompare),
        reinterpret_cast<UInt32>(newValue),
        reinterpret_cast<UInt32*>(toFill));

    return (success) ? const_cast<void*>(toCompare) : *toFill;
}


//
//	Atomic Increment and Decrement
//
//	Apple's routines return the value as it was before the
//	operation, while these routines want to return it as it
//	is after. So we perform the translation before returning
//	the value.
//
int
XMLPlatformUtils::atomicIncrement(int &location)
{
    return IncrementAtomic(reinterpret_cast<long*>(&location)) + 1;
}


int
XMLPlatformUtils::atomicDecrement(int &location)
{
    return DecrementAtomic(reinterpret_cast<long*>(&location)) - 1;
}


// ---------------------------------------------------------------------------
//  XMLPlatformUtils: Private Static Methods
// ---------------------------------------------------------------------------

//
//  This method handles the MacOS basic init functions.
//
void
XMLPlatformUtils::platformInit()
{
	long value = 0;

	//	Detect available functions
	
	//  Check whether we're on OS X
	gMacOSXOrBetter			= noErr == Gestalt(gestaltSystemVersion, &value)
							  && value >= 0x00001000
							  ;
	
    //	Look for file system services
    if (noErr == Gestalt(gestaltFSAttr, &value))
    {
        gHasFSSpecAPIs		= (value & (1 << gestaltHasFSSpecCalls)) != 0;
        gHasFS2TBAPIs		= (value & (1 << gestaltFSSupports2TBVols)) != 0;
        gHasHFSPlusAPIs		= (value & (1 << gestaltHasHFSPlusAPIs)) != 0;

        #if TARGET_API_MAC_CARBON
        gHasFSPathAPIs		= ((void*)kUnresolvedCFragSymbolAddress != FSPathMakeRef);
        #else
        gHasFSPathAPIs		= false;
        #endif

        gPathAPIsUsePosixPaths = gHasFSPathAPIs
								 && (value & (1 << gestaltFSUsesPOSIXPathsForConversion));
    }

	//	We require FSSpecs at a minimum
    gFileSystemCompatible	= gHasFSSpecAPIs;

	//	Determine which file system to use (posix or carbon file access)
	//	If we're using Metrowerks MSL, we surely don't want posix paths,
	//	as MSL doesn't use them.
	#if __MSL__ && (__MSL__ < 0x08000 || _MSL_CARBON_FILE_APIS)
	gUsePosixFiles			= false;
	#else
	gUsePosixFiles			= gMacOSXOrBetter;
	#endif
	
	//  Determine whether to use getcwd or not. We use it only if we're not using MSL,
	//  and we're on a Mac OS X system.
	#if __MSL__
	gUseGETCWD				= false;
	#else
	gUseGETCWD				= gMacOSXOrBetter;
	#endif

    //	Look for MP
	gHasMPAPIs				= MPLibraryIsLoaded();
}


//
//  This method handles the MacOS basic termination functions.
//
void
XMLPlatformUtils::platformTerm()
{
}


// ---------------------------------------------------------------------------
//  XMLPlatformUtils: Private Static Methods
// ---------------------------------------------------------------------------

//
//  This method is called by the platform independent part of this class
//  during initialization. We have to create the type of net accessor that
//  we want to use. If none, then just return zero.
//
XMLNetAccessor*
XMLPlatformUtils::makeNetAccessor()
{
    //	The selection of NetAcessor is made through
    //	the following preprocessor defines:
    //
    //	XML_USE_NETACCESSOR_URLACCESS		-- Use netaccessor based on URLAccess
    //	XML_USE_NETACCESSOR_URLACCESSCF		-- Use netaccessor based on CFURLAccess (CoreFoundation based)
    //	XML_USE_NETACCESSOR_NATIVE			-- In absence of above selections, chooses URLACCESSCF
    //										   if targetting Carbon, and URLAccess otherwise
    //	XML_USE_NETACCESSOR_SOCKET			-- Use the sockets based netaccessor
    //
    //	These choices are resolved at the ^^^top^^^ of this file.

#if (defined(USE_URLACCESSCF))
    //	Use the URLAccess code that relies only on CoreFoundation
	return new (fgMemoryManager) MacOSURLAccessCF;
#elif (defined(USE_URLACCESS))
	//	Only try to use URLAccess if it's actually available
	if (URLAccessAvailable())
		return new (fgMemoryManager) MacOSURLAccess;
#elif (defined(XML_USE_NETACCESSOR_SOCKET))
	return new (fgMemoryManager) SocketNetAccessor;
#endif

	//	No netaccessor available--we can live with it, but you won't
	//	get net access.
	return 0;
}


//
//  This method is called by the platform independent part of this class
//  when client code asks to have one of the supported message sets loaded.
//
XMLMsgLoader*
XMLPlatformUtils::loadAMsgSet(const XMLCh* const msgDomain)
{
    XMLMsgLoader* retVal;
    try
    {
#if (defined(XML_USE_INMEMORY_MSGLOADER) || defined(XML_USE_INMEM_MESSAGELOADER))
        retVal = new (fgMemoryManager) InMemMsgLoader(msgDomain);
#else
        #error You must provide a message loader
        return 0;
#endif
    }
    catch(const OutOfMemoryException&)
    {
        throw;
    }
    catch(...)
    {
         panic(PanicHandler::Panic_CantLoadMsgDomain);
    }
    return retVal;
}


//
//  This method is called very early in the bootstrapping process. This guy
//  must create a transcoding service and return it. It cannot use any string
//  methods, any transcoding services, throw any exceptions, etc... It just
//  makes a transcoding service and returns it, or returns zero on failure.
//
XMLTransService*
XMLPlatformUtils::makeTransService()
{
#if defined (XML_USE_ICU_TRANSCODER)
	return new (fgMemoryManager) ICUTransService;
#elif (defined(XML_USE_MACOS_UNICODECONVERTER) || defined(XML_USE_NATIVE_TRANSCODER))
    if (MacOSUnicodeConverter::IsMacOSUnicodeConverterSupported())
        return new (fgMemoryManager) MacOSUnicodeConverter;
#else
    #error You must provide a transcoding service implementation
#endif

    //	If we got here it's because we didn't detect the Mac OS
    //	Unicode Converter or Text Encoding Converter routines
    //	that we require to function properly. Xerces will not
    //	survive this condition.
    return NULL;
}


// ---------------------------------------------------------------------------
//	Utility Functions
// ---------------------------------------------------------------------------

XMLCh*
CopyUniCharsToXMLChs(const UniChar* src, XMLCh* dst, std::size_t charCount, std::size_t maxChars)
{
	//	Ensure we don't step on anybody's toes
	std::size_t cnt = std::min(charCount, maxChars);
	
	//	Copy the characters. UniChar is unsigned, so we shouldn't have
	//	any sign extension problems.
	//	To allow copy within a identical range, we copy backwards,
	//	since XMLCh (may be) larger than UniChar.
	dst += cnt;
	src += cnt;
	for (; cnt > 0; --cnt)
		*--dst = *--src;
		
	return dst;
}


UniChar*
CopyXMLChsToUniChars(const XMLCh* src, UniChar* dst, std::size_t charCount, std::size_t maxChars)
{
	UniChar* dstBegin = dst;
	
	//	Ensure we don't step on anybody's toes
	std::size_t cnt = std::min(charCount, maxChars);
	
	//	Copy the characters. XMLCh's will be truncated on copy to UniChar's.
	//	To allow copy within a identical range, we copy forwards,
	//	since XMLCh (may be) larger than UniChar.
	for (; cnt > 0; --cnt)
		*dst++ = *src++;
		
	return dstBegin;
}


XMLCh*
ConvertColonToSlash(XMLCh* p, std::size_t charCount)
{
	XMLCh* start = p;
	for (; charCount > 0; --charCount)
	{
		XMLCh c = *p;
		if (c == ':')
			*p++ = '/';
		else
			p++;
	}
	return start;
}


XMLCh*
ConvertSlashToColon(XMLCh* p, std::size_t charCount)
{
	XMLCh* start = p;
	for (; charCount > 0; --charCount)
	{
		XMLCh c = *p;
		if (c == '/')
			*p++ = ':';
		else
			p++;
	}
	return start;
}


char*
ConvertSlashToColon(char* p, std::size_t charCount)
{
	char* start = p;
	for (; charCount > 0; --charCount)
	{
		char c = *p;
		if (c == '/')
			*p++ = ':';
		else
			p++;
	}
	return start;
}


//	Factory method to make an appropriate subclass of XMLMacAbstractFile
//	for our use
XMLMacAbstractFile*
XMLMakeMacFile(MemoryManager* manager)
{
	XMLMacAbstractFile* result = NULL;
	
	if (gUsePosixFiles)
		result = new (manager) XMLMacPosixFile;
	else
		result = new (manager) XMLMacCarbonFile;
		
	return result;
}


bool
XMLParsePathToFSRef(const XMLCh* const pathName, FSRef& ref, MemoryManager* const manager)
{
	bool result;
	
	//	If FSPathMakeRef is available, we use it to parse the
	//	path: this gives us "standard" path support under MacOS X.
	//	Without this, our paths in that environment would always
	//	have a volume name at their root...which would look
	//	"normal" to Mac users, but not normal to unix users. Since
	//	we're making "unix" paths, we'll stick with the unix
	//	style paths. This also allows us to easilly take paths
	//	off the command line.
	//
	//	FSPathMakeRef is available on Mac OS X and in CarbonLib 1.1
	//	and greater. But on classic under CarbonLib, it expects paths
	//	to contain ':' separators, for which we're not prepared. Since
	//	this isn't a case where we need to use it, we drop back to the
	//	classic case for this.
		
	if (TARGET_API_MAC_CARBON && gHasFSPathAPIs && gPathAPIsUsePosixPaths)
		result = XMLParsePathToFSRef_X(pathName, ref, manager);
	else
		result = XMLParsePathToFSRef_Classic(pathName, ref, manager);
		
	return result;
}


bool
XMLParsePathToFSRef_X(const XMLCh* const pathName, FSRef& ref, MemoryManager* const manager)
{
	//	Parse Path to FSRef using FSPathMakeRef as available under
	//	Mac OS X and CarbonLib 1.1 and greater.
	
	OSStatus err = noErr;	
	std::size_t pathLen = XMLString::stringLen(pathName);

    //	Transcode XMLCh into UniChar
	UniChar uniBuf[kMaxMacStaticPathChars];
	CopyXMLChsToUniChars(pathName, uniBuf, pathLen, kMaxMacStaticPathChars);
	
	//	Transcode Unicode to UTF-8
	char utf8Buf[kMaxMacStaticPathChars];
	pathLen = TranscodeUniCharsToUTF8(uniBuf, utf8Buf, pathLen, kMaxMacStaticPathChars-1);
	
	//	Terminate the path
	char* p = utf8Buf;
	p[pathLen++] = '\0';
	
	//	If it's a relative path, pre-pend the current directory to the path.
	//	FSPathMakeRef doesn't deal with relativity on the front of the path
	if (*p != '/' && kMaxMacStaticPathChars > pathLen)
	{
		//	Right justify the user path to make room for the pre-pended path
		std::memmove(p + kMaxMacStaticPathChars - pathLen, p, pathLen);
		*p = '\0';
				
		//	Get the current directory
		if (gUseGETCWD)
		{
			//	Get current directory path, leaving room for one '/' after
			if (err == noErr)
				getcwd(p, kMaxMacStaticPathChars - pathLen - 1);
		}
		else
		{
			//	Get current directory path, leaving room for one '/' after

			//	We quiz the carbon file manager for the current directory.
			//	Note that carbon defaults its concept of the current directory
			//  to the location of the executable.
	        FSSpec spec;
			if (err == noErr)
				err = FSMakeFSSpec(0, 0, NULL, &spec);
	        if (err == noErr)
	            err = FSpMakeFSRef(&spec, &ref);
			
			//	Get current directory path, leaving room for one '/' after
			if (err == noErr)
				err = FSRefMakePath(&ref, reinterpret_cast<UInt8*>(p), kMaxMacStaticPathChars - pathLen - 1);
		}
					
		//	Now munge the two paths back together
		std::size_t prefixLen = std::strlen(p);
		p[prefixLen++] = '/';
		std::memmove(p + prefixLen, p + kMaxMacStaticPathChars - pathLen, pathLen);
		
		//	We now have a path from an absolute starting point
	}
	
	//	Let the OS discover the location
	Boolean isDirectory = false;
	if (err == noErr)
		err = FSPathMakeRef(reinterpret_cast<UInt8*>(p), &ref, &isDirectory);
		
	//	Return true on success
	return (err == noErr);
}


bool
XMLParsePathToFSRef_Classic(const XMLCh* const pathName, FSRef& ref, MemoryManager* const manager)
{
	//	Parse Path to FSRef by stepping manually through path components.
	
	//	Path's parsed in this way must always begin with a volume name.
	//	This assumption would fail for standard unix paths under Mac OS X,
	//	so for those cases we use the routine XMLParsePathToFSRef_Carbon
	//	above.
	
    const XMLCh* p = pathName;
    const XMLCh* pEnd;
    std::size_t segLen;
	
	const std::size_t kXMLBufCount = 256;
	XMLCh xmlBuf[kXMLBufCount];

    OSErr err = noErr;

    if (*p == L'/')
    {
        // Absolute name: grab the first component as volume name

        // Find the end of the path segment
        for (pEnd = ++p; *pEnd && *pEnd != L'/'; ++pEnd) ;
        segLen = pEnd - p;

        // Try to find a volume that matches this name
        for (ItemCount volIndex = 1; err == noErr; ++volIndex)
        {
            HFSUniStr255 hfsStr;
            hfsStr.length = 0;

            // Get the volume name
            err = FSGetVolumeInfo(
                0,
                volIndex,
                static_cast<FSVolumeRefNum*>(NULL),
                0,
                static_cast<FSVolumeInfo*>(NULL),
                &hfsStr,
                &ref
                );

            // Compare against our path segment
            if (err == noErr && segLen == hfsStr.length)
            {
            	//	Case-insensitive compare
            	if (XMLString::compareNIString(
									ConvertSlashToColon(
										CopyUniCharsToXMLChs(hfsStr.unicode, xmlBuf, segLen, kXMLBufCount),
										segLen),
									p, segLen) == 0)
                    break;  // we found our volume
            }
        }

        p = pEnd;
    }
    else
    {
        // Relative name, so get the default directory as parent ref
        FSSpec spec;
        err = FSMakeFSSpec(0, 0, NULL, &spec);
        if (err == noErr)
            err = FSpMakeFSRef(&spec, &ref);
    }

    // ref now refers to the parent directory: parse the rest of the path
    while (err == noErr && *p)
    {
        switch (*p)
        {
        case L'/':   // Just skip any number of path separators
            ++p;
            break;

        case L'.':   // Potentially "current directory" or "parent directory"
            if (p[1] == L'/' || p[1] == 0)       // "current directory"
            {
                ++p;
                break;
            }
            else if (p[1] == L'.' && (p[2] == L'/' || p[2] == 0)) // "parent directory"
            {
                p += 2;  // Get the parent of our parent

                FSCatalogInfo catalogInfo;
                err = FSGetCatalogInfo(
                    &ref,
                    kFSCatInfoParentDirID,
                    &catalogInfo,
                    static_cast<HFSUniStr255*>(NULL),
                    static_cast<FSSpec*>(NULL),
                    &ref
                    );

                // Check that we didn't go too far
                if (err != noErr || catalogInfo.parentDirID == fsRtParID)
                    return false;

                break;
            }
            else // some other sequence of periods...fall through and treat as segment
                ;

        default:
            // Find the end of the path segment
            for (pEnd = p; *pEnd && *pEnd != L'/'; ++pEnd) ;
            segLen = pEnd - p;
			
            // pEnd now points either to '/' or NUL
            // Create a new ref using this path segment
            err = FSMakeFSRefUnicode(
                &ref,
                segLen,
                ConvertColonToSlash(
                	CopyXMLChsToUniChars(p, reinterpret_cast<UniChar*>(xmlBuf), segLen, kXMLBufCount),
                	segLen),
                kTextEncodingUnknown,
                &ref
                );

            p = pEnd;
            break;
        }
    }

    return err == noErr;
}


bool
XMLParsePathToFSSpec(const XMLCh* const pathName, FSSpec& spec,
                            MemoryManager* const manager)
{
	//	Parse Path to an FSSpec

	//	If we've got HFS+ APIs, do this in terms of refs so that
	//	we can grandfather in the use of FSPathMakeRef under Mac OS X
	//	and CarbonLib 1.1. Otherwise, do it the hard way.
	
	bool result = false;
	
	if (gHasHFSPlusAPIs)
	{
		//	Parse to a ref
		FSRef ref;
		result = XMLParsePathToFSRef(pathName, ref, manager);
		
		//	Down convert to a spec
		if (result)
			result = (noErr == FSGetCatalogInfo(&ref,
								kFSCatInfoNone,
								static_cast<FSCatalogInfo*>(NULL),	// catalogInfo
								static_cast<HFSUniStr255*>(NULL),	// outName
								&spec,
								static_cast<FSRef*>(NULL)			// parentRef
								));
	}
	else
		result = XMLParsePathToFSSpec_Classic(pathName, spec, manager);
		
	//	Return true on success
	return result;
}


bool
XMLParsePathToFSSpec_Classic(const XMLCh* const pathName, FSSpec& spec,
                            MemoryManager* const manager)
{
	//	Manually parse the path using FSSpec APIs.
	
    //	Transcode the path into ascii
    const char* p = XMLString::transcode(pathName, manager);
    ArrayJanitor<const char> janPath(p, manager);
    const char* pEnd;
    std::size_t segLen;

    OSStatus err = noErr;
    Str255 name;  // Must be long enough for a partial pathname consisting of two segments (64 bytes)

    if (*p == '/')
    {
        // Absolute name: grab the first component as volume name

        // Find the end of the path segment
        for (pEnd = ++p; *pEnd && *pEnd != '/'; ++pEnd) ;
        segLen = pEnd - p;

        // Try to find a volume that matches this name
        for (ItemCount volIndex = 1; err == noErr; ++volIndex)
        {
            FSVolumeRefNum volRefNum;

            if (gHasFS2TBAPIs)
            {
                XVolumeParam xVolParam;
                name[0] = 0;
                xVolParam.ioNamePtr  = name;
                xVolParam.ioVRefNum  = 0;
                xVolParam.ioXVersion = 0;
                xVolParam.ioVolIndex = volIndex;
                err = PBXGetVolInfoSync(&xVolParam);
                volRefNum = xVolParam.ioVRefNum;
            }
            else
            {
#if !TARGET_API_MAC_CARBON
                HParamBlockRec hfsParams;
                name[0] = 0;
                hfsParams.volumeParam.ioNamePtr  = name;
                hfsParams.volumeParam.ioVRefNum  = 0;
                hfsParams.volumeParam.ioVolIndex = volIndex;
                err = PBHGetVInfoSync(&hfsParams);
                volRefNum = hfsParams.volumeParam.ioVRefNum;
#else
                err = nsvErr;
#endif
            }

            // Compare against our path segment
            if (err == noErr && segLen == StrLength(name))
            {
            	//	Case-insensitive compare
            	if (XMLString::compareNIString(
	            		ConvertSlashToColon(reinterpret_cast<char*>(&name[1]), segLen),
	            		p, segLen) == 0)
                {
                    // we found our volume: fill in the spec
                    err = FSMakeFSSpec(volRefNum, fsRtDirID, NULL, &spec);
                    break;
                }
            }
        }

        p = pEnd;
    }
    else
    {
        // Relative name, so get the default directory as parent spec
        err = FSMakeFSSpec(0, 0, NULL, &spec);
    }

    // We now have a parent directory in the spec.
    while (err == noErr && *p)
    {
        switch (*p)
        {
        case '/': 	// Just skip any number of path separators
            ++p;
            break;

        case L'.': 	// Potentially "current directory" or "parent directory"
            if (p[1] == '/' || p[1] == 0)      // "current directory"
            {
                ++p;
                break;
            }
            else if (p[1] == '.' && (p[2] == '/' || p[2] == 0)) // "parent directory"
            {
                p += 2;  // Get the parent of our parent

                CInfoPBRec catInfo;
                catInfo.dirInfo.ioNamePtr = NULL;
                catInfo.dirInfo.ioVRefNum = spec.vRefNum;
                catInfo.dirInfo.ioFDirIndex = -1;
                catInfo.dirInfo.ioDrDirID = spec.parID;
                err = PBGetCatInfoSync(&catInfo);

                // Check that we didn't go too far
                if (err != noErr || catInfo.dirInfo.ioDrParID == fsRtParID)
                    return false;

                // Update our spec
                if (err == noErr)
                    err = FSMakeFSSpec(spec.vRefNum, catInfo.dirInfo.ioDrDirID, NULL, &spec);

                break;
            }
            else // some other sequence of periods...fall through and treat as segment
                ;

        default:
            {
                // Find the end of the path segment
                for (pEnd = p; *pEnd && *pEnd != '/'; ++pEnd) ;
                segLen = pEnd - p;

                // Check for name length overflow
                if (segLen > 31)
                    return false;

                // Make a partial pathname from our current spec to the new object
                unsigned char* partial = &name[1];

                *partial++ = ':';       // Partial leads with :
                const unsigned char* specName = spec.name; // Copy in spec name
                for (int specCnt = *specName++; specCnt > 0; --specCnt)
                    *partial++ = *specName++;

                *partial++ = ':';       // Separator
                while (p != pEnd)       // Copy in new element
               	{
                	if (*p == ':')				// Convert : to /
                	{
                		*partial++ = '/';
                		p++;
                	}
                	else
                		*partial++ = *p++;
				}
				
                name[0] = partial - &name[1];   // Set the name length

                // Update the spec
                err = FSMakeFSSpec(spec.vRefNum, spec.parID, name, &spec);
            }
            break;
        }
    }

    return err == noErr;
}


XMLCh*
XMLCreateFullPathFromFSRef(const FSRef& startingRef,
                            MemoryManager* const manager)
{
	XMLCh* result = NULL;
	
	//	If FSRefMakePath is available, we use it to create the
	//	path: this gives us "standard" path support under MacOS X.
	//	Without this, our paths in that environment would always
	//	have a volume name at their root...which would look
	//	"normal" to Mac users, but not normal to unix users. Since
	//	we're making "unix" paths, we'll stick with the unix
	//	style paths. This also allows us to easilly take paths
	//	off the command line.
	//
	//	FSRefMakePath is available on Mac OS X and in CarbonLib 1.1
	//	and greater. But we use it only on X since on Classic it
	//	makes paths with ':' separators, which really confuses us!
	
	if (TARGET_API_MAC_CARBON && gHasFSPathAPIs && gPathAPIsUsePosixPaths)
		result = XMLCreateFullPathFromFSRef_X(startingRef, manager);
	else
		result = XMLCreateFullPathFromFSRef_Classic(startingRef, manager);
		
	return result;
}


XMLCh*
XMLCreateFullPathFromFSRef_X(const FSRef& startingRef,
                            MemoryManager* const manager)
{
	//	Create the path using FSRefMakePath as available on Mac OS X
	//	and under CarbonLib 1.1 and greater.
	OSStatus err = noErr;
	
	//	Make the path in utf8 form
	char utf8Buf[kMaxMacStaticPathChars];
	utf8Buf[0] = '\0';
	
	if (err == noErr)
		err = FSRefMakePath(&startingRef, reinterpret_cast<UInt8*>(utf8Buf), kMaxMacStaticPathChars);
		
	//	Bail if path conversion failed
	if (err != noErr)
		return NULL;
	
	//	Transcode into UniChars
	UniChar uniBuf[kMaxMacStaticPathChars];
	std::size_t pathLen = TranscodeUTF8ToUniChars(utf8Buf, uniBuf, kMaxMacStaticPathChars-1);
	uniBuf[pathLen++] = 0;
	
	//	Transcode into a dynamically allocated buffer of XMLChs
	ArrayJanitor<XMLCh> result((XMLCh*) manager->allocate(pathLen * sizeof(XMLCh))/*new XMLCh[pathLen]*/,
			manager);
	if (result.get() != NULL)
		CopyUniCharsToXMLChs(uniBuf, result.get(), pathLen, pathLen);
		
	return result.release();
}


XMLCh*
XMLCreateFullPathFromFSRef_Classic(const FSRef& startingRef,
                            MemoryManager* const manager)
{
	//	Manually create the path using FSRef APIs.
    OSStatus err = noErr;
    FSCatalogInfo catalogInfo;
    HFSUniStr255 name;
    FSRef ref = startingRef;

    XMLCh buf[kMaxMacStaticPathChars];
    std::size_t bufPos   = kMaxMacStaticPathChars;
    std::size_t bufCnt   = 0;

    ArrayJanitor<XMLCh> result(NULL);
    std::size_t resultLen = 0;

    buf[--bufPos] = L'\0';
    ++bufCnt;

	do
	{
		err = FSGetCatalogInfo(
			&ref,
			kFSCatInfoParentDirID,
			&catalogInfo,
			&name,
			static_cast<FSSpec*>(NULL),
			&ref
			);
		
		if (err == noErr)
		{
			// If there's not room in our static buffer for the new
			// name plus separator, dump it to the dynamic result buffer.
			if (bufPos < (std::size_t)name.length + 1)
			{
				ArrayJanitor<XMLCh> temp
                (
                    (XMLCh*) manager->allocate((bufCnt + resultLen) * sizeof(XMLCh)) //new XMLCh[bufCnt + resultLen]
                    , manager
                );
				
				// Copy in the static buffer
				std::memcpy(temp.get(), &buf[bufPos], bufCnt * sizeof(XMLCh));
				
				// Copy in the old buffer
				if (resultLen > 0)
					std::memcpy(temp.get() + bufCnt, result.get(), resultLen * sizeof(XMLCh));
				
				result.reset(temp.release());
				resultLen += bufCnt;
				
				bufPos = kMaxMacStaticPathChars;
				bufCnt = 0;
			}
			
			// Prepend our new name and a '/'
			bufPos -= name.length;
			ConvertSlashToColon(
				CopyUniCharsToXMLChs(name.unicode, &buf[bufPos], name.length, name.length),
				name.length);
			buf[--bufPos] = L'/';
			bufCnt += (name.length + 1);
		}
	}
	while (err == noErr && catalogInfo.parentDirID != fsRtParID);
	
	// Composite existing buffer + any previous result buffer
	ArrayJanitor<XMLCh> final
    (
        (XMLCh*) manager->allocate((bufCnt + resultLen) * sizeof(XMLCh))//new XMLCh[bufCnt + resultLen]
        , manager
    );
	
	// Copy in the static buffer
	std::memcpy(final.get(), &buf[bufPos], bufCnt * sizeof(XMLCh));
	
	// Copy in the old buffer
	if (resultLen > 0)
		std::memcpy(final.get() + bufCnt, result.get(), resultLen * sizeof(XMLCh));
	
    return final.release();
}


XMLCh*
XMLCreateFullPathFromFSSpec(const FSSpec& startingSpec,
                            MemoryManager* const manager)
{
	XMLCh* result = NULL;
	
	//	If FSRefs are available, do this operation in terms of refs...this
	//	allows us to grandfather in the use of FSPathMakeRef and FSRefMakePath
	//	if possible.
	if (gHasHFSPlusAPIs)
	{
		OSStatus err = noErr;
		FSRef ref;
		
		//	Up convert to FSRef
		if (err == noErr)
			err = FSpMakeFSRef(&startingSpec, &ref);
			
		//	Create the path
		if (err == noErr)
			result = XMLCreateFullPathFromFSRef(ref, manager);
	}
	else
	{
		//	Create using FSSpecs only
		result = XMLCreateFullPathFromFSSpec_Classic(startingSpec, manager);
	}
		
	return result;
}


XMLCh*
XMLCreateFullPathFromFSSpec_Classic(const FSSpec& startingSpec,
                                    MemoryManager* const manager)
{
	//	Manually create the path using FSSpec APIs.
    OSStatus err = noErr;
    FSSpec spec = startingSpec;

    char buf[kMaxMacStaticPathChars];
    std::size_t bufPos   = kMaxMacStaticPathChars;
    std::size_t bufCnt   = 0;

    ArrayJanitor<char> result(NULL);
    std::size_t resultLen = 0;

    buf[--bufPos] = '\0';
    ++bufCnt;

	short index = 0;
	do
	{
		CInfoPBRec catInfo;
		catInfo.dirInfo.ioNamePtr = spec.name;
		catInfo.dirInfo.ioVRefNum = spec.vRefNum;
		catInfo.dirInfo.ioFDirIndex = index;
		catInfo.dirInfo.ioDrDirID = spec.parID;
		err = PBGetCatInfoSync(&catInfo);
		
		if (err == noErr)
		{
			std::size_t nameLen = StrLength(spec.name);
			
			// If there's not room in our static buffer for the new
			// name plus separator, dump it to the dynamic result buffer.
			if (bufPos < nameLen + 1)
			{
				ArrayJanitor<char> temp
                (
                    (char*) manager->allocate((bufCnt + resultLen) * sizeof(char))//new char[bufCnt + resultLen]
                    , manager
                );
				
				// Copy in the static buffer
				std::memcpy(temp.get(), &buf[bufPos], bufCnt);
				
				// Copy in the old buffer
				if (resultLen > 0)
					std::memcpy(temp.get() + bufCnt, result.get(), resultLen);
				
				result.reset(temp.release());
				resultLen += bufCnt;
				
				bufPos = kMaxMacStaticPathChars;
				bufCnt = 0;
			}
			
			// Prepend our new name and a '/'
			bufPos -= nameLen;
			ConvertSlashToColon((char*)std::memcpy(&buf[bufPos], &spec.name[1], nameLen), nameLen);
			buf[--bufPos] = '/';
			bufCnt += (nameLen + 1);
			
			// From here on out, ignore the input file name
			index = -1;
			
			// Move up to the parent
			spec.parID = catInfo.dirInfo.ioDrParID;
		}
	}
	while (err == noErr && spec.parID != fsRtParID);
	
	// Composite existing buffer with any previous result buffer
	ArrayJanitor<char> final
    (
        (char*) manager->allocate((bufCnt + resultLen) * sizeof(char))//new char[bufCnt + resultLen]
        , manager
    );
	
	// Copy in the static buffer
	std::memcpy(final.get(), &buf[bufPos], bufCnt);
	
	// Copy in the old buffer
	if (resultLen > 0)
		std::memcpy(final.get() + bufCnt, result.get(), resultLen);

    // Cleanup and transcode to unicode
    return XMLString::transcode(final.get(), manager);
}


std::size_t
TranscodeUniCharsToUTF8(const UniChar* src, char* dst, std::size_t srcCnt, std::size_t maxChars)
{
	std::size_t result = 0;
	
    //	Use the text encoding converter to perform the format conversion.
    //	Note that this is slightly heavyweight, but we're not in a performance
    //	sensitive code-path.

    OSStatus err = noErr;
    TECObjectRef tec = 0;
    ByteCount bytesConsumed = 0;
    ByteCount bytesProduced = 0;

    TextEncoding inputEncoding	= CreateTextEncoding(kTextEncodingUnicodeDefault,
                                        kTextEncodingDefaultVariant,
                                        kUnicode16BitFormat);

    TextEncoding outputEncoding = CreateTextEncoding(kTextEncodingUnicodeDefault,
                                        kTextEncodingDefaultVariant,
                                        kUnicodeUTF8Format);

    if (err == noErr)
        err = TECCreateConverter(&tec, inputEncoding, outputEncoding);

    if (err == noErr)
        err = TECConvertText(tec,
                    (ConstTextPtr) src,
                    srcCnt * sizeof(UniChar),	// inputBufferLength
                    &bytesConsumed,				// actualInputLength
                    (TextPtr) dst,				// outputBuffer
                    maxChars * sizeof(char),	// outputBufferLength
                    &bytesProduced);			// actualOutputLength

    TECDisposeConverter(tec);

    result = bytesProduced;
	
    //	Return number of chars in dst
	return result;
}


std::size_t
TranscodeUTF8ToUniChars(const char* src, UniChar* dst, std::size_t maxChars)
{
	std::size_t result = 0;
	
    //	Use the text encoding converter to perform the format conversion.
    //	Note that this is slightly heavyweight, but we're not in a performance
    //	sensitive code-path.

    OSStatus err = noErr;
    TECObjectRef tec = 0;
    ByteCount bytesConsumed = 0;
    ByteCount bytesProduced = 0;

    TextEncoding inputEncoding	= CreateTextEncoding(kTextEncodingUnicodeDefault,
                                        kTextEncodingDefaultVariant,
                                        kUnicodeUTF8Format);

    TextEncoding outputEncoding = CreateTextEncoding(kTextEncodingUnicodeDefault,
                                        kTextEncodingDefaultVariant,
                                        kUnicode16BitFormat);

    if (err == noErr)
        err = TECCreateConverter(&tec, inputEncoding, outputEncoding);

    if (err == noErr)
        err = TECConvertText(tec,
                    (ConstTextPtr) src,
                    std::strlen(src),			// inputBufferLength
                    &bytesConsumed,				// actualInputLength
                    (TextPtr) dst,				// outputBuffer
                    maxChars * sizeof(UniChar),	// outputBufferLength
                    &bytesProduced);			// actualOutputLength

    TECDisposeConverter(tec);

    result = bytesProduced / sizeof(UniChar);
	
    //	Return number of unicode characters in dst
	return result;
}

#include <xercesc/util/LogicalPath.c>

XERCES_CPP_NAMESPACE_END
