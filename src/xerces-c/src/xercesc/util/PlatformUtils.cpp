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
 * $Id: PlatformUtils.cpp 568078 2007-08-21 11:43:25Z amassari $
 */


// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/util/XMLMsgLoader.hpp>
#include <xercesc/util/Mutexes.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/RefVectorOf.hpp>
#include <xercesc/util/TransService.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/XMLNetAccessor.hpp>
#include <xercesc/util/XMLUni.hpp>
#include <xercesc/internal/XMLReader.hpp>
#include <xercesc/util/RuntimeException.hpp>
#include <xercesc/util/XMLRegisterCleanup.hpp>
#include <xercesc/util/DefaultPanicHandler.hpp>
#include <xercesc/util/XMLInitializer.hpp>
#include <xercesc/internal/MemoryManagerImpl.hpp>
#include <xercesc/internal/MemoryManagerArrayImpl.hpp>

#include <limits.h>

XERCES_CPP_NAMESPACE_BEGIN

// ---------------------------------------------------------------------------
//  Local data members
//
//  gSyncMutex
//      This is a mutex that will be used to synchronize access to some of
//      the static data of the platform utilities class and here locally.
// ---------------------------------------------------------------------------
static XMLMutex*                gSyncMutex = 0;
static long                     gInitFlag = 0;

// ---------------------------------------------------------------------------
//  Global data
//
//	gXMLCleanupList
//		This is a list of cleanup functions to be called on
//		XMLPlatformUtils::Terminate.  Their function is to reset static
//		data in classes that use it.
//
//	gXMLCleanupListMutex
//		This is a mutex that will be used to synchronise access to the global
//		static data cleanup list
// ---------------------------------------------------------------------------
XMLRegisterCleanup*	gXMLCleanupList = 0;
XMLMutex*           gXMLCleanupListMutex = 0;


// ---------------------------------------------------------------------------
//  XMLPlatformUtils: Static Data Members
// ---------------------------------------------------------------------------
XMLNetAccessor*         XMLPlatformUtils::fgNetAccessor = 0;
XMLTransService*        XMLPlatformUtils::fgTransService = 0;
#ifdef OS390
XMLTransService*        XMLPlatformUtils::fgTransService2 = 0;
#endif
PanicHandler*           XMLPlatformUtils::fgUserPanicHandler = 0;
PanicHandler*           XMLPlatformUtils::fgDefaultPanicHandler = 0;
MemoryManager*          XMLPlatformUtils::fgMemoryManager = 0;
MemoryManagerArrayImpl  gArrayMemoryManager;
MemoryManager*          XMLPlatformUtils::fgArrayMemoryManager = &gArrayMemoryManager;
bool                    XMLPlatformUtils::fgMemMgrAdopted = true;
XMLMutex*               XMLPlatformUtils::fgAtomicMutex = 0;

// ---------------------------------------------------------------------------
//  XMLPlatformUtils: Init/term methods
// ---------------------------------------------------------------------------
void XMLPlatformUtils::Initialize(const char*          const locale 
                                , const char*          const nlsHome
                                ,       PanicHandler*  const panicHandler
                                ,       MemoryManager* const memoryManager
                                ,       bool                 toInitStatics)
{
    //
    //  Effects of overflow:
    //  . resouce re-allocations
    //  . consequently resource leaks
    //  . potentially terminate() may never get executed
    //
    //  We got to prevent overflow from happening.
    //  no error or exception
    //
    if (gInitFlag == LONG_MAX)
        return;
	
    //
    //  Make sure we haven't already been initialized. Note that this is not
    //  thread safe and is not intended for that. Its more for those COM
    //  like processes that cannot keep up with whether they have initialized
    //  us yet or not.
    //
    gInitFlag++;

    if (gInitFlag > 1)
      return;

    // Set pluggable memory manager
    if (!fgMemoryManager)
    {
        if (memoryManager)
        {
            fgMemoryManager = memoryManager;
            fgMemMgrAdopted = false;
        }
        else
        {
            fgMemoryManager = new MemoryManagerImpl();
        }
    }

    /***
     * Panic Handler:
     *
     ***/
    if (!panicHandler)
    {
        fgDefaultPanicHandler = new DefaultPanicHandler();
    }
    else
    {
        fgUserPanicHandler = panicHandler;
    }
    
    //
    //  Call the platform init method, which is implemented in each of the
    //  per-platform implementation cpp files. This one does the very low
    //  level per-platform setup. It cannot use any XML util services at all,
    //  i.e. only native services.
    //
    platformInit();

    // Create the local sync mutex
    gSyncMutex = new XMLMutex(fgMemoryManager);

	// Create the mutex for the static data cleanup list
    gXMLCleanupListMutex = new XMLMutex(fgMemoryManager);
    fgAtomicMutex = new XMLMutex(fgMemoryManager);

    //
    //  Ask the per-platform code to make the desired transcoding service for
    //  us to use. This call cannot throw any exceptions or do anything that
    //  cause any transcoding to happen. It should create the service and
    //  return it or zero if it cannot.
    //
    //  This one also cannot use any utility services. It can only create a
    //  transcoding service object and return it.
    //
    //  If we cannot make one, then we call panic to end the process.
    //
    fgTransService = makeTransService();

    if (!fgTransService)
        panic(PanicHandler::Panic_NoTransService);

    // Initialize the transcoder service
    fgTransService->initTransService();

    //
    //  Try to create a default local code page transcoder. This is the one
    //  that will be used internally by the XMLString class. If we cannot
    //  create one, then call the panic method.
    //
    XMLLCPTranscoder* defXCode = XMLPlatformUtils::fgTransService->makeNewLCPTranscoder();
    if (!defXCode)
        panic(PanicHandler::Panic_NoDefTranscoder);
    XMLString::initString(defXCode, fgMemoryManager);

    //
    //  Now lets ask the per-platform code to give us an instance of the type
    //  of network access implementation he wants to use. This can return
    //  a zero pointer if this platform doesn't want to support this.
    //
    fgNetAccessor = makeNetAccessor();

    /***
     * Message Loader:
     *
     *     Locale setting 
     *     nlsHome setting 
     ***/
    XMLMsgLoader::setLocale(locale);
    XMLMsgLoader::setNLSHome(nlsHome);

    if (toInitStatics) {
        XMLInitializer::InitializeAllStaticData();
    }
}


void XMLPlatformUtils::Terminate()
{
    //
    // To prevent it from running underflow.
    // otherwise we come to delete non-existing resources.
    //
    //  no error or exception
    //
    if (gInitFlag == 0)
        return;

	gInitFlag--;
	
	if (gInitFlag > 0)
		return;

    // Delete any net accessor that got installed
    delete fgNetAccessor;
    fgNetAccessor = 0;

    //
    //  Call some other internal modules to give them a chance to clean up.
    //  Do the string class last in case something tries to use it during
    //  cleanup.
    //
    XMLString::termString();

    // Clean up the the transcoding service
    delete fgTransService;
    fgTransService = 0;

    // Clean up the sync mutex
    delete gSyncMutex;
    gSyncMutex = 0;

    // Clean up mutex
    delete fgAtomicMutex;
    fgAtomicMutex = 0;

	// Clean up statically allocated, lazily cleaned data in each class
	// that has registered for it.
	// Note that calling doCleanup() also unregisters the cleanup
	// function, so that we are chewing the list down to nothing here
	while (gXMLCleanupList)
		gXMLCleanupList->doCleanup();

	// Clean up the mutex for accessing gXMLCleanupList
	delete gXMLCleanupListMutex;
	gXMLCleanupListMutex = 0;

    //
    //  And do platform termination. This cannot do use any XML services
    //  at all, it can only clean up local stuff. It it reports an error,
    //  it cannot use any XML exception or error reporting services.
    //
    platformTerm();

    /***
     *  de-allocate resource
     *
     *  refer to discussion in the Initialize()
     ***/
    XMLMsgLoader::setLocale(0);
    XMLMsgLoader::setNLSHome(0);

    if (fgDefaultPanicHandler)
    {
        delete fgDefaultPanicHandler;
    }
    fgDefaultPanicHandler = 0;
    fgUserPanicHandler = 0;

    // de-allocate default memory manager
    if (fgMemMgrAdopted)
        delete fgMemoryManager;
    else
        fgMemMgrAdopted = true;

    // set memory manager to 0
    fgMemoryManager = 0;

    // And say we are no longer initialized
    gInitFlag = 0;
}


// ---------------------------------------------------------------------------
//  XMLPlatformUtils: Msg support methods
// ---------------------------------------------------------------------------
XMLMsgLoader* XMLPlatformUtils::loadMsgSet(const XMLCh* const msgDomain)
{
    //
    //  Ask the platform support to load up the correct type of message
    //  loader for the indicated message set. We don't check here whether it
    //  works or not. That's their decision.
    //
    return loadAMsgSet(msgDomain);
}

// ---------------------------------------------------------------------------
//  XMLPlatformUtils: NEL Character Handling
// ---------------------------------------------------------------------------
void XMLPlatformUtils::recognizeNEL(bool state, MemoryManager* const manager) {

    //Make sure initialize has been called
    if (gInitFlag == 0) {
        return;
    }

    if (state) {

        if (!XMLChar1_0::isNELRecognized()) {
            XMLChar1_0::enableNELWS();
        }
    }
    else {

        if (XMLChar1_0::isNELRecognized()) {
            ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::NEL_RepeatedCalls, manager);
        }
    }
}


bool XMLPlatformUtils::isNELRecognized() {

    return XMLChar1_0::isNELRecognized();
}

// ---------------------------------------------------------------------------
//  XMLPlatformUtils: IANA Encoding checking setting
// ---------------------------------------------------------------------------
void XMLPlatformUtils::strictIANAEncoding(const bool state) {

    //Make sure initialize has been called
    if (gInitFlag == 0) {
        return;
    }

    fgTransService->strictIANAEncoding(state);
}


bool XMLPlatformUtils::isStrictIANAEncoding() {

    if (gInitFlag)
        return fgTransService->isStrictIANAEncoding();

    return false;
}

XERCES_CPP_NAMESPACE_END
