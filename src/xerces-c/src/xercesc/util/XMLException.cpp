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
 * $Id: XMLException.cpp 568078 2007-08-21 11:43:25Z amassari $
 */


// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/util/XMLException.hpp>
#include <xercesc/util/Mutexes.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/XMLMsgLoader.hpp>
#include <xercesc/util/XMLRegisterCleanup.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/XMLUniDefs.hpp>
#include <xercesc/util/XMLInitializer.hpp>


XERCES_CPP_NAMESPACE_BEGIN

// ---------------------------------------------------------------------------
//  Local, static data
// ---------------------------------------------------------------------------
static XMLMsgLoader* sMsgLoader = 0;
static XMLRegisterCleanup msgLoaderCleanup;
static bool sScannerMutexRegistered = false;
static XMLMutex* sMsgMutex = 0;
static XMLRegisterCleanup msgMutexCleanup;

// ---------------------------------------------------------------------------
//  Local, static functions
// ---------------------------------------------------------------------------

//
//  We need to fault in this mutex. But, since its used for synchronization
//  itself, we have to do this the low level way using a compare and swap.
//
static XMLMutex& gMsgMutex()
{
    if (!sScannerMutexRegistered)
    {
        XMLMutexLock lockInit(XMLPlatformUtils::fgAtomicMutex);

        if (!sScannerMutexRegistered)
        {
            sMsgMutex = new XMLMutex(XMLPlatformUtils::fgMemoryManager);
            msgMutexCleanup.registerCleanup(XMLException::reinitMsgMutex);
            sScannerMutexRegistered = true;
        }
    }

    return *sMsgMutex;
}

//
//  This method is a lazy evaluator for the message loader for exception
//  messages.
//
static XMLMsgLoader& gGetMsgLoader()
{
    if (!sMsgLoader)
    {
        // Lock the message loader mutex and load the text
	    XMLMutexLock lockInit(&gMsgMutex());

        // Fault it in on first request
        if (!sMsgLoader)
        {
            sMsgLoader = XMLPlatformUtils::loadMsgSet(XMLUni::fgExceptDomain);
            if (!sMsgLoader)
                XMLPlatformUtils::panic(PanicHandler::Panic_CantLoadMsgDomain);

            //
            // Register this XMLMsgLoader for cleanup at Termination.
            //
            msgLoaderCleanup.registerCleanup(XMLException::reinitMsgLoader);
        }
    }

    // We got it, so return it
    return *sMsgLoader;
}

void XMLInitializer::initializeExceptionMsgLoader()
{
    sMsgLoader = XMLPlatformUtils::loadMsgSet(XMLUni::fgExceptDomain);
    if (sMsgLoader) {
        msgLoaderCleanup.registerCleanup(XMLException::reinitMsgLoader);
    }
}

// ---------------------------------------------------------------------------
//  XMLException: Virtual destructor
// ---------------------------------------------------------------------------
XMLException::~XMLException()
{
    fMemoryManager->deallocate(fMsg);
    fMemoryManager->deallocate(fSrcFile);
}


// ---------------------------------------------------------------------------
//  XMLException: Setter methods
// ---------------------------------------------------------------------------
void XMLException::setPosition(const char* const file, const unsigned int line)
{
    fSrcLine = line;
	fMemoryManager->deallocate(fSrcFile);
    fSrcFile = XMLString::replicate(file, fMemoryManager);
}


// ---------------------------------------------------------------------------
//  XMLException: Hidden constructors and operators
// ---------------------------------------------------------------------------
XMLException::XMLException() :

    fCode(XMLExcepts::NoError)
    , fSrcFile(0)
    , fSrcLine(0)
    , fMsg(0)
    , fMemoryManager(XMLPlatformUtils::fgMemoryManager)
{
}


XMLException::XMLException( const   char* const     srcFile
                            , const unsigned int    srcLine
                            , MemoryManager* const  memoryManager) :

    fCode(XMLExcepts::NoError)
    , fSrcFile(0)
    , fSrcLine(srcLine)
    , fMsg(0)
    , fMemoryManager(memoryManager)
{
    if (!memoryManager)
        fMemoryManager = XMLPlatformUtils::fgMemoryManager;
    fSrcFile = XMLString::replicate(srcFile, fMemoryManager);
}


XMLException::XMLException(const XMLException& toCopy) :
    XMemory(toCopy)
    , fCode(toCopy.fCode)
    , fSrcFile(0)
    , fSrcLine(toCopy.fSrcLine)
    , fMsg(XMLString::replicate(toCopy.fMsg, toCopy.fMemoryManager))
    , fMemoryManager(toCopy.fMemoryManager)        
{
    if (toCopy.fSrcFile) {
        fSrcFile = XMLString::replicate
        (
            toCopy.fSrcFile
            , fMemoryManager
        );
    }
}


XMLException& XMLException::operator=(const XMLException& toAssign)
{
    if (this != &toAssign)
    {
        //use the original memory manager to deallocate
        fMemoryManager->deallocate(fSrcFile);
        fSrcFile = 0;
        fMemoryManager->deallocate(fMsg);
        fMsg = 0;

        fMemoryManager = toAssign.fMemoryManager;
        fSrcLine = toAssign.fSrcLine;
        fCode = toAssign.fCode;

        if (toAssign.fMsg) {
            fMsg = XMLString::replicate
            (
                toAssign.fMsg
                , fMemoryManager
            );
        }

        if (toAssign.fSrcFile) {
            fSrcFile = XMLString::replicate
            (
                toAssign.fSrcFile
                , fMemoryManager
            );
        }
    }
    return *this;
}



// ---------------------------------------------------------------------------
//  XMLException: Protected methods
// ---------------------------------------------------------------------------
void XMLException::loadExceptText(const XMLExcepts::Codes toLoad)
{
    // Store the error code
    fCode = toLoad;

    // Load up the text into a local buffer
    const unsigned int msgSize = 2047;
    XMLCh errText[msgSize + 1];

    // load the text
	if (!gGetMsgLoader().loadMsg(toLoad, errText, msgSize))
	{
		fMsg = XMLString::replicate
        (
        XMLUni::fgDefErrMsg
            , fMemoryManager
        );
		return;
	}

    // We got the text so replicate it into the message member
    fMsg = XMLString::replicate(errText, fMemoryManager);
}


void
XMLException::loadExceptText(const  XMLExcepts::Codes toLoad
                            , const XMLCh* const        text1
                            , const XMLCh* const        text2
                            , const XMLCh* const        text3
                            , const XMLCh* const        text4)
{
    // Store the error code
    fCode = toLoad;

    // Load up the text into a local buffer
    const unsigned int msgSize = 4095;
    XMLCh errText[msgSize + 1];

    // load the text
	if (!gGetMsgLoader().loadMsg(toLoad, errText, msgSize, text1, text2, text3, text4, fMemoryManager))
	{
		fMsg = XMLString::replicate
        (
        XMLUni::fgDefErrMsg
            , fMemoryManager
        );
		return;
	}

    // We got the text so replicate it into the message member
    fMsg = XMLString::replicate(errText, fMemoryManager);
}


void
XMLException::loadExceptText(const  XMLExcepts::Codes toLoad
                            , const char* const         text1
                            , const char* const         text2
                            , const char* const         text3
                            , const char* const         text4)
{
    // Store the error code
    fCode = toLoad;

    // Load up the text into a local buffer
    const unsigned int msgSize = 4095;
    XMLCh errText[msgSize + 1];

    // load the text
	if (!gGetMsgLoader().loadMsg(toLoad, errText, msgSize, text1, text2, text3, text4, fMemoryManager))
	{
		fMsg = XMLString::replicate
        (
        XMLUni::fgDefErrMsg
            , fMemoryManager
        );
		return;
	}

    // We got the text so replicate it into the message member
    fMsg = XMLString::replicate(errText, fMemoryManager);
}

// -----------------------------------------------------------------------
//  Reinitialise the message mutex
// -----------------------------------------------------------------------
void XMLException::reinitMsgMutex()
{
    delete sMsgMutex;
    sMsgMutex = 0;
    sScannerMutexRegistered = false;
}

// -----------------------------------------------------------------------
//  Reinitialise the message loader
// -----------------------------------------------------------------------
void XMLException::reinitMsgLoader()
{
    delete sMsgLoader;
    sMsgLoader = 0;
}

XERCES_CPP_NAMESPACE_END
