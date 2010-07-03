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
 * $Id: IRIXPlatformUtils.cpp 568078 2007-08-21 11:43:25Z amassari $
 */


// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------

#if !defined(APP_NO_THREADS)
#ifdef XML_USE_SPROC
#include    <ulocks.h>
#else
#include    <pthread.h>
#endif
#endif

#include    <unistd.h>
#include    <stdio.h>
#include    <stdlib.h>
#include    <errno.h>
#include    <sys/param.h>
#include    <sys/timeb.h>
#include    <string.h>
#include    <xercesc/util/PlatformUtils.hpp>
#include    <xercesc/util/RuntimeException.hpp>
#include    <xercesc/util/Janitor.hpp>
#include    <xercesc/util/Mutexes.hpp>
#include    <xercesc/util/XMLHolder.hpp>
#include    <xercesc/util/XMLString.hpp>
#include    <xercesc/util/XMLUniDefs.hpp>
#include    <xercesc/util/XMLUni.hpp>
#include    <xercesc/util/PanicHandler.hpp>
#include    <xercesc/util/OutOfMemoryException.hpp>

#if defined(XML_USE_ICU_TRANSCODER)
    #include <xercesc/util/Transcoders/ICU/ICUTransService.hpp>
#else
    // Use native transcoder. Same as -DXML_USE_NATIVE_TRANSCODER
    #include <xercesc/util/Transcoders/Iconv/IconvTransService.hpp>
#endif


#if defined(XML_USE_ICU_MESSAGELOADER)
    #include <xercesc/util/MsgLoaders/ICU/ICUMsgLoader.hpp>
#elif defined (XML_USE_ICONV_MESSAGELOADER)
    #include <xercesc/util/MsgLoaders/MsgCatalog/MsgCatalogLoader.hpp>
#else
    // Same as -DXML_USE_INMEM_MESSAGELOADER
    #include <xercesc/util/MsgLoaders/InMemory/InMemMsgLoader.hpp>
#endif


#if defined (XML_USE_NETACCESSOR_SOCKET)
    #include <xercesc/util/NetAccessors/Socket/SocketNetAccessor.hpp>
#endif

XERCES_CPP_NAMESPACE_BEGIN

// ---------------------------------------------------------------------------
//  XMLPlatformUtils: Private Static Methods
// ---------------------------------------------------------------------------

XMLNetAccessor* XMLPlatformUtils::makeNetAccessor()
{
#if defined (XML_USE_NETACCESSOR_SOCKET)
    return new (fgMemoryManager) SocketNetAccessor();
#else
    return 0;
#endif
}

//
//  This method is called by the platform independent part of this class
//  when client code asks to have one of the supported message sets loaded.
//

XMLMsgLoader* XMLPlatformUtils::loadAMsgSet(const XMLCh* const msgDomain)
{
    XMLMsgLoader* retVal;
    try
    {
#if defined (XML_USE_ICU_MESSAGELOADER)
        retVal = new (fgMemoryManager) ICUMsgLoader(msgDomain);
#elif defined (XML_USE_ICONV_MESSAGELOADER)
        retVal = new (fgMemoryManager) MsgCatalogLoader(msgDomain);
#else
        // same as -DXML_USE_INMEM_MESSAGELOADER
        retVal = new (fgMemoryManager) InMemMsgLoader(msgDomain);
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

XMLTransService* XMLPlatformUtils::makeTransService()
{
#if defined (XML_USE_ICU_TRANSCODER)
    // Use ICU transcoding services.
    // same as -DXML_USE_ICU_MESSAGELOADER
    return new (fgMemoryManager) ICUTransService;
#else
    // Use native transcoding services.
    // same as -DXML_USE_INMEM_MESSAGELOADER
    return new (fgMemoryManager) IconvTransService;

#endif
}

// ---------------------------------------------------------------------------
//  XMLPlatformUtils: The panic method
// ---------------------------------------------------------------------------
void XMLPlatformUtils::panic(const PanicHandler::PanicReasons reason)
{
    fgUserPanicHandler? fgUserPanicHandler->panic(reason) : fgDefaultPanicHandler->panic(reason);	
}


// ---------------------------------------------------------------------------
//  XMLPlatformUtils: File Methods
// ---------------------------------------------------------------------------

unsigned int XMLPlatformUtils::curFilePos(FileHandle theFile
                                          , MemoryManager* const manager)
{
    int curPos = ftell( (FILE*)theFile);
    if (curPos == -1)
        ThrowXMLwithMemMgr(XMLPlatformUtilsException,
                 XMLExcepts::File_CouldNotGetSize, manager);

    return (unsigned int)curPos;
}

void XMLPlatformUtils::closeFile(FileHandle theFile
                                 , MemoryManager* const manager)
{
    if (fclose((FILE*)theFile))
        ThrowXMLwithMemMgr(XMLPlatformUtilsException,
                 XMLExcepts::File_CouldNotCloseFile, manager);
}

unsigned int XMLPlatformUtils::fileSize(FileHandle theFile
                                        , MemoryManager* const manager)
{
    // Get the current position
    long  int curPos = ftell((FILE*) theFile);
    if (curPos == -1)
        ThrowXMLwithMemMgr(XMLPlatformUtilsException,
                 XMLExcepts::File_CouldNotGetCurPos, manager);

    // Seek to the end and save that value for return
     if (fseek((FILE*) theFile, 0, SEEK_END))
        ThrowXMLwithMemMgr(XMLPlatformUtilsException,
                 XMLExcepts::File_CouldNotSeekToEnd, manager);

    long int retVal = ftell((FILE*)theFile);
    if (retVal == -1)
        ThrowXMLwithMemMgr(XMLPlatformUtilsException,
                 XMLExcepts::File_CouldNotSeekToEnd, manager);

    // And put the pointer back

    if (fseek( (FILE*)theFile, curPos, SEEK_SET) )
        ThrowXMLwithMemMgr(XMLPlatformUtilsException,
                 XMLExcepts::File_CouldNotSeekToPos, manager);

    return (unsigned int)retVal;
}

FileHandle XMLPlatformUtils::openFile(const XMLCh* const fileName
                                      , MemoryManager* const manager)
{
    const char* tmpFileName = XMLString::transcode(fileName, manager);
    ArrayJanitor<char> janText((char*)tmpFileName, manager);
    FileHandle retVal = (FILE*)fopen( tmpFileName , "rb" );

    if (retVal == NULL)
        return 0;
    return retVal;
}

FileHandle XMLPlatformUtils::openFile(const char* const fileName
                                      , MemoryManager* const manager)
{
    FileHandle retVal = (FILE*)fopen( fileName , "rb" );

    if (retVal == NULL)
        return 0;

    return retVal;
}


FileHandle XMLPlatformUtils::openFileToWrite(const XMLCh* const fileName
                                             , MemoryManager* const manager)
{
    const char* tmpFileName = XMLString::transcode(fileName, manager);
    ArrayJanitor<char> janText((char*)tmpFileName, manager);
    return fopen( tmpFileName , "wb" );
}

FileHandle XMLPlatformUtils::openFileToWrite(const char* const fileName
                                             , MemoryManager* const manager)
{
    return fopen( fileName , "wb" );
}


FileHandle XMLPlatformUtils::openStdInHandle(MemoryManager* const manager)
{
        return (FileHandle)fdopen(dup(0), "rb");
}



unsigned int
XMLPlatformUtils::readFileBuffer( FileHandle           theFile
                                , const unsigned int   toRead
                                , XMLByte* const       toFill
                                , MemoryManager* const manager)
{
    size_t noOfItemsRead = fread((void*) toFill, 1, toRead, (FILE*)theFile);

    if(ferror((FILE*)theFile))
    {
        ThrowXMLwithMemMgr(XMLPlatformUtilsException,
                 XMLExcepts::File_CouldNotReadFromFile, manager);
    }


    return (unsigned int)noOfItemsRead;
}


void
XMLPlatformUtils::writeBufferToFile( FileHandle     const  theFile
                                   , long                  toWrite
                                   , const XMLByte* const  toFlush
                                   , MemoryManager* const  manager)
{
    if (!theFile        ||
        (toWrite <= 0 ) ||
        !toFlush         )
        return;

    const XMLByte* tmpFlush = (const XMLByte*) toFlush;
    size_t bytesWritten = 0;

    while (true)
    {
        bytesWritten=fwrite(tmpFlush, sizeof(XMLByte), toWrite, (FILE*)theFile);

        if(ferror((FILE*)theFile))
        {
            ThrowXMLwithMemMgr(XMLPlatformUtilsException,XMLExcepts::File_CouldNotWriteToFile, manager);
        }

        if (bytesWritten < toWrite) //incomplete write
        {
            tmpFlush+=bytesWritten;
            toWrite-=bytesWritten;
            bytesWritten=0;
        }
        else
            return;
    }

    return;
}


void XMLPlatformUtils::resetFile(FileHandle theFile
                                 , MemoryManager* const manager)
{
    // Seek to the start of the file
    if (fseek((FILE*)theFile, 0, SEEK_SET))
        ThrowXMLwithMemMgr(XMLPlatformUtilsException,
                 XMLExcepts::File_CouldNotResetFile, manager);
}


// ---------------------------------------------------------------------------
//  XMLPlatformUtils: File system methods
// ---------------------------------------------------------------------------
XMLCh* XMLPlatformUtils::getFullPath(const XMLCh* const srcPath,
                                     MemoryManager* const manager)
{
    //
    //  NOTE: The path provided has always already been opened successfully,
    //  so we know that its not some pathological freaky path. It comes in
    //  in native format, and goes out as Unicode always
    //
    char* newSrc = XMLString::transcode(srcPath, manager);
    ArrayJanitor<char> janText(newSrc, manager);

    // Use a local buffer that is big enough for the largest legal path
    char absPath[PATH_MAX + 1];
    // get the absolute path
    char* retPath = realpath(newSrc, &absPath[0]);

    if (!retPath)
    {
        ThrowXMLwithMemMgr(XMLPlatformUtilsException, XMLExcepts::File_CouldNotGetBasePathName, manager);
    }
    return XMLString::transcode(absPath, manager);
}

bool XMLPlatformUtils::isRelative(const XMLCh* const toCheck
                                  , MemoryManager* const manager)
{
    // Check for pathological case of empty path
    if (!toCheck[0])
        return false;

    //
    //  If it starts with a slash, then it cannot be relative. This covers
    //  both something like "\Test\File.xml" and an NT Lan type remote path
    //  that starts with a node like "\\MyNode\Test\File.xml".
    //
    if (toCheck[0] == XMLCh('/'))
        return false;

    // Else assume its a relative path
    return true;
}

XMLCh* XMLPlatformUtils::getCurrentDirectory(MemoryManager* const manager)
{
    char  dirBuf[PATH_MAX + 2];
    char  *curDir = getcwd(&dirBuf[0], PATH_MAX + 1);

    if (!curDir)
    {
        ThrowXMLwithMemMgr(XMLPlatformUtilsException,
                 XMLExcepts::File_CouldNotGetBasePathName, manager);
    }

    return XMLString::transcode(curDir, manager);
}

inline bool XMLPlatformUtils::isAnySlash(XMLCh c) 
{
    return ( chBackSlash == c || chForwardSlash == c);
}

// ---------------------------------------------------------------------------
//  XMLPlatformUtils: Timing Methods
// ---------------------------------------------------------------------------

unsigned long XMLPlatformUtils::getCurrentMillis()
{
    timeb aTime;
    ftime(&aTime);
    return (unsigned long)(aTime.time*1000 + aTime.millitm);

}

// -----------------------------------------------------------------------
//  Mutex methods
// -----------------------------------------------------------------------

#if !defined(APP_NO_THREADS)

static XMLMutex *atomicOpsMutex = 0;

#ifdef XML_USE_SPROC
// ---------------------------------------------------------------------------
//  XMLPlatformUtils: Platform init method
// ---------------------------------------------------------------------------

static char* arenaName = NULL;
static usptr_t* arena = NULL;

void XMLPlatformUtils::platformInit()
{
    //
    // The atomicOps mutex needs to be created early.
    // Normally, mutexes are created on first use, but there is a
    // circular dependency between compareAndExchange() and
    // mutex creation that must be broken.

    // need a shared memory space
    usconfig(CONF_INITUSERS, 128);
    usconfig(CONF_INITSIZE, 16000);
    usconfig(CONF_AUTOGROW, 1);   // Default, but we set anyway

    arenaName = strdup ("/var/tmp/xerces-sharedmemXXXXXX");
    arena = usinit (mktemp (arenaName));

    if(!atomicOpsMutex)
    {
        atomicOpsMutex = new (fgMemoryManager) XMLMutex(fgMemoryManager);
        if (atomicOpsMutex->fHandle == 0)
            atomicOpsMutex->fHandle = XMLPlatformUtils::makeMutex(fgMemoryManager);
    }
}

void XMLPlatformUtils::platformTerm()
{
    // delete the mutex we created
    closeMutex(atomicOpsMutex->fHandle);
    atomicOpsMutex->fHandle = 0;
    delete atomicOpsMutex;
    atomicOpsMutex = 0;

    usdetach (arena);
    unlink (arenaName);
    free (arenaName);

    arena = NULL;
    // We don't have any termination requirements at this time
}


void* XMLPlatformUtils::makeMutex(MemoryManager*)
{
    if (arena) {
        usema_t* sema = usnewsema (arena, 1);
        if (sema && (usctlsema (sema, CS_RECURSIVEON) != -1)) {
            return (void*)sema;
        }
        else
            panic(PanicHandler::Panic_MutexErr);
    }
    else {
        // arena==0; therefore platformInit hasn't been called.
        // it's important that we fail quietly here so that we don't
        // throw an exception when trying to initizlize the
        // atomicOpsMutex, which we re-initizlize in platformInit anyay.
        return 0;
    }
}


void XMLPlatformUtils::closeMutex(void* const mtxHandle)
{

    if ((mtxHandle != NULL) && (arena != NULL)) {

        usfreesema ((usema_t *)mtxHandle, arena);
        // never returns anything testable for failure, so nothing
        // to throw an exception about.
    }
}


void XMLPlatformUtils::lockMutex(void* const mtxHandle)
{

    if (mtxHandle != NULL) {
        if (uspsema (mtxHandle) != 1)
            panic(PanicHandler::Panic_MutexErr);
    }

}


void XMLPlatformUtils::unlockMutex(void* const mtxHandle)
{
    if (mtxHandle != NULL)
    {
        if (usvsema(mtxHandle) == -1)
        {
            panic(PanicHandler::Panic_MutexErr);
        }
    }
}

#else

// ---------------------------------------------------------------------------
//  XMLPlatformUtils: Platform init method
// ---------------------------------------------------------------------------

void XMLPlatformUtils::platformInit()
{
    //
    // The atomicOps mutex needs to be created early.
    // Normally, mutexes are created on first use, but there is a
    // circular dependency between compareAndExchange() and
    // mutex creation that must be broken.
    if(!atomicOpsMutex)
    {
        atomicOpsMutex = new (fgMemoryManager) XMLMutex(fgMemoryManager);
        if (atomicOpsMutex->fHandle == 0)
            atomicOpsMutex->fHandle = XMLPlatformUtils::makeMutex(fgMemoryManager);
    }
}

void XMLPlatformUtils::platformTerm()
{
    // delete the mutex we created
	closeMutex(atomicOpsMutex->fHandle);
	atomicOpsMutex->fHandle = 0;
    delete atomicOpsMutex;
    atomicOpsMutex = 0;
}

typedef XMLHolder<pthread_mutex_t>  MutexHolderType;

void* XMLPlatformUtils::makeMutex(MemoryManager* manager)
{
    MutexHolderType* const  holder = new (manager) MutexHolderType;

    pthread_mutexattr_t  attr;
    pthread_mutexattr_init(&attr);
    pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
    if (pthread_mutex_init(&holder->fInstance, &attr))
    {
        delete holder;

        panic(PanicHandler::Panic_MutexErr);
    }
    pthread_mutexattr_destroy(&attr);

    return holder;
}

void XMLPlatformUtils::closeMutex(void* const mtxHandle)
{
    if (mtxHandle != NULL)
    {
        MutexHolderType* const  holder =
            MutexHolderType::castTo(mtxHandle);

        if (pthread_mutex_destroy(&holder->fInstance))
        {
            delete holder;

            ThrowXMLwithMemMgr(XMLPlatformUtilsException,
                     XMLExcepts::Mutex_CouldNotDestroy, fgMemoryManager);
        }
        delete holder;
    }
}


void XMLPlatformUtils::lockMutex(void* const mtxHandle)
{
    if (mtxHandle != NULL)
    {
        if (pthread_mutex_lock(&MutexHolderType::castTo(mtxHandle)->fInstance))
        {
            panic(PanicHandler::Panic_MutexErr);
        }
    }
}


void XMLPlatformUtils::unlockMutex(void* const mtxHandle)
{
    if (mtxHandle != NULL)
    {
        if (pthread_mutex_unlock(&MutexHolderType::castTo(mtxHandle)->fInstance))
        {
            panic(PanicHandler::Panic_MutexErr);
        }
    }
}

#endif   // XML_USE_SPROC

// -----------------------------------------------------------------------
//  Miscellaneous synchronization methods
// -----------------------------------------------------------------------

void* XMLPlatformUtils::compareAndSwap(void**            toFill
                                     , const void* const newValue
                                     , const void* const toCompare)
{
    XMLMutexLock lockMutex(atomicOpsMutex);

    void *retVal = *toFill;
    if (*toFill == toCompare)
        *toFill = (void *)newValue;

    return retVal;
}

int XMLPlatformUtils::atomicIncrement(int &location)
{
    XMLMutexLock localLock(atomicOpsMutex);

    return ++location;
}

int XMLPlatformUtils::atomicDecrement(int &location)
{
    XMLMutexLock localLock(atomicOpsMutex);

    return --location;
}

#else // #if !defined (APP_NO_THREADS)

void XMLPlatformUtils::platformInit()
{
}

void* XMLPlatformUtils::makeMutex(MemoryManager*)
{
        return 0;
}

void XMLPlatformUtils::closeMutex(void* const mtxHandle)
{
}

void XMLPlatformUtils::lockMutex(void* const mtxHandle)
{
}

void XMLPlatformUtils::unlockMutex(void* const mtxHandle)
{
}

void* XMLPlatformUtils::compareAndSwap (void**             toFill,
                                        const void* const  newValue,
                                        const void* const  toCompare)
{
    void *retVal = *toFill;
    if (*toFill == toCompare)
       *toFill = (void *)newValue;
    return retVal;
}

int XMLPlatformUtils::atomicIncrement(int &location)
{
    return ++location;
}

int XMLPlatformUtils::atomicDecrement(int &location)
{
    return --location;
}

void XMLPlatformUtils::platformTerm()
{
    // We don't have any termination requirements at this time
}

#endif // APP_NO_THREADS

//void XMLPlatformUtils::platformTerm()
//{
    // We don't have any termination requirements at this time
//}

#include <xercesc/util/LogicalPath.c>

XERCES_CPP_NAMESPACE_END

