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
 * 
 *
 */


// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------

#if !defined(APP_NO_THREADS)
#include    <pthread.h>
#endif

/* #ifndef __USE_UNIX98
    #error __USE_UNIX98 is not defined in your compile settings
#endif */

#include    <unistd.h>
#include    <stdio.h>
#include    <stdlib.h>
#include    <errno.h>
#include    <libgen.h>
#include    <sys/time.h>
#include    <string.h>
#include    <strings.h>/* for strcasecmp & strncasecmp */
#include    <wchar.h>  /* for win_t */
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
#else
    // Same as -DXML_USE_INMEM_MESSAGELOADER
    #include <xercesc/util/MsgLoaders/InMemory/InMemMsgLoader.hpp>
#endif


#if defined (XML_USE_NETACCESSOR_SOCKET)
    #include <xercesc/util/NetAccessors/Socket/SocketNetAccessor.hpp>
#endif

XERCES_CPP_NAMESPACE_BEGIN

// ---------------------------------------------------------------------------
//  Local Methods
// ---------------------------------------------------------------------------

static void WriteCharStr( FILE* stream, const char* const toWrite)
{
    if (fputs(toWrite, stream) == EOF)
    {
        ThrowXML(XMLPlatformUtilsException,
                 XMLExcepts::Strm_StdErrWriteFailure);
    }
}

static void WriteUStrStdErr( const XMLCh* const toWrite)
{
    char* tmpVal = XMLString::transcode(toWrite, XMLPlatformUtils::fgMemoryManager);
    ArrayJanitor<char> janText(tmpVal, XMLPlatformUtils::fgMemoryManager);
    if (fputs(tmpVal, stderr) == EOF)
    {
       ThrowXML(XMLPlatformUtilsException,
                XMLExcepts::Strm_StdErrWriteFailure);
   }
}

static void WriteUStrStdOut( const XMLCh* const toWrite)
 {
    char* tmpVal = XMLString::transcode(toWrite, XMLPlatformUtils::fgMemoryManager);
    ArrayJanitor<char> janText(tmpVal, XMLPlatformUtils::fgMemoryManager);
    if (fputs(tmpVal, stdout) == EOF)
    {
        ThrowXML(XMLPlatformUtilsException,
                 XMLExcepts::Strm_StdOutWriteFailure);
    }
}

XMLNetAccessor* XMLPlatformUtils::makeNetAccessor()
{
#if defined (XML_USE_NETACCESSOR_SOCKET)
    return new (fgMemoryManager) SocketNetAccessor();
#else
    return 0;
#endif
}

// ---------------------------------------------------------------------------
//  XMLPlatformUtils: Private Static Methods
// ---------------------------------------------------------------------------

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
    if (theFile == NULL)
        ThrowXMLwithMemMgr(XMLPlatformUtilsException,
                 XMLExcepts::CPtr_PointerIsZero, manager);
    int curPos = ftell( (FILE*)theFile);
    if (curPos == -1)
        ThrowXMLwithMemMgr(XMLPlatformUtilsException,
                 XMLExcepts::File_CouldNotGetSize, manager);

    return (unsigned int)curPos;
}

void XMLPlatformUtils::closeFile(FileHandle theFile
                                 , MemoryManager* const manager)
{
    if (theFile == NULL)
        ThrowXMLwithMemMgr(XMLPlatformUtilsException,
                 XMLExcepts::CPtr_PointerIsZero, manager);
    if (fclose((FILE*)theFile))
        ThrowXMLwithMemMgr(XMLPlatformUtilsException,
                 XMLExcepts::File_CouldNotCloseFile, manager);
}

unsigned int XMLPlatformUtils::fileSize(FileHandle theFile
                                        , MemoryManager* const manager)
{
    if (theFile == NULL)
        ThrowXMLwithMemMgr(XMLPlatformUtilsException,
                 XMLExcepts::CPtr_PointerIsZero, manager);
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
    if (fileName == NULL)
        ThrowXMLwithMemMgr(XMLPlatformUtilsException,
                 XMLExcepts::CPtr_PointerIsZero, manager);
    const char* tmpFileName = XMLString::transcode(fileName, manager);
    ArrayJanitor<char> janText((char*)tmpFileName, manager);
    FileHandle retVal = (FILE*)fopen( tmpFileName , "r" );

    if (retVal == NULL)
        return 0;
    return retVal;
}

FileHandle XMLPlatformUtils::openFile(const char* const fileName
                                      , MemoryManager* const manager)
{
    if (fileName == NULL)
        ThrowXMLwithMemMgr(XMLPlatformUtilsException,
                 XMLExcepts::CPtr_PointerIsZero, manager);
    FileHandle retVal = (FILE*)fopen( fileName , "r" );

    if (retVal == NULL)
        return 0;
    return retVal;
}

FileHandle XMLPlatformUtils::openFileToWrite(const XMLCh* const fileName
                                             , MemoryManager* const manager)
{
    if (fileName == NULL)
        ThrowXMLwithMemMgr(XMLPlatformUtilsException,
                 XMLExcepts::CPtr_PointerIsZero, manager);
    const char* tmpFileName = XMLString::transcode(fileName, manager);
    ArrayJanitor<char> janText((char*)tmpFileName, manager);

    FileHandle retVal = (FILE*)fopen( tmpFileName, "w" );
    if (retVal == NULL)
        return 0;
    return retVal;
}

FileHandle XMLPlatformUtils::openFileToWrite(const char* const fileName
                                             , MemoryManager* const manager)
{
    if (fileName == NULL)
        ThrowXMLwithMemMgr(XMLPlatformUtilsException,
                 XMLExcepts::CPtr_PointerIsZero, manager);
    FileHandle retVal = (FILE*)fopen( fileName, "w" );

    if (retVal == NULL)
        return 0;
    return retVal;
}

FileHandle XMLPlatformUtils::openStdInHandle(MemoryManager* const manager)
{
    int nfd = dup(0);
    if (nfd == -1)
        ThrowXMLwithMemMgr(XMLPlatformUtilsException,
                XMLExcepts::File_CouldNotDupHandle, manager);
    return (FileHandle)fdopen(dup(0), "r");
}



unsigned int
XMLPlatformUtils::readFileBuffer( FileHandle           theFile
                                , const unsigned int   toRead
                                , XMLByte* const       toFill
                                , MemoryManager* const manager)
{
    if ( !theFile || !toFill )
        ThrowXMLwithMemMgr(XMLPlatformUtilsException,
                 XMLExcepts::CPtr_PointerIsZero, manager);
    if (toRead == 0)
        return 0;
    size_t noOfItemsRead = fread((void*) toFill, 1, toRead, (FILE*)theFile);

    if(ferror((FILE*)theFile))
    {
        ThrowXMLwithMemMgr(XMLPlatformUtilsException,
                 XMLExcepts::File_CouldNotReadFromFile, manager);
    }

    return (unsigned int)noOfItemsRead;
}

void XMLPlatformUtils::writeBufferToFile( FileHandle     const  theFile
                                        , long                  toWrite
                                        , const XMLByte* const  toFlush 
                                        , MemoryManager* const  manager)
{
    if ( !theFile || !toFlush )
        ThrowXMLwithMemMgr(XMLPlatformUtilsException,
                 XMLExcepts::CPtr_PointerIsZero, manager);
    if ( toWrite <= 0 )
        return;

    const XMLByte* tmpFlush = (const XMLByte*) toFlush;
    size_t bytesWritten = 0;

    while (true)
    {
        bytesWritten = fwrite(tmpFlush, sizeof(XMLByte), toWrite, (FILE*)theFile);

        if(ferror((FILE*)theFile))
        {
#if 0
            ThrowXMLwithMemMgr(XMLPlatformUtilsException,
                     XMLExcepts::File_CouldNotWriteToFile, manager);
#else
            ThrowXMLwithMemMgr(XMLPlatformUtilsException,
                     XMLExcepts::File_CouldNotReadFromFile, manager);
#endif
        }

        if (bytesWritten < (size_t) toWrite) //incomplete write
        {
            tmpFlush += bytesWritten;
            toWrite -= bytesWritten;
            bytesWritten = 0;
        }
        else
            return;
    }
}

void XMLPlatformUtils::resetFile(FileHandle theFile
                                 , MemoryManager* const manager)
{
    if (theFile == NULL)
        ThrowXMLwithMemMgr(XMLPlatformUtilsException,
                 XMLExcepts::CPtr_PointerIsZero, manager);
    // Seek to the start of the file
    if (fseek((FILE*)theFile, 0, SEEK_SET))
        ThrowXMLwithMemMgr(XMLPlatformUtilsException,
                 XMLExcepts::File_CouldNotResetFile, manager);
}


// ---------------------------------------------------------------------------
//  XMLPlatformUtils: Timing Methods
// ---------------------------------------------------------------------------

unsigned long XMLPlatformUtils::getCurrentMillis()
{
    struct timeval t;
    struct timezone tz;

    gettimeofday(&t, &tz);
    return (unsigned long)(t.tv_sec*1000 + t.tv_usec);
}

XMLCh* XMLPlatformUtils::getFullPath(const XMLCh* const srcPath,
                                     MemoryManager* const manager)
{

    //
    //  NOTE: THe path provided has always already been opened successfully,
    //  so we know that its not some pathological freaky path. It comes in
    //  in native format, and goes out as Unicode always
    //
    char* newSrc = XMLString::transcode(srcPath, manager);
    ArrayJanitor<char> janText(newSrc, manager);

    // Use a local buffer that is big enough for the largest legal path
    char absPath[1024];
    // get the absolute path 
    char* retPath = realpath(newSrc, absPath);
    
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


// -----------------------------------------------------------------------
//  Mutex methods
// -----------------------------------------------------------------------

#if !defined(APP_NO_THREADS)

// ---------------------------------------------------------------------------
//  XMLPlatformUtils: Platform init method
// ---------------------------------------------------------------------------

static XMLMutex *atomicOpsMutex = 0;

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

typedef XMLHolder<pthread_mutex_t>  MutexHolderType;

void* XMLPlatformUtils::makeMutex(MemoryManager* manager)
{
    MutexHolderType* const  holder = new (manager) MutexHolderType;

    pthread_mutexattr_t  attr;
    pthread_mutexattr_init(&attr);
    pthread_mutexattr_settype(attr, PTHREAD_MUTEX_RECURSIVE);
    if (pthread_mutex_init(mutex, attr))
    {
        delete holder;

        panic(PanicHandler::Panic_MutexErr);
    }
    pthread_mutexattr_destroy(attr);

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

void XMLPlatformUtils::closeMutex(void* const)
{
}

void XMLPlatformUtils::lockMutex(void* const)
{
}

void XMLPlatformUtils::unlockMutex(void* const)
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

#endif // APP_NO_THREADS

void XMLPlatformUtils::platformTerm()
{
#if !defined(APP_NO_THREADS)
    // delete the mutex we created
        closeMutex(atomicOpsMutex->fHandle);
        atomicOpsMutex->fHandle = 0;
        delete atomicOpsMutex;
        atomicOpsMutex = 0;
#endif
}

#include <xercesc/util/LogicalPath.c>

XERCES_CPP_NAMESPACE_END
