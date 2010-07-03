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
 * $Id: SolarisPlatformUtils.cpp 568078 2007-08-21 11:43:25Z amassari $
 */


// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------

#if !defined (APP_NO_THREADS)
#  if defined (XML_USE_DCE)
#    include  <dce/pthread.h>
#  else
#    include    <pthread.h>
#  endif
#endif // APP_NO_THREADS


#include    <unistd.h>
#include    <stdio.h>
#include    <stdlib.h>
#include    <errno.h>
#include    <libgen.h>
#include    <sys/timeb.h>
#include    <string.h>
#include    <link.h>
#include    <limits.h>
#include    <dlfcn.h>
#include    <sys/types.h>
#include    <sys/stat.h>
#include    <fcntl.h>
#include    <xercesc/util/Janitor.hpp>
#include    <xercesc/util/PlatformUtils.hpp>
#include    <xercesc/util/RuntimeException.hpp>
#include    <xercesc/util/Mutexes.hpp>
#include    <xercesc/util/XMLHolder.hpp>
#include    <xercesc/util/XMLString.hpp>
#include    <xercesc/util/XMLUniDefs.hpp>
#include    <xercesc/util/XMLUni.hpp>
#include    <xercesc/util/PanicHandler.hpp>
#include    <xercesc/util/OutOfMemoryException.hpp>

#if defined (XML_USE_ICU_TRANSCODER)
    #include <xercesc/util/Transcoders/ICU/ICUTransService.hpp>
#else   // use native transcoder
    #include <xercesc/util/Transcoders/Iconv/IconvTransService.hpp>
#endif

#if defined (XML_USE_ICU_MESSAGELOADER)
    #include <xercesc/util/MsgLoaders/ICU/ICUMsgLoader.hpp>
#elif defined (XML_USE_ICONV_MESSAGELOADER)
    #include <xercesc/util/MsgLoaders/MsgCatalog/MsgCatalogLoader.hpp>
#else   // use In-memory message loader
    #include <xercesc/util/MsgLoaders/InMemory/InMemMsgLoader.hpp>
#endif


#if defined (XML_USE_NETACCESSOR_LIBWWW)
    #include <xercesc/util/NetAccessors/libWWW/LibWWWNetAccessor.hpp>
#elif defined (XML_USE_NETACCESSOR_SOCKET)
    #include <xercesc/util/NetAccessors/Socket/SocketNetAccessor.hpp>
#endif


#if defined (SOLARIS)
extern "C" int ftime(struct timeb *); // Solaris headers missing this decl
#endif

XERCES_CPP_NAMESPACE_BEGIN

// ---------------------------------------------------------------------------
//  XMLPlatformUtils: Private Static Methods
// ---------------------------------------------------------------------------

XMLNetAccessor* XMLPlatformUtils::makeNetAccessor()
{
#if defined (XML_USE_NETACCESSOR_LIBWWW)
    return new (fgMemoryManager) LibWWWNetAccessor();
#elif defined (XML_USE_NETACCESSOR_SOCKET)
    return new (fgMemoryManager) SocketNetAccessor();
#else
    return 0;
#endif
}



//
//  This method is called by the platform independent part of this class
//  when client code asks to have one of the supported message sets loaded.
//  In our case, we use the ICU based message loader mechanism.
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

#if defined (XML_USE_ICU_TRANSCODER)
{
    return new (fgMemoryManager) ICUTransService;
}
#elif defined (XML_USE_ICONV_TRANSCODER)
{
    return new (fgMemoryManager) IconvTransService;
}
#else // Use Native transcoding service
{
    return new (fgMemoryManager) IconvTransService;

}
#endif


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
    // Get the current position
    int curPos = tell( (int)theFile);
    if (curPos == -1)
        ThrowXMLwithMemMgr(XMLPlatformUtilsException,
                 XMLExcepts::File_CouldNotGetSize, manager);

    return (unsigned int)curPos;
}

void XMLPlatformUtils::closeFile(FileHandle theFile
                                 , MemoryManager* const manager)
{
    if (close((int) theFile))
        ThrowXMLwithMemMgr(XMLPlatformUtilsException,
                 XMLExcepts::File_CouldNotCloseFile, manager);
}

unsigned int XMLPlatformUtils::fileSize(FileHandle theFile
                                        , MemoryManager* const manager)
{
    // Get the current position
    long  int curPos = tell((int) theFile);
    if (curPos == -1)
        ThrowXMLwithMemMgr(XMLPlatformUtilsException,
                 XMLExcepts::File_CouldNotGetCurPos, manager);

    // Seek to the end and save that value for return
    if (lseek( (int) theFile, 0, SEEK_END) == (off_t)-1)
        ThrowXMLwithMemMgr(XMLPlatformUtilsException,
                 XMLExcepts::File_CouldNotSeekToEnd, manager);

    long int retVal = tell((int) theFile);
    if (retVal == -1)
        ThrowXMLwithMemMgr(XMLPlatformUtilsException,
                 XMLExcepts::File_CouldNotSeekToEnd, manager);

    // And put the pointer back
    if (lseek((int) theFile, curPos, SEEK_SET) == (off_t)-1)
        ThrowXMLwithMemMgr(XMLPlatformUtilsException,
                 XMLExcepts::File_CouldNotSeekToPos, manager);

    return (unsigned int)retVal;
}

FileHandle XMLPlatformUtils::openFile(const char* const fileName
                                      , MemoryManager* const manager)
{
    return (FileHandle) open( fileName , O_RDONLY | O_LARGEFILE);
}

FileHandle XMLPlatformUtils::openFile(const XMLCh* const fileName
                                      , MemoryManager* const manager)
{
    const char* tmpFileName = XMLString::transcode(fileName, manager);
    ArrayJanitor<char> janText((char*)tmpFileName, manager);
    return (FileHandle) open( tmpFileName , O_RDONLY | O_LARGEFILE); 
}

FileHandle XMLPlatformUtils::openFileToWrite(const XMLCh* const fileName
                                             , MemoryManager* const manager)
{
    const char* tmpFileName = XMLString::transcode(fileName, manager);
    ArrayJanitor<char> janText((char*)tmpFileName, manager);
    return (FileHandle)open( tmpFileName , O_WRONLY | O_CREAT | O_TRUNC | O_LARGEFILE, 0666);
}

FileHandle XMLPlatformUtils::openFileToWrite(const char* const fileName
                                             , MemoryManager* const manager)
{
    return (FileHandle)open( fileName , O_WRONLY | O_CREAT | O_TRUNC | O_LARGEFILE, 0666);
}

unsigned int
XMLPlatformUtils::readFileBuffer(FileHandle              theFile
                               , const unsigned int      toRead
                               , XMLByte* const          toFill
                               , MemoryManager* const    manager)
{
    ssize_t noOfItemsRead =
      read((int) theFile, (void*) toFill, toRead);

    if(noOfItemsRead == -1)
    {
        ThrowXMLwithMemMgr(XMLPlatformUtilsException,
                 XMLExcepts::File_CouldNotReadFromFile, manager);
    }

    return (unsigned int) noOfItemsRead;
}

void
XMLPlatformUtils::writeBufferToFile( FileHandle     const  theFile
                                   , long                  toWrite
                                   , const XMLByte* const  toFlush
                                   , MemoryManager*  const manager)
{
    if (!theFile        ||
        (toWrite <= 0 ) ||
        !toFlush         )
        return;

    const XMLByte* tmpFlush = (const XMLByte*) toFlush;
    ssize_t bytesWritten = 0;

    while (true)
    {
      bytesWritten=write((int)theFile, (const void *)tmpFlush, toWrite);

        if(bytesWritten == -1)
        {
            ThrowXMLwithMemMgr(XMLPlatformUtilsException, XMLExcepts::File_CouldNotWriteToFile, manager);
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
    if (lseek((int) theFile, 0, SEEK_SET) == -1)
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
    //  NOTE: THe path provided has always already been opened successfully,
    //  so we know that its not some pathological freaky path. It comes in
    //  in native format, and goes out as Unicode always
    //
    char* newSrc = XMLString::transcode(srcPath, manager);
    ArrayJanitor<char> janText(newSrc, manager);

    // Use a local buffer that is big enough for the largest legal path
    char absPath[PATH_MAX + 1];
    //get the absolute path
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

#if !defined (APP_NO_THREADS)

// ---------------------------------------------------------------------------
//  XMLPlatformUtils: Platform init method
// ---------------------------------------------------------------------------

typedef XMLHolder<pthread_mutex_t>  MutexHolderType;

static MutexHolderType* gAtomicOpMutex = 0;

void XMLPlatformUtils::platformInit()
{
    //
    // The gAtomicOpMutex mutex needs to be created
    // because compareAndSwap and incrementlocation and decrementlocation
    // does not have the atomic system calls for usage
    // Normally, mutexes are created on first use, but there is a
    // circular dependency between compareAndExchange() and
    // mutex creation that must be broken.

    gAtomicOpMutex = new (fgMemoryManager) MutexHolderType;

#if defined(XML_USE_DCE)
    if (pthread_mutex_init(&gAtomicOpMutex->fInstance, pthread_mutexattr_default)) {
#else // XML_USE_DCE
    if (pthread_mutex_init(&gAtomicOpMutex->fInstance, NULL)) {
#endif // XML_USE_DCE
	    delete gAtomicOpMutex;
	    gAtomicOpMutex = 0;
        panic( PanicHandler::Panic_SystemInit );
    }
}

#ifndef XML_USE_DCE
// inlining the class with dce threading causes segmentation fault
class  RecursiveMutex : public XMemory
{
public:
    pthread_mutex_t         mutex;
    int                     recursionCount;
    pthread_t               tid;
    MemoryManager* const    fMemoryManager;

    RecursiveMutex(MemoryManager* manager) :
        mutex(),
        recursionCount(0),
        tid(0),
        fMemoryManager(manager) {
               if (pthread_mutex_init(&mutex, NULL))
                   XMLPlatformUtils::panic(PanicHandler::Panic_MutexErr);
                     }

    ~RecursiveMutex() {
            if (pthread_mutex_destroy(&mutex))
                ThrowXMLwithMemMgr(XMLPlatformUtilsException, XMLExcepts::Mutex_CouldNotDestroy, fMemoryManager);
                      }

     void lock()      {
              if (pthread_equal(tid, pthread_self()))
              {
                  recursionCount++;
                  return;
              }
              if (pthread_mutex_lock(&mutex) != 0)
                  XMLPlatformUtils::panic(PanicHandler::Panic_MutexErr);
              tid = pthread_self();
              recursionCount = 1;
              }


     void unlock()    {
                          if (--recursionCount > 0)
                              return;

              if (pthread_mutex_unlock(&mutex) != 0)
                  XMLPlatformUtils::panic(PanicHandler::Panic_MutexErr);
                          tid = 0;
                       }
   };
#endif // ifndef XML_USE_DCE

void* XMLPlatformUtils::makeMutex(MemoryManager* manager)
{
#if defined(XML_USE_DCE)

    MutexHolderType* const  holder = new (manager) MutexHolderType;

    if (holder ==  NULL)
    {
        panic(PanicHandler::Panic_MutexErr);
    }
    pthread_mutexattr_t attr;
    pthread_mutexattr_create(&attr);
    pthread_mutexattr_setkind_np(&attr, MUTEX_RECURSIVE_NP);
    if (pthread_mutex_init(&holder->fInstance, attr))
    {
        panic(PanicHandler::Panic_MutexErr);
    }
    pthread_mutexattr_delete(&attr);
    return holder;
#else
    return new (manager) RecursiveMutex(manager);
#endif
}


void XMLPlatformUtils::closeMutex(void* const mtxHandle)
{
    if (mtxHandle == NULL)
        return;
#if defined(XML_USE_DCE)
    MutexHolderType *rm = MutexHolderType::castTo(mtxHandle);
    pthread_mutex_destroy(&rm->fInstance);
#else
    RecursiveMutex *rm = (RecursiveMutex *)mtxHandle;
#endif
    delete rm;
}


void XMLPlatformUtils::lockMutex(void* const mtxHandle)
{
    if (mtxHandle == NULL)
        return;
#if defined(XML_USE_DCE)
    MutexHolderType *rm = MutexHolderType::castTo(mtxHandle);

    pthread_mutex_lock(&rm->fInstance);
#else
    RecursiveMutex *rm = (RecursiveMutex *)mtxHandle;
    rm->lock();
#endif
}

void XMLPlatformUtils::unlockMutex(void* const mtxHandle)
{
    if (mtxHandle == NULL)
        return;
#if defined(XML_USE_DCE)
    MutexHolderType *rm = MutexHolderType::castTo(mtxHandle);

    pthread_mutex_unlock(&rm->fInstance);
#else
    RecursiveMutex *rm = (RecursiveMutex *)mtxHandle;
    rm->unlock();
#endif
}

// -----------------------------------------------------------------------
//  Miscellaneous synchronization methods
// -----------------------------------------------------------------------
//atomic system calls in Solaris is only restricted to kernel libraries
//So, to make operations thread safe we implement static mutex and lock
//the atomic operations. It makes the process slow but what's the alternative!

void* XMLPlatformUtils::compareAndSwap ( void**      toFill ,
                    const void* const newValue ,
                    const void* const toCompare)
{
    //return ((void*)cas32( (uint32_t*)toFill,  (uint32_t)toCompare, (uint32_t)newValue) );
    // the below calls are temporarily made till the above functions are part of user library
    // Currently its supported only in the kernel mode

    if (pthread_mutex_lock(&gAtomicOpMutex->fInstance))
        panic(PanicHandler::Panic_SynchronizationErr);

    void *retVal = *toFill;
    if (*toFill == toCompare)
              *toFill = (void *)newValue;

    if (pthread_mutex_unlock(&gAtomicOpMutex->fInstance))
        panic(PanicHandler::Panic_SynchronizationErr);

    return retVal;
}

int XMLPlatformUtils::atomicIncrement(int &location)
{
    //return (int)atomic_add_32_nv( (uint32_t*)&location, 1);

    if (pthread_mutex_lock(&gAtomicOpMutex->fInstance))
        panic(PanicHandler::Panic_SynchronizationErr);

    int tmp = ++location;

    if (pthread_mutex_unlock(&gAtomicOpMutex->fInstance))
        panic(PanicHandler::Panic_SynchronizationErr);

    return tmp;
}
int XMLPlatformUtils::atomicDecrement(int &location)
{
    //return (int)atomic_add_32_nv( (uint32_t*)&location, -1);

    if (pthread_mutex_lock(&gAtomicOpMutex->fInstance))
        panic(PanicHandler::Panic_SynchronizationErr);

    int tmp = --location;

    if (pthread_mutex_unlock(&gAtomicOpMutex->fInstance))
        panic(PanicHandler::Panic_SynchronizationErr);

    return tmp;
}

#else // #if !defined (APP_NO_THREADS)

void XMLPlatformUtils::platformInit()
{
}

void XMLPlatformUtils::closeMutex(void* const)
{
}

void XMLPlatformUtils::lockMutex(void* const)
{
}

void* XMLPlatformUtils::makeMutex(MemoryManager*)
{
        return 0;
}

void XMLPlatformUtils::unlockMutex(void* const)
{
}

void* XMLPlatformUtils::compareAndSwap ( void**      toFill,
                                   const void* const newValue,
                                   const void* const toCompare)
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



FileHandle XMLPlatformUtils::openStdInHandle(MemoryManager* const manager)
{
        return (FileHandle)dup(0);
}

void XMLPlatformUtils::platformTerm()
{
#if !defined(APP_NO_THREADS)
	pthread_mutex_destroy(&gAtomicOpMutex->fInstance);
    delete gAtomicOpMutex;
	gAtomicOpMutex = 0;
#endif
}

#include <xercesc/util/LogicalPath.c>

XERCES_CPP_NAMESPACE_END
