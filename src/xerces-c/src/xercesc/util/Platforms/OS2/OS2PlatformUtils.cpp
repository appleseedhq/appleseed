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
 * $Id: OS2PlatformUtils.cpp 568078 2007-08-21 11:43:25Z amassari $
 */



// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------

#define INCL_DOSSEMAPHORES
#define INCL_DOSERRORS
#define INCL_DOSMISC
#define INCL_DOSFILEMGR

#include    <xercesc/util/XercesDefs.hpp>
#include    <xercesc/util/PlatformUtils.hpp>
#include    <xercesc/util/RuntimeException.hpp>
#include    <xercesc/util/Janitor.hpp>
#include    <xercesc/util/XMLString.hpp>
#include    <xercesc/util/XMLUniDefs.hpp>
#include    <xercesc/util/PanicHandler.hpp>
#include    <xercesc/util/OutOfMemoryException.hpp>

#include    <stdio.h>
#include    <stdlib.h>
#include    <io.h>
#if defined(XML_USE_ICU_TRANSCODER)
  #include  <xercesc/util/Transcoders/ICU/ICUTransService.hpp>
#elif defined(XML_USE_ICONV_TRANSCODER)
  #include  <xercesc/util/Transcoders/Iconv/IconvTransService.hpp>
#else
  #error A transcoding service must be chosen
#endif

#if defined(XML_USE_INMEMORY_MSGLOADER)
  #include  <xercesc/util/MsgLoaders/InMemory/InMemMsgLoader.hpp>
#else
  #error A message loading service must be chosen
#endif

#if defined(XML_IBMVAOS2)
#include    <builtin.h>
#endif
#include    <OS2.h>

XERCES_CPP_NAMESPACE_BEGIN

// ---------------------------------------------------------------------------
//  XMLPlatformUtils: Platform init and term methods
// ---------------------------------------------------------------------------
void XMLPlatformUtils::platformInit()
{
}

void XMLPlatformUtils::platformTerm()
{
    // We don't have any termination requirements at this time
}

// ---------------------------------------------------------------------------
//  XMLPlatformUtils: File Methods
// ---------------------------------------------------------------------------
unsigned int XMLPlatformUtils::curFilePos(FileHandle theFile
                                          , MemoryManager* const manager)
{
    // Get the current position
    int curPos = ftell( (FILE*)theFile);
    if (curPos == -1)
        ThrowXMLwithMemMgr(XMLPlatformUtilsException, XMLExcepts::File_CouldNotGetCurPos, manager);

    return (unsigned int)curPos;
}

void XMLPlatformUtils::closeFile(FileHandle theFile
                                 , MemoryManager* const manager)
{
    if (fclose((FILE*)theFile))
        ThrowXMLwithMemMgr(XMLPlatformUtilsException, XMLExcepts::File_CouldNotCloseFile, manager);
}

unsigned int XMLPlatformUtils::fileSize(FileHandle theFile
                                        , MemoryManager* const manager)
{
    return (unsigned int)filelength(fileno((FILE *)theFile));
}

FileHandle XMLPlatformUtils::openFile(const char* const fileName
                                      , MemoryManager* const manager)
{
    FileHandle retVal = (FILE*)fopen( fileName , "rb" );

    if (retVal == NULL)
        return 0;
    return retVal;
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

FileHandle XMLPlatformUtils::openStdInHandle(MemoryManager* const manager)
{
    return (FileHandle)fdopen(dup(0), "rb");
}

unsigned int XMLPlatformUtils::readFileBuffer ( FileHandle      theFile
                            , const unsigned int    toRead
                            ,       XMLByte* const  toFill 
                            , MemoryManager* const manager)
{
    size_t noOfItemsRead = fread( (void*) toFill, 1, toRead, (FILE*)theFile);

    if(ferror((FILE*)theFile))
    {
        ThrowXMLwithMemMgr(XMLPlatformUtilsException, XMLExcepts::File_CouldNotReadFromFile, manager);
    }

    return (unsigned int)noOfItemsRead;
}

void XMLPlatformUtils::resetFile(FileHandle theFile
                                 , MemoryManager* const manager)
{
    // Seek to the start of the file
    if (fseek((FILE*)theFile, 0, SEEK_SET) )
        ThrowXMLwithMemMgr(XMLPlatformUtilsException, XMLExcepts::File_CouldNotResetFile, manager);
}



// -----------------------------------------------------------------------
//  File system methods
// -----------------------------------------------------------------------
XMLCh* XMLPlatformUtils::getFullPath(const XMLCh* const srcPath,
                                     MemoryManager* const manager)
{
    // Transcode the incoming string
    char* tmpSrcPath = XMLString::transcode(srcPath, fgMemoryManager);
    ArrayJanitor<char> janSrcPath(tmpSrcPath, fgMemoryManager);

    char tmpPath[CCHMAXPATH];
    _fullpath(tmpPath, tmpSrcPath, CCHMAXPATH);

    return XMLString::transcode(tmpPath, manager);
}

bool XMLPlatformUtils::isRelative(const XMLCh* const toCheck
                                  , MemoryManager* const manager)
{
    // Check for pathological case of an empty path
    if (!toCheck[0])
        return false;

    //
    //  If it starts with a drive, then it cannot be relative. Note that
    //  we checked the drive not being empty above, so worst case it's one
    //  char long and the check of the 1st char will fail because it's really
    //  a null character.
    //
    if (toCheck[1] == chColon)
    {
        if (((toCheck[0] >= chLatin_A) && (toCheck[0] <= chLatin_Z))
        ||  ((toCheck[0] >= chLatin_a) && (toCheck[0] <= chLatin_z)))
        {
            return false;
        }
    }

    //
    //  If it starts with a double slash, then it cannot be relative since
    //  its a remote file.
    //
    if ((toCheck[0] == chBackSlash) && (toCheck[1] == chBackSlash))
        return false;

    // Else assume its a relative path
    return true;
}

XMLCh* XMLPlatformUtils::getCurrentDirectory(MemoryManager* const manager)
{

    /*** 
     *  REVISIT:
     * 
     *   To be implemented later
    ***/

    XMLCh curDir[]={ chPeriod, chForwardSlash, chNull};
    return getFullPath(curDir, manager);
}

inline bool XMLPlatformUtils::isAnySlash(XMLCh c) 
{
    return ( chBackSlash == c || chForwardSlash == c);
}

// -----------------------------------------------------------------------
//  Timing methods
// -----------------------------------------------------------------------
unsigned long XMLPlatformUtils::getCurrentMillis()
{
    APIRET  retr;
    ULONG   timerBuf = 0;

    retr =  DosQuerySysInfo( QSV_MS_COUNT, QSV_MS_COUNT, (PVOID) &timerBuf,
                             sizeof( ULONG ) );
//  if ( retr != NO_ERROR )
//     return (timerBuf);


    return (timerBuf);
}



// -----------------------------------------------------------------------
//  Mutex methods
// -----------------------------------------------------------------------
void XMLPlatformUtils::closeMutex(void* const mtxHandle)
{
#if ! defined(APP_NO_THREADS)
    if (mtxHandle == NULL)
      return;

    if (DosCloseMutexSem( (HMTX)mtxHandle))
    {
      ThrowXMLwithMemMgr(XMLPlatformUtilsException, XMLExcepts::Mutex_CouldNotDestroy, fgMemoryManager);
    }
#endif
}

void XMLPlatformUtils::lockMutex(void* const mtxHandle)
{
#if ! defined(APP_NO_THREADS)
    if (mtxHandle == NULL)
      return;

    if (DosRequestMutexSem( (HMTX)mtxHandle,(ULONG) SEM_INDEFINITE_WAIT) )
    {
      panic(PanicHandler::Panic_MutexErr);
    }
#endif
}

void* XMLPlatformUtils::makeMutex(MemoryManager*)
{
#if ! defined(APP_NO_THREADS)
    HMTX hRet; // Mutex Handle

    if (DosCreateMutexSem(NULL, &hRet, 0, FALSE))
        panic(PanicHandler::Panic_MutexErr);
    return (void*)hRet;
#else
    return 0;
#endif
}

void XMLPlatformUtils::unlockMutex(void* const mtxHandle)
{
#if ! defined(APP_NO_THREADS)
    if (mtxHandle == NULL)
       return;

    if (DosReleaseMutexSem( (HMTX)mtxHandle))
    {
        panic(PanicHandler::Panic_MutexErr);
    }
#endif
}



// -----------------------------------------------------------------------
//  Miscellaneous synchronization methods
// -----------------------------------------------------------------------
void* XMLPlatformUtils::compareAndSwap ( void**      toFill
                                       , const void* const newValue
                                       , const void* const toCompare )
{
#if defined(XML_IBMVA4_OS2)
    return (void *)__smp_cmpxchg4((unsigned int *)toFill,
                                  (unsigned int)newValue,
                                  (unsigned int)toCompare);
#elif defined(__GNUG__)
    char ret;
    long int readval;
    long int * p    = (long **)toFill;

    __asm__ __volatile__ ("lock; cmpxchgl %3, %1\n\tsete %0"
                          : "=q" (ret), "=m" (*p), "=a" (readval)
                          : "r" (newValue), "m" (*p), "a" (toCompare)
                          : "memory");
    return (void *)(long)ret;
#else
    void * retVal = *toFill;
    if (*toFill == toCompare)
      *toFill = (void *)newValue;

    return retVal;
#endif
}



// -----------------------------------------------------------------------
//  Atomic Increment and Decrement
//
//  The function return value is positive if the result of the operation
//  was positive. Zero if the result of the operation was zero. Negative
//  if the result of the operation was negative. Except for the zero
//  case, the value returned may differ from the actual result of the
//  operation - only the sign and zero/nonzero state is guaranteed to be
//  correct.
// -----------------------------------------------------------------------
int XMLPlatformUtils::atomicIncrement(int& location)
{
#if defined(XML_IBMVA4_OS2)
    return __smp_inc4(&location);
#elif defined(__GNUG__)
    __asm__ __volatile__ ("lock; incl %0" : "=m" (location) : );
    return location;
#else
    return ++location;
#endif
}

int XMLPlatformUtils::atomicDecrement(int& location)
{
#if defined(XML_IBMVA4_OS2)
    return __smp_dec4(&location);
#elif defined(__GNUG__)
    __asm__ __volatile__ ("lock; decl %0" : "=m" (location) : );
    return location;
#else
    return --location;
#endif
}


// ---------------------------------------------------------------------------
//  XMLPlatformUtils: The panic method
// ---------------------------------------------------------------------------
void XMLPlatformUtils::panic(const PanicHandler::PanicReasons reason)
{
    fgUserPanicHandler? fgUserPanicHandler->panic(reason) : fgDefaultPanicHandler->panic(reason);	
}


// -----------------------------------------------------------------------
//  Private static methods. These are provided by the per-platform
//  implementation files.
// -----------------------------------------------------------------------
XMLMsgLoader* XMLPlatformUtils::loadAMsgSet(const XMLCh* const msgDomain)
{
    XMLMsgLoader* retVal;
    try
    {
#if defined(XML_USE_INMEMORY_MSGLOADER)
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

XMLNetAccessor* XMLPlatformUtils::makeNetAccessor()
{
  return 0;
}

XMLTransService* XMLPlatformUtils::makeTransService()
{
#if defined(XML_USE_ICU_TRANSCODER)
    return new (fgMemoryManager) ICUTransService;
#elif defined(XML_USE_ICONV_TRANSCODER)
    return new (fgMemoryManager) IconvTransService;
#else
    return 0;
#endif
}

#include <xercesc/util/LogicalPath.c>

XERCES_CPP_NAMESPACE_END

