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
 * $Id: OS390PlatformUtils.cpp 568078 2007-08-21 11:43:25Z amassari $
 */


// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#ifdef OS390
#define _OPEN_SYS
#endif

#ifndef APP_NO_THREADS
#include    <pthread.h>
#endif

#include    <xercesc/util/PlatformUtils.hpp>
#include    <xercesc/util/RuntimeException.hpp>
#include    <xercesc/util/Janitor.hpp>
#include    <stdio.h>
#include    <stdlib.h>
#include    <errno.h>
#include    <libgen.h>
#ifdef EXM_OS390
#include    <timeb.h>
#else
#include    <sys/timeb.h>
#endif
#include    <string.h>
#include    <unistd.h>
#include    <limits.h>
#include    <xercesc/util/XMLHolder.hpp>
#include    <xercesc/util/XMLString.hpp>
#include    <xercesc/util/XMLUniDefs.hpp>
#include    <xercesc/util/XMLUni.hpp>
#include    <xercesc/util/PanicHandler.hpp>
#include    "Path390.hpp"
#include    "FileHandleImpl.hpp"
#include    <xercesc/util/OutOfMemoryException.hpp>

#if defined (XML_USE_ICU_TRANSCODER)
    #include <xercesc/util/Transcoders/ICU/ICUTransService.hpp>
#elif defined (XML_USE_UNICONV390_TRANSCODER)
    #include <xercesc/util/Transcoders/Uniconv390/Uniconv390TransService.hpp>
#else   // use native transcoder
    #include <xercesc/util/Transcoders/Iconv390/Iconv390TransService.hpp>
#endif

#if defined (XML_USE_ICU_MESSAGELOADER)
    #include <xercesc/util/MsgLoaders/ICU/ICUMsgLoader.hpp>
#elif defined (XML_USE_ICONV_MESSAGELOADER)
    #include <xercesc/util/MsgLoaders/MsgCatalog/MsgCatalogLoader.hpp>
#else   // use In-memory message loader
    #include <xercesc/util/MsgLoaders/InMemory/InMemMsgLoader.hpp>   //hint for the user to include this file.
#endif
#if defined (XML_USE_NETACCESSOR_SOCKET)
    #include <xercesc/util/NetAccessors/Socket/SocketNetAccessor.hpp>
#endif

XERCES_CPP_NAMESPACE_BEGIN

// ---------------------------------------------------------------------------
//  Static data
// ---------------------------------------------------------------------------
static bool isPosixEnabled = false;

// ---------------------------------------------------------------------------
//  XMLPlatformUtils: Platform init method
// ---------------------------------------------------------------------------

void XMLPlatformUtils::platformInit()
{

// The next conditional section is to turn on support for allowing
// the NEL character as a valid whitespace character. Once this is
// enabled the parser is non-compliant with the XML standard.
// Assumption: Once this is enabled, it cannot be reset and
// should remain the same during the same process.
#if defined XML_ALLOW_NELWS
   try
   {
     XMLPlatformUtils::recognizeNEL(true);
   }
   catch(const OutOfMemoryException&)
   {
     throw;
   }
   catch (...)
   {
      panic(PanicHandler::Panic_SystemInit);
   }
#endif

// The next section checks if Posix is enabled on the OS/390 system.
// It stores the result in the static isPosixEnabled flag.
// The __isPosixOn() native routine uses too much
// overhead to call repeatedly at runtime so this will be the only
// remaining call of it.
// Assumption: Once this is enabled, it cannot be reset and
// should remain the same during the same process.

   try
   {
      if (__isPosixOn())
      {
        if (!isPosixEnabled)
            isPosixEnabled = true;
      }
   }
   catch(const OutOfMemoryException&)
   {
      throw;
   }
   catch (...)
   {
      panic(PanicHandler::Panic_SystemInit);
   }

}

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
#elif defined (XML_USE_UNICONV390_TRANSCODER)
{

    return new (fgMemoryManager) Uniconv390TransService;
}
#else
{
    return new (fgMemoryManager) Iconv390TransService;
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
//  Local Functions
// ---------------------------------------------------------------------------
static void emptyBuffer(XMLByte* writeBuffer,
                        size_t   bytesToWrite,
                        FileHandleImpl* fhiPtr
                        , MemoryManager* const manager)
{
     FILE* fileHandle = (FILE*)fhiPtr->getHandle();
     size_t bytesThisWrite = 0;
     size_t bytesThisPass = 0;
     int typeRecordLrecl = 0;
     bool isTypeRecord = fhiPtr->isRecordType();

     if (isTypeRecord)
        typeRecordLrecl = fhiPtr->getLrecl();

     // Try to write as many bytes as possible at a time to the file.
     // If less than the total bytes were written then loop as many times
     // as necessary until all bytes are written. For the case of
     // an MVS dataset with "type=record" specified, we must not
     // attempt to write more than one logical record at a time
     // or it will fail and not return the number of bytes remaining.

     while (bytesToWrite > 0)
     {
        if ((isTypeRecord) &&
            (bytesToWrite > typeRecordLrecl))
        {
           bytesThisPass = typeRecordLrecl;

        }
        else
           bytesThisPass = bytesToWrite;

        bytesThisWrite = fwrite(writeBuffer, sizeof(XMLByte), bytesThisPass, fileHandle);

        if (ferror(fileHandle))
           ThrowXMLwithMemMgr(XMLPlatformUtilsException, XMLExcepts::File_CouldNotWriteToFile, manager);

        bytesToWrite -= bytesThisWrite;
        writeBuffer += bytesThisWrite;
     }  // while (bytesToWrite > 0)

  return;
}


static FileHandleImpl* openRead(char* tmpFileName
                                , MemoryManager* const manager)
{
     FileHandleImpl* retVal;
     FILE* fileHandle = 0;
     Path390 pathobj;
     pathobj.setPath(tmpFileName);
     int optionBufferSize = strlen("rb,");

     // Check if we have fopen options specified in addition to a
     // filename. If we do then we need to build a buffer that has
     // "rb," followed by the options. Otherwise we just pass "rb"
     // in as the options.

     if (pathobj.getfopenParms())
     {
        optionBufferSize += (strlen(pathobj.getfopenParms()) + 1);
     }

     char* optionBuffer = (char*) manager->allocate(optionBufferSize * sizeof(char));//new char[optionBufferSize];
     ArrayJanitor<char> janText((char*)optionBuffer, manager);
     strcpy(optionBuffer,"rb");

     // Build the options buffer

     if (pathobj.getfopenParms())
     {
        strcpy(optionBuffer + 2, ",");
        strcpy(optionBuffer + 3, pathobj.getfopenParms());
     }

     // If we don't have a valid path specified just exit now

     if (pathobj.getfopenPath() == NULL)
        return 0;

     // Take the file handle returned by fopen and create an instance
     // of the FileHandleImpl class.

     fileHandle = fopen(pathobj.getfopenPath(), optionBuffer);
     retVal = new (manager) FileHandleImpl(fileHandle, _FHI_READ, pathobj.isRecordType(), 0, manager);

     // temp fix for HFS files with "type=record" specified
     // close the file and re-open it without the fopen options
     // since they don't work too well with HFS files.

     if ((pathobj.isRecordType()) && (fileHandle != NULL))
     {
        int fldata_rc = 0;
        fldata_t fileinfo;
        fldata_rc = fldata(fileHandle, pathobj.getfopenPath(), &fileinfo);

        if (fldata_rc)
           ThrowXMLwithMemMgr(XMLPlatformUtilsException, XMLExcepts::File_CouldNotReadFromFile, manager);

        if (fileinfo.__dsorgHFS)
        {
           if (fclose(fileHandle))
              ThrowXMLwithMemMgr(XMLPlatformUtilsException, XMLExcepts::File_CouldNotCloseFile, manager);

           fileHandle=fopen(pathobj.getfopenPath(), "rb");
           retVal->setHandle(fileHandle);
           retVal->setRecordType(_FHI_NOT_TYPE_RECORD);
        }
     }
     // End temp fix ......

    if ((fileHandle == NULL) && (pathobj.getPathType() == PATH390_OTHER)) {
    //
    // We failed to open the file using its native format (OE or MVS)
    // Try to go an extra step to map the path into a MVS dataset under BATCH:
    //
    //     /path/path2/filename.ext => //'path.path2.ext(filename)'
    //     /path/path2/filename     => //'path.path2.filename'
    // and
    //     path/path2/filename.ext  => //path.path2.ext(filename)
    //     path/path2/filename      => //path.path2.filename

    char* datasetName = (char*) manager->allocate
    (
        (strlen(tmpFileName) + 5) * sizeof(char)
    );//new char[ strlen(tmpFileName) + 5 ];
    ArrayJanitor<char> janText1((char*)datasetName, manager);
    char *datasetPos = datasetName, *tmpPos = tmpFileName;

    // We are in EBCDIC mode here
    // Specify "//" to indicate that the filename refers to a non-POSIX file
    // or a MVS data set

    strcpy(datasetName, "//");
    datasetPos += 2;

    // If we have a leading '/' then the path is absolute

    if( *tmpPos == '/' ) {
        *datasetPos++ = '\'';
        tmpPos++;
    }

    // Find the last '/' in the path - this seperates the path from the
    // filename.  Then copy the pathname.

    char* pathEnd = strrchr( tmpPos, '/' );
    if( pathEnd == NULL ) pathEnd = tmpPos - 1;
    while( tmpPos <= pathEnd ) {
        switch( *tmpPos ) {
                case '/':
                        *datasetPos = '.';
                        break;
                default:
                        *datasetPos = *tmpPos;
        }
        datasetPos++; tmpPos++;
    }

    // Now we try to locate the extension, and copy that.

    char* extStart = strrchr( tmpFileName, '.' );
    if ( extStart != NULL ) {
        tmpPos = extStart + 1;
        while( *tmpPos != '\0' ) {
                *datasetPos++ = *tmpPos++;
        }
        *datasetPos++ = '(';
    }

    // Now we copy in the filename.

    tmpPos = pathEnd + 1;
    while( *tmpPos != '\0' && ((extStart == NULL) || (tmpPos < extStart)) ) {
        *datasetPos++ = *tmpPos++;
    }

    // Finally cap off the filename with optional ")" and "'", plus a null
    if( extStart != NULL ) *datasetPos++ = ')';
    if( *tmpFileName == '/' ) *datasetPos++ = '\'';
    *datasetPos = '\0';

    fileHandle = fopen( datasetName , optionBuffer);
    retVal->setHandle(fileHandle);

    }

    // fix for file:// protocol
    // the tmpFileName has a prefix of //absolute path
    // for example, file:////u/.... instead of file:///u/....
    // the fopen() on OS/390 cannot open a //u/... POSIX file

    if ((fileHandle == NULL) && (pathobj.getPathType() == PATH390_OTHER))
    {
       if ((tmpFileName[0] == '/') && (tmpFileName[1] == '/'))
        {
            char *srcName = tmpFileName + 1; // points past the first '/'
            fileHandle = fopen(srcName , optionBuffer);

            retVal->setHandle(fileHandle);

            // temp fix for HFS files with type=record specified
            // close the file and re-open it without the fopen options
            // since they don't work too well with HFS files.

            if (pathobj.isRecordType())
            {
               int fldata_rc = 0;
               fldata_t fileinfo;
               fldata_rc = fldata(fileHandle, srcName, &fileinfo);

               if (fldata_rc)
                 ThrowXMLwithMemMgr(XMLPlatformUtilsException, XMLExcepts::File_CouldNotReadFromFile, manager);

               if (fileinfo.__dsorgHFS)
               {
                  if (fclose(fileHandle))
                     ThrowXMLwithMemMgr(XMLPlatformUtilsException, XMLExcepts::File_CouldNotCloseFile, manager);

                  fileHandle=fopen(srcName, "rb");
                  retVal->setHandle(fileHandle);
                  retVal->setRecordType(_FHI_NOT_TYPE_RECORD);
               }
            }
            // End temp fix ......
   }
    }

    // If the fopen failed we have a pointer to the FileHandleImpl class but
    // inside that class a file handle of NULL. In this case we need to delete
    // the class and return a NULL to the caller since they should be checking
    // for a NULL returned as a file open failed indicator.

    if (fileHandle == NULL)
    {
       delete retVal;
       retVal = 0;
    }

    return retVal;

}


static FileHandleImpl* openWrite(char* tmpFileName
                                 , MemoryManager* const manager)
{
    FileHandleImpl* retVal;
    FILE* fileHandle = 0;
    Path390 pathobj;
    pathobj.setPath(tmpFileName);
    fldata_t fileinfo;
    int fldata_rc = 0;
    int optionBufferSize = strlen("wb,");
    int fileLrecl = 0;
    bool isTypeRecord = _FHI_NOT_TYPE_RECORD;

    // Check if we have fopen options specified in addition to a
    // filename. If we do then we need to build a buffer that has
    // "wb," followed by the options. Otherwise we just pass "wb"
    // in as the options.

    if (pathobj.getfopenParms())
       optionBufferSize += (strlen(pathobj.getfopenParms()) + 1);

    char* optionBuffer = (char*) manager->allocate((optionBufferSize) * sizeof(char));//new char[optionBufferSize];
    ArrayJanitor<char> janText((char*)optionBuffer, manager);
    strcpy(optionBuffer,"wb");

    // Build the options buffer

    if (pathobj.getfopenParms())
    {
       strcpy(optionBuffer + 2, ",");
       strcpy(optionBuffer + 3, pathobj.getfopenParms());
    }

    // FileHandleImpl class. The constructor will allocate our staging
    // buffer if it is a "type=record" open.

    fileHandle = fopen(pathobj.getfopenPath(),optionBuffer);

    // If this is a "type=record" open then we need to determine the
    // LRECL of the file. The fldata info will have this in it.
    // We save this in the FileHandleImpl class for use on subsequent
    // writes to the file.

    if (pathobj.isRecordType())
    {
       isTypeRecord = _FHI_TYPE_RECORD;
       fldata_rc = fldata(fileHandle, pathobj.getfopenPath(), &fileinfo);

       if (fldata_rc)
          ThrowXMLwithMemMgr(XMLPlatformUtilsException, XMLExcepts::File_CouldNotWriteToFile, manager);

       fileLrecl = fileinfo.__maxreclen;

       // temp fix for HFS files
       // close the file and re-open it without the fopen options
       // since they don't work too well with HFS files.

       if (fileinfo.__dsorgHFS)
       {
          if (fclose(fileHandle))
            ThrowXMLwithMemMgr(XMLPlatformUtilsException, XMLExcepts::File_CouldNotCloseFile, manager);

          fileHandle=fopen(pathobj.getfopenPath(), "wb");

       }
      // end temp fix

    }

    // If the fopen failed we won't need to create an instance of the
    // FileHandleImpl class .... just return a NULL wich the caller recognizes
    // as a failure. If we have a valid file handle then we'll build create
    // an instance of the class and return that to the caller.

    if (fileHandle == NULL)
       retVal = 0;
    else
       retVal = new (manager) FileHandleImpl(fileHandle, _FHI_WRITE, isTypeRecord,fileLrecl, manager);

    return retVal;
}


// ---------------------------------------------------------------------------
//  XMLPlatformUtils: File Methods
// ---------------------------------------------------------------------------
unsigned int XMLPlatformUtils::curFilePos(FileHandle theFile
                                          , MemoryManager* const manager)
{
    // Get the current position
    FileHandleImpl* fhiPtr = (FileHandleImpl*) theFile;
    FILE* fileHandle = (FILE*)fhiPtr->getHandle();
    int curPos = ftell(fileHandle);

    if (curPos == -1)
       ThrowXMLwithMemMgr(XMLPlatformUtilsException, XMLExcepts::File_CouldNotGetSize, manager);

    return (unsigned int)curPos;
}

void XMLPlatformUtils::closeFile(FileHandle theFile
                                 , MemoryManager* const manager)
{
    FileHandleImpl* fhiPtr = (FileHandleImpl*) theFile;
    FILE* fileHandle = (FILE*)fhiPtr->getHandle();

    // We need to handle a close differently if this was a
    // "type=record" open. In that case we may have data sitting in
    // the staging buffer that is waiting to be written to the file.
    // In this case write that data to the file before we actually
    // close it.

    if ( (fhiPtr->getOpenType() == _FHI_WRITE) &&
         (fhiPtr->isRecordType())  &&
         (fhiPtr->getNextByte()) )
    {
        XMLByte* tmpFlush = fhiPtr->getStgBufferPtr();
        size_t bytesToWrite = fhiPtr->getNextByte();
        emptyBuffer(tmpFlush, bytesToWrite, fhiPtr, manager);
    }

    // Do the regular close stuff ....

    if (fclose(fileHandle))
       ThrowXMLwithMemMgr(XMLPlatformUtilsException, XMLExcepts::File_CouldNotCloseFile, manager);

    // Delete the instance of the FileHandleImpl class (this will free the
    // staging buffer for us)

    delete fhiPtr;

}

unsigned int XMLPlatformUtils::fileSize(FileHandle theFile
                                        , MemoryManager* const manager)
{
    // Get the current position
    FileHandleImpl* fhiPtr = (FileHandleImpl*) theFile;
    FILE* fileHandle = (FILE*)fhiPtr->getHandle();
    long  int curPos = ftell(fileHandle);

    if (curPos == -1)
       ThrowXMLwithMemMgr(XMLPlatformUtilsException, XMLExcepts::File_CouldNotGetCurPos, manager);

    // Seek to the end and save that value for return
    if (fseek(fileHandle, 0, SEEK_END))
       ThrowXMLwithMemMgr(XMLPlatformUtilsException, XMLExcepts::File_CouldNotSeekToEnd, manager);

    long int retVal = ftell(fileHandle);

    if (retVal == -1)
       ThrowXMLwithMemMgr(XMLPlatformUtilsException, XMLExcepts::File_CouldNotSeekToEnd, manager);


    // And put the pointer back
    if (fseek(fileHandle, curPos, SEEK_SET) )
       ThrowXMLwithMemMgr(XMLPlatformUtilsException, XMLExcepts::File_CouldNotSeekToPos, manager);

    return (unsigned int)retVal;
}

FileHandle XMLPlatformUtils::openFile(const XMLCh* const fileName
                                      , MemoryManager* const manager)
{
    char* tmpFileName = XMLString::transcode(fileName, manager);
    ArrayJanitor<char> janText((char*)tmpFileName, manager);

    return openRead(tmpFileName, manager);
}


FileHandle XMLPlatformUtils::openFile(const char* const fileName
                                      , MemoryManager* const manager)
{
    char* tmpFileName = (char*) manager->allocate
    (
        (strlen(fileName) + 1) * sizeof(char)
    );//new char[strlen(fileName) + 1];
    ArrayJanitor<char> janText((char*)tmpFileName, manager);
    strcpy(tmpFileName,fileName);

    return openRead(tmpFileName, manager);
}


FileHandle XMLPlatformUtils::openFileToWrite(const XMLCh* const fileName
                                             , MemoryManager* const manager)
{
    char* tmpFileName = XMLString::transcode(fileName, manager);
    ArrayJanitor<char> janText((char*)tmpFileName, manager);

    return openWrite(tmpFileName, manager);
}


FileHandle XMLPlatformUtils::openFileToWrite(const char* const fileName
                                             , MemoryManager* const manager)
{
    char* tmpFileName = (char*) manager->allocate
    (
        (strlen(fileName) + 1) * sizeof(char)
    );//new char[strlen(fileName) + 1];
    ArrayJanitor<char> janText((char*)tmpFileName, manager);
    strcpy(tmpFileName,fileName);

    return openWrite(tmpFileName, manager);
}


void
XMLPlatformUtils::writeBufferToFile( FileHandle     const  theFile
                                   , long                  toWrite
                                   , const XMLByte* const  toFlush
                                   , MemoryManager* const  manager)
{
    FileHandleImpl* fhiPtr = (FileHandleImpl*) theFile;
    FILE* fileHandle = (FILE*)fhiPtr->getHandle();

    if (!fileHandle     ||
        (toWrite <= 0 ) ||
        !toFlush )
        return;

    XMLByte* tmpFlush = (XMLByte*) toFlush;

    // For writes where the file has been opened with the "type=record"
    // option we must do a lot of extra work. In this case we have
    // a staging buffer that we copy data to and only write it out
    // when we find a newline or line feed in the buffer.

    if (fhiPtr->isRecordType())
    {
       XMLByte* inputBufferPtr =  tmpFlush;
       XMLByte* stageBufferStartPtr =  fhiPtr->getStgBufferPtr();
       int stageBufferNextByte = fhiPtr->getNextByte();
       XMLByte* stageBufferAddPtr =  stageBufferStartPtr + stageBufferNextByte;
       int stageThisAdd = 0;
       int stageTotalAdded = 0;
       int stageToWrite = 0;
       int fileLrecl = fhiPtr->getLrecl();

       // Check each byte passed into us. If it is a new line or
       // line feed we copy up to and including that byte to the
       // staging buffer starting at the next available byte. We
       // then write the valid contents of the staging buffer to
       // the file and then continue going thru the input data
       // since a newline is not the end of the input.
       // Also we want to copy the data to the staging buffer and
       // write it to disk as soon as the staging buffer contains
       // a logical record's worth of data.

       for(int i = 0; i < toWrite; i++)
       {
          if ( (tmpFlush[i] == '\n') ||
               (tmpFlush[i] == 0x25) ||
               ((stageBufferNextByte + (i + 1)) - stageTotalAdded == fileLrecl) )
          {
             stageThisAdd = (i + 1) - stageTotalAdded;
             memcpy((void*) stageBufferAddPtr, (void*) inputBufferPtr, stageThisAdd);
             stageTotalAdded += stageThisAdd;
             inputBufferPtr += stageThisAdd;
             stageBufferAddPtr += stageThisAdd;
             stageToWrite = stageBufferAddPtr - stageBufferStartPtr;
             emptyBuffer(stageBufferStartPtr, stageToWrite, fhiPtr, manager);
             stageBufferAddPtr = stageBufferStartPtr;
             stageBufferNextByte = 0;
          }
       }  // for loop


      // When we finish looping thru the input buffer we may have data
      // left in it that was not copied to the staging buffer yet. Copy
      // whatever is left in the input buffer to the staging buffer now
      // and update the staging buffer next byte offset.

      if (stageTotalAdded < toWrite)
      {
         stageThisAdd = toWrite - stageTotalAdded;
         memcpy((void*) stageBufferAddPtr, (void*) inputBufferPtr, stageThisAdd);
         stageBufferNextByte += stageThisAdd;
      }

      fhiPtr->setNextByte(stageBufferNextByte);
    }

    // Normal type of write, just write data to disk ...

    else
       emptyBuffer(tmpFlush, toWrite, fhiPtr, manager);

  return;
}


unsigned int
XMLPlatformUtils::readFileBuffer(  FileHandle      theFile
                                , const unsigned int    toRead
                                , XMLByte* const  toFill
                                , MemoryManager* const manager)
{
    FileHandleImpl* fhiPtr = (FileHandleImpl*) theFile;
    FILE* fileHandle = (FILE*)fhiPtr->getHandle();
    size_t noOfItemsRead = 0;
    bool isTypeRecord = fhiPtr->isRecordType();

    // On a read, if this was a "type=record" open then we do not want
    // to return any terminating NULLS that fill out a logical
    // record. We also will discard a record that is all NULLS.

    while (true)
    {
       noOfItemsRead = fread( (void*) toFill, 1, toRead, fileHandle);

       if(ferror(fileHandle))
          ThrowXMLwithMemMgr(XMLPlatformUtilsException, XMLExcepts::File_CouldNotReadFromFile, manager);

       if (isTypeRecord)
       {
          if (noOfItemsRead == 0)
              break;

          // Go thru the data we read and strip off trailing NULLS
          // until a non-NULL is encountered. If a record is all
          // NULLS then we discard it.

          bool recordIsValid = false;

          for (int i = noOfItemsRead; i > 0; i--)
          {
             if (toFill[i - 1])
             {
                recordIsValid = true;
                break;
             }
             else
                noOfItemsRead--;
          }

          // If we didn't hit a non-NULL we need will go read another
          // record in hopes of returning some valid data.

          if (recordIsValid)
             break;
          else
          {
             noOfItemsRead = 0;
             break;
          }
       } // isTypeRecord
       // for a "non-type=record" read we do only one read
       else
          break;
    }  // while (true)

    return (unsigned int)noOfItemsRead;
}


void XMLPlatformUtils::resetFile(FileHandle theFile
                                 , MemoryManager* const manager)
{
    // Seek to the start of the file
    FileHandleImpl* fhiPtr = (FileHandleImpl*) theFile;
    FILE* fileHandle = (FILE*)fhiPtr->getHandle();
    if (fseek(fileHandle, 0, SEEK_SET) )
        ThrowXMLwithMemMgr(XMLPlatformUtilsException, XMLExcepts::File_CouldNotResetFile, manager);
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
    //  in Unicode, and goes out as Unicode always
    //
    char* newSrc = XMLString::transcode(srcPath, manager);
    ArrayJanitor<char> janText(newSrc, manager);

    Path390 pathobj;
    pathobj.setPath(newSrc);

    // Check if this is a relative path that we care about ....
    // If it is, then use getCurrentDirectory and append
    // the relative part passed in to those results
    if ( (pathobj.isRelative()) && 
         ( (pathobj.getPathType() == PATH390_HFS) ||
           (pathobj.getPathType() == PATH390_OTHER) ) )
       { 
          XMLCh* curDir = getCurrentDirectory(manager);
          ArrayJanitor<XMLCh> janText1(curDir, manager);

          int curDirLen = XMLString::stringLen(curDir);
          int srcPathLen = XMLString::stringLen(srcPath);

          XMLCh* fullPath = (XMLCh*) manager->allocate
          (
             (curDirLen + srcPathLen + 2) * sizeof(XMLCh)
          );

          // Copy the current directory to the new buffer, then
          // follow it by a slash, then add relative path after that.  
          XMLString::copyString(fullPath, curDir);
          fullPath[curDirLen] = chForwardSlash;
          XMLString::copyString(&fullPath[curDirLen +1], srcPath);

          // get rid of the redundant stuff
          removeDotSlash(fullPath);
          removeDotDotSlash(fullPath);
        
          return fullPath;
        }

    // Just return what they sent in since it wasn't relative
    return XMLString::transcode(newSrc, manager);
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
    //  Check for 0x2F instead of '/' because '/' will be compiled into
    //  0x61
    //
    if (toCheck[0] == 0x2F)
        return false;

    //
    //  Determine whether we are using the DD convention
    //  'D': 0x44 'd': 0x64 ':': 0x3A
    //
    if (XMLString::stringLen(toCheck) > 3) {
      if (
          ((toCheck[0] == (0x44)) || (toCheck[0] == (0x64))) &&
          ((toCheck[1] == (0x44)) || (toCheck[1] == (0x64))) &&
          (toCheck[2] == XMLCh(0x3A))
         )
         return false;
    }

    char* tmpFileName = XMLString::transcode(toCheck, manager);
    ArrayJanitor<char> janText((char*)tmpFileName, manager);
    Path390 pathobj;
    pathobj.setPath(tmpFileName);

    // let Path390 do the final determination
    return pathobj.isRelative();

}

XMLCh* XMLPlatformUtils::getCurrentDirectory(MemoryManager* const manager)
{

    // Use a local buffer that is big enough for the largest legal path
    char *absPath = (char*) manager->allocate((_POSIX_PATH_MAX+1) * sizeof(char));
    ArrayJanitor<char> janText2(absPath, manager);
    if (getcwd(absPath, _POSIX_PATH_MAX) == NULL) 
    {
       ThrowXMLwithMemMgr(XMLPlatformUtilsException, XMLExcepts::File_CouldNotGetBasePathName, manager);
    }
        
    return XMLString::transcode(absPath, manager);

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
//  OS390BATCH: Unless POSIX(ON) is specified, pthread functions are
//  not available. We will use compareAndSwap for now untill a block
//  locking mechanism is developed
// -----------------------------------------------------------------------
#ifndef APP_NO_THREADS
typedef XMLHolder<pthread_mutex_t> PThreadMutexHolderType;
typedef XMLHolder<int>             IntMutexHolderType;

void XMLPlatformUtils::closeMutex(void* const mtxHandle)
{
    if (mtxHandle == NULL)
        return;
    if (isPosixEnabled) {
        PThreadMutexHolderType* const  holder = PThreadMutexHolderType::castTo(mtxHandle);
        if (pthread_mutex_destroy(&holder->fInstance))
        {
            delete holder;
 
            ThrowXMLwithMemMgr(XMLPlatformUtilsException, XMLExcepts::Mutex_CouldNotDestroy, fgMemoryManager);
        }
        delete holder;
    } // __isPosixOn
    else {
        delete IntMutexHolderType::castTo(mtxHandle);
    }
}
void XMLPlatformUtils::lockMutex(void* const mtxHandle)
{
    if (mtxHandle == NULL)
        return;
    if (isPosixEnabled) {
        if (pthread_mutex_lock(&PThreadMutexHolderType::castTo(mtxHandle)->fInstance))
        {
            panic(PanicHandler::Panic_MutexErr);
        }
    } // __isPosixOn
    else {
        int locked = 1, unlocked;

        int*    mutex =
            &IntMutexHolderType::castTo(mtxHandle)->fInstance;

        do {
            unlocked = 0;
            compareAndSwap( (void**) &mutex, (void*) locked, (void*) unlocked );
        } while( unlocked != 0 );
    }
    return;

}

void* XMLPlatformUtils::makeMutex(MemoryManager* manager)
{
    if (isPosixEnabled) {
    PThreadMutexHolderType* const  holder = new (manager) PThreadMutexHolderType;

    pthread_mutexattr_t  attr;
    pthread_mutexattr_init(&attr);
    pthread_mutexattr_setkind_np(&attr, __MUTEX_RECURSIVE);
    if (pthread_mutex_init(&holder->fInstance, &attr))
    {
        delete holder;

        panic(PanicHandler::Panic_MutexErr);
    }
    pthread_mutexattr_destroy(&attr);

    return holder;

    } // __isPosixOn
    else
    {
        IntMutexHolderType* holder = new (manager) IntMutexHolderType;
        holder->fInstance = 0;
        return holder;
    }
}
void XMLPlatformUtils::unlockMutex(void* const mtxHandle)
{
    if (mtxHandle == NULL)
        return;
    if (isPosixEnabled) {
        if (pthread_mutex_unlock(&PThreadMutexHolderType::castTo(mtxHandle)->fInstance))
        {
            panic(PanicHandler::Panic_MutexErr);
        }
    } // __isPosixOn
    else {
        int&    mutex =
            IntMutexHolderType::castTo(mtxHandle)->fInstance;

        if (mutex == 0)
            mutex = 0;
    }
}

#else // #ifndef APP_NO_THREADS

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

#endif // APP_NO_THREADS


// -----------------------------------------------------------------------
//  Miscellaneous synchronization methods
// -----------------------------------------------------------------------
void* XMLPlatformUtils::compareAndSwap ( void**      toFill ,
                    const void* const newValue ,
                    const void* const toCompare)
{
  int boolVar = cs( (cs_t*) &toCompare, (cs_t*) toFill, (cs_t) newValue );
  return (void*) toCompare;
}

int XMLPlatformUtils::atomicIncrement(int &location)
{
  int newVal, oldVal = location;

  do {
    newVal = oldVal + 1;
    oldVal = (int) compareAndSwap( (void**) &location, (void*) newVal, (void*) oldVal );
  } while( newVal != oldVal + 1 );

  return newVal;
}
int XMLPlatformUtils::atomicDecrement(int &location)
{
  int newVal, oldVal = location;

  do {
    newVal = oldVal - 1;
    oldVal = (int) compareAndSwap( (void**) &location, (void*) newVal, (void*) oldVal );
  } while( newVal != oldVal - 1 );

  return newVal;
}


FileHandle XMLPlatformUtils::openStdInHandle(MemoryManager* const manager)
{
     FileHandleImpl* retVal;
     FILE* fileHandle = 0;
     fileHandle = fdopen(dup(0), "rb");
     if (fileHandle)
        retVal = new (manager) FileHandleImpl(fileHandle, _FHI_READ, _FHI_NOT_TYPE_RECORD, 0, manager);
     else
        retVal = 0;

     return retVal;

}

void XMLPlatformUtils::platformTerm()
{
    // We don't have any termination requirements at this time
}

#include <xercesc/util/LogicalPath.c>

XERCES_CPP_NAMESPACE_END
