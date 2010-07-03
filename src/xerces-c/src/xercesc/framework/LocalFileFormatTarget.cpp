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
 * $Id: LocalFileFormatTarget.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include <xercesc/framework/LocalFileFormatTarget.hpp>
#include <xercesc/framework/MemoryManager.hpp>
#include <xercesc/util/IOException.hpp>
#include <xercesc/util/OutOfMemoryException.hpp>
#include <assert.h>
#include <string.h>

XERCES_CPP_NAMESPACE_BEGIN

LocalFileFormatTarget::LocalFileFormatTarget( const XMLCh* const   fileName
                                            , MemoryManager* const manager)
: fSource(0)
, fDataBuf(0)
, fIndex(0)
, fCapacity(1023)
, fMemoryManager(manager)
{
    fSource = XMLPlatformUtils::openFileToWrite(fileName, manager);

    if (fSource == (FileHandle) XERCES_Invalid_File_Handle)
        ThrowXMLwithMemMgr1(IOException, XMLExcepts::File_CouldNotOpenFile, fileName, fMemoryManager);

    // Buffer is one larger than capacity, to allow for zero term
    fDataBuf = (XMLByte*) fMemoryManager->allocate
    (
        (fCapacity+4) * sizeof(XMLByte)
    );//new XMLByte[fCapacity+4];

    // Keep it null terminated
    fDataBuf[0] = XMLByte(0);

}

LocalFileFormatTarget::LocalFileFormatTarget( const char* const    fileName
                                            , MemoryManager* const manager)
: fSource(0)
, fDataBuf(0)
, fIndex(0)
, fCapacity(1023)
, fMemoryManager(manager)
{
    fSource = XMLPlatformUtils::openFileToWrite(fileName, manager);

    if (fSource == (FileHandle) XERCES_Invalid_File_Handle)
        ThrowXMLwithMemMgr1(IOException, XMLExcepts::File_CouldNotOpenFile, fileName, fMemoryManager);

    // Buffer is one larger than capacity, to allow for zero term
    fDataBuf = (XMLByte*) fMemoryManager->allocate
    (
        (fCapacity+4) * sizeof(XMLByte)
    );//new XMLByte[fCapacity+4];

    // Keep it null terminated
    fDataBuf[0] = XMLByte(0);
}

LocalFileFormatTarget::~LocalFileFormatTarget()
{
    // flush remaining buffer before destroy
    flushBuffer();

    if (fSource)
        XMLPlatformUtils::closeFile(fSource, fMemoryManager);

    fMemoryManager->deallocate(fDataBuf);//delete [] fDataBuf;
}

void LocalFileFormatTarget::flush()
{
    flushBuffer();
}

void LocalFileFormatTarget::writeChars(const XMLByte* const toWrite
                                     , const unsigned int   count
                                     , XMLFormatter * const        )
{
    if (count) {
        if (insureCapacity(count))
        {
            memcpy(&fDataBuf[fIndex], toWrite, count * sizeof(XMLByte));
            fIndex += count;
        }
        else
        {
            //flush whatever we have in the buffer and the incoming byte stream
            flushBuffer();
            XMLPlatformUtils::writeBufferToFile(fSource, (long) count, toWrite, fMemoryManager);
        }
    }

    return;
}

void LocalFileFormatTarget::flushBuffer()
{
    // Exception thrown in writeBufferToFile, if any, will be propagated to
    // the XMLFormatter and then to the DOMWriter, which may notify
    // application through DOMErrorHandler, if any.
    XMLPlatformUtils::writeBufferToFile(fSource, (long) fIndex, fDataBuf, fMemoryManager);
    fIndex = 0;
    fDataBuf[0] = 0;
    fDataBuf[fIndex + 1] = 0;
    fDataBuf[fIndex + 2] = 0;
    fDataBuf[fIndex + 3] = 0;
}

/***
 *
 *   if the current capacity is not enough, and we can not have
 *   enough memory for the new buffer, we got to notify the caller 
 *
 ***/
bool LocalFileFormatTarget::insureCapacity(const unsigned int extraNeeded)
{
    // If we can handle it, do nothing yet
    if (fIndex + extraNeeded < fCapacity)
        return true;

    // Oops, not enough room. Calc new capacity and allocate new buffer
    const unsigned int newCap = (unsigned int)((fIndex + extraNeeded) * 2);
    XMLByte* newBuf = 0;

    try
    {
        newBuf = (XMLByte*) fMemoryManager->allocate
        (
            (newCap+4) * sizeof(XMLByte)
        );//new XMLByte[newCap+4];
    }
    catch(const OutOfMemoryException&)
    {
        return false;
    }

    assert(newBuf);

    // Copy over the old stuff
    memcpy(newBuf, fDataBuf, fCapacity * sizeof(XMLByte) + 4);

    // Clean up old buffer and store new stuff
    fMemoryManager->deallocate(fDataBuf); //delete [] fDataBuf;
    fDataBuf = newBuf;
    fCapacity = newCap;

    // flush the buffer too
    flushBuffer();
    return true;
}

XERCES_CPP_NAMESPACE_END


