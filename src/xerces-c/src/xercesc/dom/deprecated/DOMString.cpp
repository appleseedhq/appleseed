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
 * $Id: DOMString.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include <stdio.h>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/RuntimeException.hpp>
#include <xercesc/util/TransService.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/XMLRegisterCleanup.hpp>
#include "DOM_DOMException.hpp"
#include "DOMString.hpp"

#ifndef XML_DEBUG
#include "DOMStringImpl.hpp"
#endif

#include <assert.h>
#include <string.h>

XERCES_CPP_NAMESPACE_BEGIN

//----------------------------------------------
//
//  Forward decls
//
//----------------------------------------------
static void reinitDomConverter();
static void reinitDomMutex();
XMLLCPTranscoder*  getDomConverter();


// ---------------------------------------------------------------------------
//  Local static functions
// ---------------------------------------------------------------------------

//  getDOMConverter - get the converter from the system default
//          codepage to Unicode that is to be used when
//          a DOMString is constructed from a char *.
//
static XMLLCPTranscoder* gDomConverter = 0;
static XMLRegisterCleanup cleanupDomConverter;

int DOMString::gLiveStringDataCount    = 0;
int DOMString::gTotalStringDataCount   = 0;
int DOMString::gLiveStringHandleCount  = 0;
int DOMString::gTotalStringHandleCount = 0;


XMLLCPTranscoder*  getDomConverter()
{
    if (!gDomConverter)
    {
        XMLLCPTranscoder* transcoder = XMLPlatformUtils::fgTransService->makeNewLCPTranscoder();
        if (!transcoder)
            XMLPlatformUtils::panic(PanicHandler::Panic_NoDefTranscoder
            );

        if (XMLPlatformUtils::compareAndSwap((void **)&gDomConverter, transcoder, 0) != 0)
            delete transcoder;
        else
            cleanupDomConverter.registerCleanup(reinitDomConverter);
    }
    return gDomConverter;
}

//
//  There is one global mutex that is used to synchronize access to the
//     allocator free list for DOMStringHandles.  This function gets that
//     mutex, and will create it on the first attempt to get it.
//
static XMLMutex* DOMStringHandleMutex = 0;   // Mutex will be deleted by ~DOMStringHandle.
static XMLRegisterCleanup cleanupDomMutex;

XMLMutex& DOMStringHandle::getMutex()
{
    if (!DOMStringHandleMutex)
    {
        XMLMutex* tmpMutex = new XMLMutex(XMLPlatformUtils::fgMemoryManager);
        if (XMLPlatformUtils::compareAndSwap((void**)&DOMStringHandleMutex, tmpMutex, 0))
        {
            // Someone beat us to it, so let's clean up ours
            delete tmpMutex;
        }
        else
            cleanupDomMutex.registerCleanup(reinitDomMutex);

    }

    return *DOMStringHandleMutex;
}



//----------------------------------------------
//
//      DOMStringData
//
//----------------------------------------------

void DOMStringData::removeRef()
{
    int result = XMLPlatformUtils::atomicDecrement(fRefCount);
    if (result==0)
    {
        fBufferLength = 0xcccc;
        fRefCount     = 0xcccc;
        XMLPlatformUtils::fgMemoryManager->deallocate(this);//delete [] this;  //  was allocated with new char[size] !
        XMLPlatformUtils::atomicDecrement(DOMString::gLiveStringDataCount);
    }
}


void DOMStringData::addRef()
{
    XMLPlatformUtils::atomicIncrement(fRefCount);
}


DOMStringData *DOMStringData::allocateBuffer(unsigned int length)
{
    unsigned int sizeToAllocate = sizeof(DOMStringData) //  buffer will contain an
        + length*sizeof(XMLCh);                //  extra elem because of stub
                                               //  array in DOMStringData struct.
    DOMStringData *buf = 0;
    buf = (DOMStringData *) XMLPlatformUtils::fgMemoryManager->allocate
        (
            sizeToAllocate * sizeof(char)
        );//new char[sizeToAllocate];
    XMLPlatformUtils::atomicIncrement(DOMString::gLiveStringDataCount);
    XMLPlatformUtils::atomicIncrement(DOMString::gTotalStringDataCount);
    buf->fBufferLength = length;
    buf->fRefCount = 1;
    buf->fData[0] = 0;
    return buf;
}




//----------------------------------------------------
//
//      DOMStringHandle
//
//-----------------------------------------------------



//
//  Specialized new and delete operators for DOMStringHandles.
//      These are used, rather than the standard system operator new,
//      for improved performance.
//
//      We allocate largish blocks of memory using the standard system
//      new function, and sub-allocate string handles from that block.
//      Un-allocated string handles within the allocated blocks are kept
//      in a singly linked list, making allocation and deallocation
//      very quick in the common case.
//
//      String handle allocation is thread safe.  A multi-threaded
//      application may have threads concurrently accessing multiple
//      DOM documents; since all string handles come from the same pool,
//      this allocator must be safe.  The compare and exchange function,
//      which is available as a single instruction in most processor
//      architectures, and typically surfaced as an OS function,
//      is used to safely update the string handle free list.
//
void *DOMStringHandle::freeListPtr = 0;   // Point to the head of the
                                          //  free list of un-allocated
                                          //  string handles, or 0 if there
                                          //  are no free string handles.

static const int allocGroupSize = 1024;   // Number of string handles to allocate
                                          //  as a chunk from the system's
                                          //  memory allocator.

DOMStringHandle *DOMStringHandle::blockListPtr = 0;  // Point to the head of the list
                                          //  of larger blocks in which DOMStringHandles
                                          //  are allocated.

//
//  Operator new for DOMStringHandles.  Called implicitly from the
//          DOMStringHandle constructor.
//
void *DOMStringHandle::operator new(size_t sizeToAlloc)
{
    assert(sizeToAlloc == sizeof(DOMStringHandle));
    void    *retPtr;
    XMLMutexLock lock(&getMutex());    // Lock the DOMStringHandle mutex for
                                       //  the duration of this function.

    if (freeListPtr == 0)
    {
        // Uncommon case.  The free list of string handles is empty
        // Allocate a new batch of them, using the system's
        // operator new to get a chunk of memory.
        //
        DOMStringHandle *dsg = (DOMStringHandle*)
            XMLPlatformUtils::fgMemoryManager->allocate
            (
                allocGroupSize * sizeof(DOMStringHandle)
            );//::new DOMStringHandle[allocGroupSize];

        // Link the block itself into the list of blocks.  The purpose of this is to
        //   let us locate and delete the blocks when shutting down.
        //
        *(DOMStringHandle **)dsg = blockListPtr;
        blockListPtr = dsg;


        // Link all of the new storage for StringHandles into the StringHandle free list
        int   i;    //   Start with index 1;  index 0 is reserved for linking the
                    //   larger allocation blocks together.
        for (i=1; i<allocGroupSize-1; i++) {
            *(void **)&dsg[i] = freeListPtr;
            freeListPtr = &dsg[i];
        }
    }

    retPtr = freeListPtr;
    freeListPtr = *(void **)freeListPtr;

    XMLPlatformUtils::atomicIncrement(DOMString::gLiveStringHandleCount);
    return retPtr;
}


//
//  Operator delete for DOMStringHandles.  Called implicitly from the
//              Destructor for DOMStringHandle.
//
void DOMStringHandle::operator delete(void *pMem)
{
    XMLMutexLock   lock(&getMutex());    // Lock the DOMStringHandle mutex for the
    //    duration of this function.
    XMLPlatformUtils::atomicDecrement(DOMString::gLiveStringHandleCount);
    *(void **)pMem = freeListPtr;
    freeListPtr = pMem;

    // If ALL of the string handles are gone, delete the storage blocks used for the
    //   handles as well.
    if (DOMString::gLiveStringHandleCount == 0)
    {
        DOMStringHandle *pThisBlock, *pNextBlock;
        for (pThisBlock = blockListPtr; pThisBlock != 0; pThisBlock = pNextBlock)
        {
            pNextBlock = *(DOMStringHandle **)pThisBlock;
            XMLPlatformUtils::fgMemoryManager->deallocate(pThisBlock);//delete [] pThisBlock;
        }
        blockListPtr = 0;
        freeListPtr  = 0;
    }


}


void DOMStringHandle::addRef()
{
    XMLPlatformUtils::atomicIncrement(fRefCount);
}


void DOMStringHandle::removeRef()
{
    int result = XMLPlatformUtils::atomicDecrement(fRefCount);
    if (result==0)
    {
        fDSData->removeRef();
//        delete this;
        DOMStringHandle* ptr = this;
        delete ptr;
    }
}


DOMStringHandle *DOMStringHandle::createNewStringHandle(unsigned int bufLength)
{
    DOMStringHandle  *h = new DOMStringHandle;
    XMLPlatformUtils::atomicIncrement(DOMString::gTotalStringHandleCount);
    h -> fLength = 0;
    h -> fRefCount = 1;
    h -> fDSData = DOMStringData::allocateBuffer(bufLength);
    return h;
}


DOMStringHandle *DOMStringHandle::cloneStringHandle()
{
    DOMStringHandle *h = new DOMStringHandle;
    h->fLength   = fLength;
    h->fRefCount = 1;
    h->fDSData   = fDSData;
    h->fDSData->addRef();
    return h;
}

//------------------------------------------------------------
//
//      DOMString
//
//------------------------------------------------------------


DOMString::DOMString()
{
    fHandle = 0;
}


DOMString::DOMString(const DOMString &other) :
    XMemory(other)
{
    fHandle = other.fHandle;
    if (fHandle)
        fHandle->addRef();
}


DOMString::DOMString(const XMLCh *data)
{
    fHandle = 0;
    if (data != 0)
    {
        unsigned int dataLength = 0;
        while (data[dataLength] != 0)
            ++dataLength;

        if (dataLength != 0)
        {
            fHandle = DOMStringHandle::createNewStringHandle(dataLength+1);
            fHandle->fLength = dataLength;
            XMLCh *strData = fHandle->fDSData->fData;
            unsigned int i;
            for (i=0; i<dataLength ; ++i)
                strData[i] = data[i];

            strData[dataLength] = 0;
        }
    }
}



DOMString::DOMString(const XMLCh *data, unsigned int dataLength)
{
    fHandle = 0;
    if (data != 0)
    {
        if (dataLength > 0)
        {
            fHandle = DOMStringHandle::createNewStringHandle(dataLength+1);
            fHandle->fLength = dataLength;
            XMLCh *strData = fHandle->fDSData->fData;
            unsigned int i;
            for (i=0; i<dataLength ; ++i)
                strData[i] = data[i];

            strData[dataLength] = 0;
        }
    }
}




//
//  Create a DOMString from a char * string in the default code page
//                     of the system on which we are executing.
//
//
DOMString::DOMString(const char *srcString)
{
    fHandle = 0;
    if (srcString != 0)
    {
        XMLLCPTranscoder*  uniConverter = getDomConverter();

        unsigned int srcLen = strlen(srcString);
        if (srcLen == 0)
            return;

        // The charsNeeded normally is same as srcLen.  To enhance performance,
        // we start with this estimate, and if overflow, then call calcRequiredSize for actual size
        fHandle = DOMStringHandle::createNewStringHandle(srcLen + 1);
        XMLCh *strData = fHandle->fDSData->fData;

        if (!uniConverter->transcode(srcString, strData, srcLen) || (XMLString::stringLen(strData) != srcLen))
        {
            // conversion failed, so try again
            if (fHandle)
                fHandle->removeRef();

            fHandle = 0;

            srcLen = uniConverter->calcRequiredSize(srcString);

            fHandle = DOMStringHandle::createNewStringHandle(srcLen + 1);
            XMLCh *strData2 = fHandle->fDSData->fData;

            if (!uniConverter->transcode(srcString, strData2, srcLen))
            {
                // <TBD> We should throw something here?
            }
        }
        fHandle->fLength = srcLen;
    }
}



DOMString::DOMString(int nullValue)
{
   assert(nullValue == 0);
   fHandle = 0;
}


DOMString::~DOMString()
{
    if (fHandle)
        fHandle->removeRef();

    fHandle = 0;
}


DOMString & DOMString::operator =(const DOMString &other)
{
    if (this == &other)
        return *this;

    if (fHandle)
        fHandle->removeRef();

    fHandle = other.fHandle;

    if (fHandle)
        fHandle->addRef();

    return *this;
}


DOMString & DOMString::operator = (DOM_NullPtr *arg)
{
    assert(arg == 0);
    if (fHandle)
        fHandle->removeRef();

    fHandle = 0;
    return *this;
}



bool DOMString::operator ==(const DOMString &other) const
{
    return this->fHandle == other.fHandle;
}


bool DOMString::operator !=(const DOMString &other) const
{
    return this->fHandle != other.fHandle;
}


bool DOMString::operator == (const DOM_NullPtr * /*p*/) const
{
    return (fHandle == 0);
}

bool DOMString::operator != (const DOM_NullPtr * /*p*/) const
{
    return (fHandle != 0);
}



void DOMString::reserve(unsigned int size)
{
	if (fHandle == 0)
	{
	    if (size > 0)
	        fHandle = DOMStringHandle::createNewStringHandle(size);
	}
}



void DOMString::appendData(const DOMString &other)
{
    if (other.fHandle == 0 || other.fHandle->fLength == 0)
        return;

    // If this string is empty and this string does not have an
    //   already allocated buffer sufficient to hold the string being
    //   appended, return a clone of the other string.
    //
    if (fHandle == 0 || (fHandle->fLength == 0 &&
        fHandle->fDSData->fBufferLength < other.fHandle->fLength))
    {
        if (fHandle) fHandle->removeRef();
        this->fHandle = other.fHandle->cloneStringHandle();
        return;
    }

    unsigned int newLength = fHandle->fLength + other.fHandle->fLength;
    if (newLength >= fHandle->fDSData->fBufferLength ||
        fHandle->fDSData->fRefCount > 1)
    {
        // We can't stick the data to be added onto the end of the
        //  existing string, either because there is not space in
        //  the buffer, or because the buffer is being shared with
        //  some other string.  So, make a new buffer.
        DOMStringData *newBuf = DOMStringData::allocateBuffer(newLength+1);
        XMLCh *newP = newBuf->fData;
        XMLCh *oldP = fHandle->fDSData->fData;
        unsigned int i;
        for (i=0; i<fHandle->fLength; ++i)
            newP[i] = oldP[i];

        fHandle->fDSData->removeRef();
        fHandle->fDSData = newBuf;
    }

    //
    // This string now had enough buffer room to hold the data to
    //  be appended.  Go ahead and copy it in.
    XMLCh *srcP = other.fHandle->fDSData->fData;
    XMLCh *destP = &fHandle->fDSData->fData[fHandle->fLength];
    unsigned int i;
    for (i=0; i<other.fHandle->fLength; i++)
        destP[i] = srcP[i];

    fHandle->fLength += other.fHandle->fLength;
}



void DOMString::appendData(XMLCh ch)
{
	unsigned int newLength = 0;

	if (fHandle == 0)
	{
		fHandle = DOMStringHandle::createNewStringHandle(2);
		newLength = 1;
	}
	else
		newLength = fHandle->fLength + 1;

    if (newLength >= fHandle->fDSData->fBufferLength ||
        fHandle->fDSData->fRefCount > 1)
    {
        // We can't stick the data to be added onto the end of the
        //  existing string, either because there is not space in
        //  the buffer, or because the buffer is being shared with
        //  some other string.  So, make a new buffer.
        DOMStringData *newBuf = DOMStringData::allocateBuffer(newLength+1);
        XMLCh *newP = newBuf->fData;
        XMLCh *oldP = fHandle->fDSData->fData;
        unsigned int i;
        for (i=0; i<fHandle->fLength; ++i)
            newP[i] = oldP[i];

        fHandle->fDSData->removeRef();
        fHandle->fDSData = newBuf;
    }

    XMLCh *destP = &fHandle->fDSData->fData[fHandle->fLength];
	destP[0] = ch;

    fHandle->fLength ++;
}

// TODO: A custom version could be written more efficiently, avoiding
// the creation of the temporary DOMString
void DOMString::appendData(const XMLCh* other)
{
	appendData(DOMString(other));
}


DOMString& DOMString::operator +=(const DOMString &other)
{
	appendData(other);

	return *this;
}

DOMString& DOMString::operator +=(const XMLCh *str)
{
	appendData(str);

	return *this;
}

DOMString& DOMString::operator +=(XMLCh ch)
{
	appendData(ch);

	return *this;
}



XMLCh     DOMString::charAt(unsigned int index) const
{
    XMLCh retCh = 0;
    if ((fHandle != 0) && (index < fHandle->fLength))
        retCh = fHandle->fDSData->fData[index];
    return retCh;
}


DOMString DOMString::clone() const
{
    DOMString retString;

    if (fHandle != 0)
        retString.fHandle = this->fHandle->cloneStringHandle();

    return retString;
}



void DOMString::deleteData(unsigned int offset, unsigned int delLength)
{
    unsigned int stringLen = this->length();
    if (offset > stringLen)
        throw DOM_DOMException(DOM_DOMException::INDEX_SIZE_ERR, 0);

    // Cap the value of delLength to avoid trouble with overflows
    //  in the following length computations.
    if (delLength > stringLen)
        delLength = stringLen;

    // If the length of data to be deleted would extend off the end
    //   of the string, cut it back to stop at the end of string.
    if (offset + delLength >= stringLen)
        delLength = stringLen - offset;

    if (delLength == 0)
        return;


    unsigned int newStringLength = stringLen - delLength;
    if (fHandle->fDSData->fRefCount > 1 && offset+delLength < stringLen)
    {
        // The deletion is of a range in the middle of the string
        //  and there's another string handle using the buffer so
        //  we need to make a new buffer before moving characters
        //  around.
        DOMStringData *newBuf = DOMStringData::allocateBuffer(newStringLength+1);
        XMLCh *newP = newBuf->fData;
        XMLCh *oldP = fHandle->fDSData->fData;
        unsigned int i;
        for (i=0; i<offset; i++)
            newP[i] = oldP[i];

        for (i=offset; i<newStringLength; i++)
            newP[i] = oldP[i+delLength];

        fHandle->fLength = newStringLength;
        fHandle->fDSData->removeRef();
        fHandle->fDSData = newBuf;
    }
    else if (offset+delLength < stringLen)
    {
        // The deletion is of a range in the middle of the string,
        // but no other string is sharing the buffer, so we can
        // just delete in place.
        unsigned int i;
        XMLCh *bufP =  fHandle->fDSData->fData;
        for (i=offset; i<newStringLength; i++)
            bufP[i] = bufP[i+delLength];

        fHandle->fLength = newStringLength;
    }
    else
    {
        // The deletion continues to the end of the string.
        // Simply reset the length.  We don't need to worry
        // about other strings sharing the buffer because
        // no characters are moved.
        fHandle->fLength = newStringLength;
    }
}



bool DOMString::equals(const DOMString &other) const
{
    bool retVal = true;
    if (this->fHandle != 0  && other.fHandle != 0)
    {
        if (this->fHandle->fLength != other.fHandle->fLength)
        {
            retVal =  false;
        }
        else
        {
            XMLCh *thisP  = this->fHandle->fDSData->fData;
            XMLCh *otherP = other.fHandle->fDSData->fData;
            unsigned int i;
            for (i=0; i<this->fHandle->fLength; i++)
            {
                if (thisP[i] != otherP[i])
                {
                    retVal = false;
                    break;
                }
            }
        }
    }
    else
    {
        // At this point, one or more of the fHandle
        //  pointers is known to be zero.
        if (fHandle       && fHandle->fLength != 0  ||
            other.fHandle && other.fHandle->fLength != 0)
            retVal = false;

    }
    return retVal;
}



bool DOMString::equals(const XMLCh *other) const
{
    if (this->fHandle != 0  && other != 0)
    {
        // Both strings have non-null data pointers, so
        //  we can go ahead and actually compare them.
        XMLCh *thisP  = this->fHandle->fDSData->fData;
        unsigned int len    = this->fHandle->fLength;

        unsigned int i;
        for (i=0; i<len; i++)
        {
            if (other[i] == 0)   // "other" is null terminated.
                return false;    //   (If there were no chance of a DOM
                                 //   string having a 0 char in the middle of
                                 //   it, this test could be omitted.)

            if (thisP[i] != other[i])
                return false;
        }

        if (other[len] != 0)     // This test for the end of the other
            return false;        //  string can't be done without first
                                 //  checking that we haven't walked off the
                                 //  end.  (It has actually happened - off end
                                 //  of string, page, and valid memory.)

        return true;
    }


    // At this point, we know that at least one of the strings had a null
    //  data pointer.
    if (fHandle  && fHandle->fLength != 0)
        return false;

    if (other && *other != 0)
        return false;

    return true;  // Both strings are empty.  DOMString treats zero-length
                  //   and a null data pointer as equivalent.
}


void DOMString::insertData(unsigned int offset, const DOMString &src)
{
    unsigned int origStrLength = this->length();
    if (offset > origStrLength)
        throw DOM_DOMException(DOM_DOMException::INDEX_SIZE_ERR, 0);

    if (fHandle == 0)
    {
        *this = src.clone();
        return;
    }

    if (src.fHandle == 0 || src.fHandle->fLength == 0)
        return;

    XMLCh *srcP = src.fHandle->fDSData->fData;
    unsigned int srcLength = src.fHandle->fLength;
    unsigned int newLength = fHandle->fLength + srcLength;
    if (newLength >= fHandle->fDSData->fBufferLength ||
        fHandle->fDSData->fRefCount > 1  || fHandle == src.fHandle )
    {
        // We can't stick the data to be added into the
        //  existing string, either because there is not space in
        //  the buffer, or because the buffer is being shared with
        //  some other string.
        //  So, make a new buffer.

        DOMStringData *newBuf = DOMStringData::allocateBuffer(newLength+1);
        XMLCh *newP  = newBuf->fData;
        XMLCh *oldP   = fHandle->fDSData->fData;
        unsigned int i;
        for (i=0; i<offset; ++i)
            newP[i] = oldP[i];

        for (i=0; i<srcLength; i++)
            newP[i+offset] = srcP[i];

        for (i=offset; i<origStrLength; i++)
            newP[i+srcLength] = oldP[i];

        fHandle->fDSData->removeRef();
        fHandle->fDSData = newBuf;
    }
    else
    {
        // There is room in the already-existing buffer to hold
        //  the data to be inserted.  Insert it.
        //
        XMLCh *destP = fHandle->fDSData->fData;
        int i;
        for (i=(int)origStrLength-1; i>=(int)offset; i--)
            destP[i+srcLength] = destP[i];

        unsigned int j;
        for (j=0; j<srcLength; j++)
            destP[j+offset] = srcP[j];
    }

    fHandle->fLength += srcLength;
}



unsigned int DOMString::length() const
{
    unsigned int len = 0;
    if (fHandle != 0)
        len = fHandle->fLength;

    return len;
}



void DOMString::print() const
{
    unsigned int len = this->length();

    if (len > 0)
    {
        // Transcode from Unicode to char * in whatever the system local code page is.
        char *pc = transcode(XMLPlatformUtils::fgMemoryManager);
        fputs(pc, stdout);

        XMLPlatformUtils::fgMemoryManager->deallocate(pc);//delete [] pc;
    }
}


void DOMString::println() const
{
	print();
    putchar('\n');
}



const XMLCh *DOMString::rawBuffer() const
{
    XMLCh  *retP = 0;
    if (fHandle)
    {
        retP = fHandle->fDSData->fData;
        retP[fHandle->fLength]=0;
    }
    return retP;
}


char *DOMString::transcode() const
{
    if (!fHandle || fHandle->fLength == 0)
    {
        char* retP = new char[1];
        *retP = 0;
        return retP;
    }

    // We've got some data
    const XMLCh* srcP = rawBuffer();

    //
    //  Find out how many output chars we need and allocate a buffer big enough
    //  for that plus a null.
    //
    //  The charsNeeded normally is same as fHandle->fLength.  To enhance performance,
    //  we start with this estimate, and if overflow, then call calcRequiredSize for actual size
    unsigned int charsNeeded = fHandle->fLength;
    char* retP = new char[charsNeeded + 1];

    if (!getDomConverter()->transcode(srcP, retP, charsNeeded) || (XMLString::stringLen(retP) != charsNeeded))
    {
        delete [] retP;
        charsNeeded = getDomConverter()->calcRequiredSize(srcP);
        retP = new char[charsNeeded + 1];
        if (!getDomConverter()->transcode(srcP, retP, charsNeeded))
        {
            // <TBD> We should throw something here?
        }
    }

    // Cap it off and return it
    retP[charsNeeded] = 0;
    return retP;
}

char *DOMString::transcode(MemoryManager* const manager) const
{
    if (!fHandle || fHandle->fLength == 0)
    {
        char* retP = (char*) manager->allocate(sizeof(char));//new char[1];
        *retP = 0;
        return retP;
    }

    // We've got some data
    const XMLCh* srcP = rawBuffer();

    //
    //  Find out how many output chars we need and allocate a buffer big enough
    //  for that plus a null.
    //
    //  The charsNeeded normally is same as fHandle->fLength.  To enhance performance,
    //  we start with this estimate, and if overflow, then call calcRequiredSize for actual size
    unsigned int charsNeeded = fHandle->fLength;
    char* retP = (char*) manager->allocate((charsNeeded + 1) * sizeof(char));//new char[charsNeeded + 1];

    if (!getDomConverter()->transcode(srcP, retP, charsNeeded) || (XMLString::stringLen(retP) != charsNeeded))
    {
        manager->deallocate(retP);//delete [] retP;
        charsNeeded = getDomConverter()->calcRequiredSize(srcP);
        retP = (char*) manager->allocate((charsNeeded + 1) * sizeof(char));//new char[charsNeeded + 1];
        if (!getDomConverter()->transcode(srcP, retP, charsNeeded))
        {
            // <TBD> We should throw something here?
        }
    }

    // Cap it off and return it
    retP[charsNeeded] = 0;
    return retP;
}


DOMString DOMString::transcode(const char* str)
{
    return DOMString(str);
}

bool DOMString::operator < (const DOMString &other) const
{
    return (compareString(other) < 0);
}

int DOMString::compareString(const DOMString &other) const
{
    // Note: this strcmp does not match the semantics
    //       of the standard C strcmp.  All it needs to do is
    //       define some less than - equals - greater than ordering
    //       of strings.  How doesn't matter.
    //
    unsigned int thisLen = length();
    unsigned int otherLen = other.length();

    if (thisLen < otherLen)
        return -1;

    if (thisLen > otherLen)
        return 1;

    if (thisLen == 0)
        return 0;

    XMLCh *thisP =  this->fHandle->fDSData->fData;
    XMLCh *otherP = other.fHandle->fDSData->fData;
    unsigned int i;
    for (i=0; i<thisLen; i++)
    {
        if (thisP[i] < otherP[i])
            return -1;
        else if (thisP[i] > otherP[i])
            return 1;
    }

    return 0;
}


DOMString DOMString::substringData(unsigned int offset, unsigned int count) const
{
    unsigned int thisLen = length();
    if (offset > thisLen)
        throw DOM_DOMException(DOM_DOMException::INDEX_SIZE_ERR, 0);

    // Cap count to the string length to eliminate overflow
    //  problems when we get passed bogus values, like -1.
    if (count > thisLen)
        count = thisLen;

    // If the count extends past the end of the string, cut it
    //   back so that the returned string will stop at the end
    //   of the source string.
    if (offset + count >= thisLen)
        count = thisLen - offset;

    if (count == 0)
        return DOMString();

    // If the substring starts at the beginning of the original string
    //   we do not need to copy the data, but can set up a new
    //   string handle with the shorter length.
    if (offset == 0)
    {
        DOMString retString = this->clone();
        retString.fHandle->fLength = count;
        return retString;
    }

    // The substring starts somewhere in the interior of the orignal string.
    // Create a completely new DOMString.  No buffer sharing is possible.
    XMLCh *data = fHandle->fDSData->fData;
    return DOMString(data+offset, count);

}


DOMString operator + (const DOMString &lhs, const DOMString &rhs)
{
    DOMString retString = lhs.clone();
    retString.appendData(rhs);
    return retString;
}

DOMString operator + (const DOMString &lhs, const XMLCh* rhs)
{
    DOMString retString = lhs.clone();
    retString.appendData(rhs);
    return retString;
}

DOMString operator + (const XMLCh* lhs, const DOMString& rhs)
{
    DOMString retString = DOMString(lhs);
    retString.appendData(rhs);
    return retString;
}


DOMString operator + (const DOMString &lhs, XMLCh rhs)
{
    DOMString retString = lhs.clone();
    retString.appendData(rhs);
    return retString;
}

DOMString operator + (XMLCh lhs, const DOMString& rhs)

{
    DOMString retString;
	retString.appendData(lhs);
    retString.appendData(rhs);
    return retString;
}


// -----------------------------------------------------------------------
//  Notification that lazy data has been deleted
// -----------------------------------------------------------------------
static void reinitDomConverter()
{
        delete gDomConverter;           //  Delete the local code page converter.
        gDomConverter = 0;
}


static void reinitDomMutex()
{
        delete DOMStringHandleMutex;    //  Delete the synchronization mutex,
        DOMStringHandleMutex = 0;
}


XERCES_CPP_NAMESPACE_END

