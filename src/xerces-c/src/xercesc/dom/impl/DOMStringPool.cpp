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
 * $Id: DOMStringPool.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/PlatformUtils.hpp>

#include "DOMStringPool.hpp"
#include "DOMDocumentImpl.hpp"

XERCES_CPP_NAMESPACE_BEGIN


//
//  DStringPoolEntry - one of these structs is allocated for each
//                      XMLCh String in the pool.  Each slot in the
//                      hash table array itself is a pointer to the head
//                      of a singly-linked list of these structs.
//
//                      Although this struct is delcared with a string length of one,
//                      the factory method allocates enough storage to hold the full
//                      string length.
//
struct DOMStringPoolEntry
{
    DOMStringPoolEntry    *fNext;
    XMLCh                 fString[1];
};


//
// createSPE - factory method for creating sting pool entry structs.
//             Allocates sufficient storage to hold the entire string
//
static DOMStringPoolEntry *createSPE(const XMLCh *str, DOMDocumentImpl *doc)
{
    //  Compute size to allocate.  Note that there's 1 char of string declared in the
    //       struct, so we don't need to add one again to account for the trailing null.
    //
    size_t sizeToAllocate = sizeof(DOMStringPoolEntry) + XMLString::stringLen(str)*sizeof(XMLCh);
    DOMStringPoolEntry *newSPE = (DOMStringPoolEntry *)doc->allocate(sizeToAllocate);
    newSPE->fNext = 0;
    XMLCh * nonConstStr = (XMLCh *)newSPE->fString;
    XMLString::copyString(nonConstStr, str);
    return newSPE;
}



DOMStringPool::DOMStringPool(int hashTableSize, DOMDocumentImpl *doc)
:   fDoc(doc)
,   fHashTableSize(hashTableSize)
{
    // needed to get access to the doc's storage allocator.

    //fHashTable = new (fDoc) DOMStringPoolEntry *[hashTableSize];
    void* p = doc->allocate(sizeof(DOMStringPoolEntry*) * hashTableSize);
    fHashTable = (DOMStringPoolEntry**) p;
    for (int i=0; i<fHashTableSize; i++)
        fHashTable[i] = 0;
}


//  Destructor.    Nothing to do, since storage all belongs to the document.
//
DOMStringPool::~DOMStringPool()
{
}


const XMLCh *DOMStringPool::getPooledString(const XMLCh *in)
{
    DOMStringPoolEntry    **pspe;
    DOMStringPoolEntry    *spe;

    int    inHash     = XMLString::hash(in, fHashTableSize, fDoc->getMemoryManager());
    pspe = &fHashTable[inHash];
    while (*pspe != 0)
    {
        if (XMLString::equals((*pspe)->fString, in))
            return (*pspe)->fString;
        pspe = &((*pspe)->fNext);
    }

    // This string hasn't been seen before.  Add it to the pool.
    *pspe = spe = createSPE(in, fDoc);
    return spe->fString;
}


// -----------------------------------------------------------------------
//  DOMBuffer: Constructors
// -----------------------------------------------------------------------
DOMBuffer::DOMBuffer(DOMDocumentImpl *doc, int capacity) :
    fBuffer(0)
    , fIndex(0)
    , fCapacity(capacity)
    , fDoc(doc)
{
    // Buffer is one larger than capacity, to allow for zero term
    fBuffer = (XMLCh*) doc->allocate((fCapacity+1)*sizeof(XMLCh));

    // Keep it null terminated
    fBuffer[0] = XMLCh(0);
}

DOMBuffer::DOMBuffer(DOMDocumentImpl *doc, const XMLCh* string) :
    fBuffer(0)
    , fIndex(0)
    , fCapacity(0)
    , fDoc(doc)
{
    unsigned int actualCount = XMLString::stringLen(string);
    fCapacity = actualCount + 15;

    // Buffer is one larger than capacity, to allow for zero term
    fBuffer = (XMLCh*) doc->allocate((fCapacity+1)*sizeof(XMLCh));

    memcpy(fBuffer, string, actualCount * sizeof(XMLCh));
    fIndex = actualCount;

    // Keep it null terminated
    fBuffer[fIndex] = 0;
}

// ---------------------------------------------------------------------------
//  DOMBuffer: Buffer management
// ---------------------------------------------------------------------------
void DOMBuffer::append(const XMLCh* const chars, const unsigned int count)
{
    unsigned int actualCount = count;
    if (!count)
        actualCount = XMLString::stringLen(chars);
    if (fIndex + actualCount >= fCapacity)
        expandCapacity(actualCount);
    memcpy(&fBuffer[fIndex], chars, actualCount * sizeof(XMLCh));
    fIndex += actualCount;

    // Keep it null terminated
    fBuffer[fIndex] = 0;
}

void DOMBuffer::set(const XMLCh* const chars, const unsigned int count)
{
    unsigned int actualCount = count;
    if (!count)
        actualCount = XMLString::stringLen(chars);
    fIndex = 0;
    if (fIndex + actualCount >= fCapacity)
        expandCapacity(actualCount);
    memcpy(fBuffer, chars, actualCount * sizeof(XMLCh));
    fIndex = actualCount;

    // Keep it null terminated
    fBuffer[fIndex] = 0;
}


// ---------------------------------------------------------------------------
//  DOMBuffer: Private helper methods
// ---------------------------------------------------------------------------
void DOMBuffer::expandCapacity(const unsigned int extraNeeded)
{
    //not enough room. Calc new capacity and allocate new buffer
    const unsigned int newCap = (unsigned int)((fIndex + extraNeeded) * 1.25);
    XMLCh* newBuf = (XMLCh*) fDoc->allocate((newCap+1)*sizeof(XMLCh));

    // Copy over the old stuff
    memcpy(newBuf, fBuffer, fCapacity * sizeof(XMLCh));

    // revisit: Leave the old buffer in document heap, yes, this is a leak, but live with it!
    // store new stuff
    fBuffer = newBuf;
    fCapacity = newCap;
}

XERCES_CPP_NAMESPACE_END

