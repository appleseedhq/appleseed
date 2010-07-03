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
 * $Id: DStringPool.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

//
// file DStringPool.cpp
//

#include "DStringPool.hpp"
#include <xercesc/util/XMLRegisterCleanup.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/PlatformUtils.hpp>

XERCES_CPP_NAMESPACE_BEGIN


//
//  DStringPoolEntry - one of these structs is allocated for each
//                      DOMString in the pool.  Each slot in the
//                      hash table array itself is a pointer to the head
//                      of a singly-linked list of these structs.
//
struct DStringPoolEntry : public XMemory
{
    DStringPoolEntry    *fNext;
    DOMString           fString;
};



DStringPool::DStringPool(int hashTableSize,
                         MemoryManager* const manager)
{
    fHashTableSize = hashTableSize;
    fHashTable = (DStringPoolEntry**) manager->allocate
    (
        hashTableSize * sizeof(DStringPoolEntry*)
    );//new DStringPoolEntry *[hashTableSize];
    fMemoryManager = manager;
    for (int i=0; i<fHashTableSize; i++)
        fHashTable[i] = 0;
};


//  Destructor.   Iterate through the pool, deleting each of the
//                DSTringPoolEntry structs, then delete the hash
//                array itself.
//
DStringPool::~DStringPool()
{
    for (int slot=0; slot<fHashTableSize; slot++)
    {
        DStringPoolEntry    *spe;
        DStringPoolEntry    *nextSPE;
        for (spe=fHashTable[slot]; spe != 0; spe = nextSPE )
        {
            // spe->string = 0;
            nextSPE = spe->fNext;
            delete spe;    // Note that this will invoke the destructor
                           //   on spe->fString.
        }
    }
    fMemoryManager->deallocate(fHashTable);//delete [] fHashTable;
    fHashTable = 0;
};


const DOMString &DStringPool::getPooledString(const XMLCh *in)
{
    DStringPoolEntry    **pspe;
    DStringPoolEntry    *spe;

    int    inHash     = XMLString::hash(in, fHashTableSize, fMemoryManager);
    pspe = &fHashTable[inHash];
    while (*pspe != 0)
    {
        if ((*pspe)->fString.equals(in))
            return (*pspe)->fString;
        pspe = &((*pspe)->fNext);
    }
    *pspe = spe = new (fMemoryManager) DStringPoolEntry;
    spe->fNext = 0;
    spe->fString = DOMString(in);
    return spe->fString;
};


const DOMString &DStringPool::getPooledString(const DOMString &in)
{
    DStringPoolEntry    **pspe;
    DStringPoolEntry    *spe;

    const XMLCh *inCharData = in.rawBuffer();
    int          inLength   = in.length();
    int          inHash     = XMLString::hashN(inCharData, inLength, fHashTableSize, fMemoryManager);

    pspe = &fHashTable[inHash];
    while (*pspe != 0)
    {
        if ((*pspe)->fString.equals(in))
            return (*pspe)->fString;
        pspe = &((*pspe)->fNext);
    }
    *pspe = spe = new (fMemoryManager) DStringPoolEntry;
    spe->fNext = 0;
    spe->fString = DOMString(in);
    return spe->fString;
};



//
//  getLiteralString
//
//     This is a static function that is somewhat separate from the rest
//      of the string pool.  It is used to manage the one-time creation of
//      static strings that are reused freqently within the DOM implementation.
//      This is primarily things like the default names for the various
//      node types ("#text" and the like).
//
const DOMString &DStringPool::getStaticString(const char *in
                                            , DOMString **loc
                                            , XMLRegisterCleanup::XMLCleanupFn fn
                                            , XMLRegisterCleanup &clnObj)
{
    if (*loc == 0)
    {
        DOMString *t = new DOMString(in);   // This is one of the very few
                                            //   places that a DOMString variable
                                            //   is heap allocated.  Normal usage
                                            //   is to create local instances and
                                            //   pass them around by value.
        if (XMLPlatformUtils::compareAndSwap((void **)loc, t, 0) != 0)
            delete t;
        else
        {
            // Register this string for deletion.  Doing each string individually
            //   may be a little heavyweight, but will work for the time being
            //   for arranging the deletion of eveything on Termination of XML.
            clnObj.registerCleanup(fn);
        }
    }
    return **loc;
}

XERCES_CPP_NAMESPACE_END

