#ifndef DOMStringImpl_HEADER_GUARD_
#define DOMStringImpl_HEADER_GUARD_

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
 * $Id: DOMStringImpl.hpp 568078 2007-08-21 11:43:25Z amassari $
 */


//
//  This file is part of the internal implementation of the C++ XML DOM.
//  It should NOT be included or used directly by application programs.
//
//  Applications should include the file <xercesc/dom/deprecated/DOM.hpp> for the entire
//  DOM API, or DOM_*.hpp for individual DOM classes, where the class
//  name is substituded for the *.
//


#include <xercesc/util/XercesDefs.hpp>
#include <xercesc/util/Mutexes.hpp>
#include <stdio.h>

XERCES_CPP_NAMESPACE_BEGIN

class   DOMStringData
{
public:
    unsigned int        fBufferLength;
    int                 fRefCount;
    XMLCh               fData[1];

    static DOMStringData *allocateBuffer(unsigned int length);
    inline void         addRef();
    inline void         removeRef();
};

class  DOMStringHandle
{
public:
            unsigned int     fLength;              // The logical length of the DOMString.
                                                   //  This may be shorter than the buffer length.
            int              fRefCount;            // The number of DOMString objects pointing to
                                                   //  this string handle.
            DOMStringData    *fDSData;             // Pointer to the string buffer. May be null.

    void    *operator new( size_t sizeToAlloc);    // StringHandles have custom, optimized
    void    operator delete( void *pvMem );        //   memory allocation.


private:
    static  void             *freeListPtr;         // Head of the linked list of unallocated String Handles

    static  DOMStringHandle  *blockListPtr;        // Head of the linked list of memory blocks from which
                                                   //  string handles are sub-allocated.

public:
    static  DOMStringHandle  *createNewStringHandle(unsigned int bufLength);
            DOMStringHandle  *cloneStringHandle();
    inline  void             addRef();
    inline  void             removeRef();
                             ~DOMStringHandle() {};
    static  void             DOMStringCleanup();
private:
    inline                   DOMStringHandle() {};
    static inline  XMLMutex &getMutex();
};

XERCES_CPP_NAMESPACE_END

#endif

