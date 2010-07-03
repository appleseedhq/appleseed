#ifndef DOMStringPool_HEADER_GUARD_
#define DOMStringPool_HEADER_GUARD_

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
 * $Id: DOMStringPool.hpp 568078 2007-08-21 11:43:25Z amassari $
 */

//
//  This file is part of the internal implementation of the C++ XML DOM.
//  It should NOT be included or used directly by application programs.
//
//  Applications should include the file <xercesc/dom/DOM.hpp> for the entire
//  DOM API, or xercesc/dom/DOM*.hpp for individual DOM classes, where the class
//  name is substituded for the *.
//

#include <xercesc/util/XercesDefs.hpp>

XERCES_CPP_NAMESPACE_BEGIN


struct  DOMStringPoolEntry;
class   DOMDocumentImpl;

//
// DOMStringPool is a hash table of XMLCh* Strings.
//  Each DOM Document maintains a DOMStringPool containing a XMLCh* String
//  for each Element tag name and Attribute Name that has been added
//  to the document.  When creating additional elements or attributes,
//  if the name has been seen before, the already existing string
//  will be reused.
//
class DOMStringPool
{
public:
    DOMStringPool(int  hashTableSize, DOMDocumentImpl *doc);
    ~DOMStringPool();

    const XMLCh *getPooledString(const XMLCh *in);


private:
    DOMStringPool(const DOMStringPool &other);      // Copy constructor and assignment
    DOMStringPool& operator = (const DOMStringPool &other); //  of DOMStringPool are not supported.


    DOMDocumentImpl     *fDoc;
    DOMStringPoolEntry **fHashTable;
    int                 fHashTableSize;

};


//
// DOMBuffer is a lightweight text buffer
// The buffer is not nul terminated until some asks to see the raw buffer
// contents. This also avoids overhead during append operations.
class DOMBuffer
{
public :
    // -----------------------------------------------------------------------
    //  Constructors and Destructor
    // -----------------------------------------------------------------------
    DOMBuffer(DOMDocumentImpl *doc, int capacity = 31);

    DOMBuffer(DOMDocumentImpl *doc, const XMLCh* string);

    ~DOMBuffer()
    {
    }

    // -----------------------------------------------------------------------
    //  Buffer Management
    // -----------------------------------------------------------------------
    void append
    (
        const   XMLCh* const    chars
        , const unsigned int    count = 0
    );

    const XMLCh* getRawBuffer() const
    {
        fBuffer[fIndex] = 0;
        return fBuffer;
    }

    void reset()
    {
        fIndex = 0;
        fBuffer[0] = 0;
    }

    void set
    (
        const   XMLCh* const    chars
        , const unsigned int    count = 0
    );

    void chop
    (
        const unsigned int    count
    )
    {
        fBuffer[count] = 0;
        fIndex = count;
    }


    // -----------------------------------------------------------------------
    //  Getters
    // -----------------------------------------------------------------------
    unsigned int getLen() const
    {
        return fIndex;
    }

    // -----------------------------------------------------------------------
    //  Private helpers
    // -----------------------------------------------------------------------
    void expandCapacity(const unsigned int extraNeeded);


private :
    // -----------------------------------------------------------------------
    //  Private data members
    //
    //  fBuffer
    //      The pointer to the buffer data. Its grown as needed. Its always
    //      one larger than fCapacity, to leave room for the null terminator.
    //
    //  fIndex
    //      The current index into the buffer, as characters are appended
    //      to it. If its zero, then the buffer is empty.
    //
    //  fCapacity
    //      The current capacity of the buffer. Its actually always one
    //      larger, to leave room for the null terminator.
    //
    //  fDoc
    //      For allocating memory
    // -----------------------------------------------------------------------
    XMLCh*          fBuffer;
    unsigned int    fIndex;
    unsigned int    fCapacity;
    DOMDocumentImpl* fDoc;

    // -----------------------------------------------------------------------
    // Unimplemented constructors and operators
    // -----------------------------------------------------------------------
    DOMBuffer(const DOMBuffer &);
    DOMBuffer & operator = (const DOMBuffer &);
};

XERCES_CPP_NAMESPACE_END

#endif
