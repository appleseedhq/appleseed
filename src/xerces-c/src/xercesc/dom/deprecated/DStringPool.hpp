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


//
// file DStringPool.hpp
//

//
//  This file is part of the internal implementation of the C++ XML DOM.
//  It should NOT be included or used directly by application programs.
//
//  Applications should include the file <xercesc/dom/deprecated/DOM.hpp> for the entire
//  DOM API, or DOM_*.hpp for individual DOM classes, where the class
//  name is substituded for the *.
//

#ifndef DStringPool_HEADER_GUARD_
#define DStringPool_HEADER_GUARD_

#include "DOMString.hpp"
#include <xercesc/util/XMLRegisterCleanup.hpp>
#include <xercesc/util/PlatformUtils.hpp>

XERCES_CPP_NAMESPACE_BEGIN


struct DStringPoolEntry;

//
// DStringPool is a hash table of DOMStrings.
//  Each DOM Document maintains a DStringPool containing a DOMString
//  for each Element tag name and Attribute Name that has been added
//  to the document.  When creating additional elements or attributes,
//  if the name has been seen before, the already existing string
//  will be reused.
//
class DStringPool : public XMemory
{
public:
    DStringPool(int  hashTableSize,
                MemoryManager* const manager = XMLPlatformUtils::fgMemoryManager);
    ~DStringPool();

    const DOMString &getPooledString(const DOMString &in);
    const DOMString &getPooledString(const XMLCh *in);

    static const DOMString &getStaticString(const char *in
                                          , DOMString **loc
                                          , XMLRegisterCleanup::XMLCleanupFn fn
                                          , XMLRegisterCleanup &clnObj);

private:
    DStringPool(const DStringPool &other);      // Copy constructor and assignment
    DStringPool& operator = (const DStringPool &other); //  of DStringPool are not supported.

    DStringPoolEntry **fHashTable;
    int              fHashTableSize;
    MemoryManager*   fMemoryManager;
};

XERCES_CPP_NAMESPACE_END

#endif
