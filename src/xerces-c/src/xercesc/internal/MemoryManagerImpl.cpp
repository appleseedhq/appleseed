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
 * $Id: MemoryManagerImpl.cpp 568078 2007-08-21 11:43:25Z amassari $
 */


// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/internal/MemoryManagerImpl.hpp>
#include <xercesc/util/OutOfMemoryException.hpp>

XERCES_CPP_NAMESPACE_BEGIN

void* MemoryManagerImpl::allocate(size_t size)
{
    void* memptr;
    try {
        memptr = ::operator new(size);
    }
    catch(...) {
        throw OutOfMemoryException();
    }
    if (memptr != NULL) {
        return memptr;
    }
    throw OutOfMemoryException();
}

void MemoryManagerImpl::deallocate(void* p)
{
    ::operator delete(p);
}

XERCES_CPP_NAMESPACE_END
