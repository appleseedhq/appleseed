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
 * $Id: FileHandleImpl.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include <fstream.h>
#include <stdio.h>
#include <ctype.h>
#include <typeinfo>
#define _XOPEN_SOURCE_EXTENDED 1
#include <stdlib.h>
#include <xercesc/util/XMLUniDefs.hpp>
#include "FileHandleImpl.hpp"
#include <xercesc/framework/MemoryManager.hpp>

XERCES_CPP_NAMESPACE_BEGIN

//Constructor:
FileHandleImpl::FileHandleImpl(FILE* open_handle, int o_type, bool r_type, int fileLrecl, MemoryManager* const manager): 
   Handle(open_handle), openType(o_type), recordType(r_type), lrecl(fileLrecl), fMemoryManager(manager) {

   stgBufferPtr = 0;
   nextByte = 0;

   if ((openType == _FHI_WRITE) &&
       (recordType == _FHI_TYPE_RECORD) &&
       (lrecl != 0))
   {
      //stgBufferPtr = new XMLByte [lrecl];
      stgBufferPtr = (XMLByte*) manager->allocate(lrecl * sizeof(XMLByte)); 
   }

// printf("FileHandleImpl constructor called\n");
// printf("stgBufferPtr is: x%8.8X\n", stgBufferPtr);
// printf("Handle is: x%8.8X\n", Handle);
// printf("openType is : %d\n",openType);
// printf("recordType is : %d\n",recordType);
// printf("lrecl is : %d\n",lrecl);

}

//Destructor:

FileHandleImpl::~FileHandleImpl() {

// printf("FileHandleImpl destructor called\n");
// printf("stgBufferPtr is: x%8.8X\n", stgBufferPtr);
// printf("Handle is: x%8.8X\n", Handle);
// printf("openType is : %d\n",openType);
// printf("recordType is : %d\n",recordType);

   if (stgBufferPtr != 0)
   {
// printf("stgBufferPtr is being freed at: x%8.8X\n", stgBufferPtr);
      //delete [] stgBufferPtr;
        fMemoryManager->deallocate(stgBufferPtr);
   }
}

XERCES_CPP_NAMESPACE_END
