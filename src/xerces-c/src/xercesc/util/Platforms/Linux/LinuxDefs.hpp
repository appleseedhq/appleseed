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
 * $Id: LinuxDefs.hpp 568078 2007-08-21 11:43:25Z amassari $
 */


// ---------------------------------------------------------------------------
//  Detect endian mode
// ---------------------------------------------------------------------------
#include <endian.h>
#ifdef __BYTE_ORDER
    #if __BYTE_ORDER == __LITTLE_ENDIAN
        #define ENDIANMODE_LITTLE
    #else
        #if __BYTE_ORDER == __BIG_ENDIAN
            #define ENDIANMODE_BIG
        #else
            #error: unknown byte order!
        #endif
    #endif
#endif /* __BYTE_ORDER */

typedef void* FileHandle;

#ifndef LINUX
#define LINUX
#endif
