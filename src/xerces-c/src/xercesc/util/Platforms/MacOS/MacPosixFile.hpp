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
 * $Id: MacPosixFile.hpp 568078 2007-08-21 11:43:25Z amassari $
 */

#pragma once

#include <xercesc/util/XercesDefs.hpp>
#include <xercesc/util/Platforms/MacOS/MacOSPlatformUtils.hpp>

#include <stdio.h>

XERCES_CPP_NAMESPACE_BEGIN

//	Concrete file class implemented using raw Carbon file system calls.
class XMLMacPosixFile : public XMLMacAbstractFile
{
    public:
        XMLMacPosixFile();
        virtual ~XMLMacPosixFile();

        unsigned int currPos();
        void close();
        unsigned int size();
        bool open(const XMLCh* path, bool toWrite = false);
        bool open(const char* path, bool toWrite = false);
        unsigned int read(unsigned int byteCount, XMLByte* buffer);
        void write(long byteCount, const XMLByte* buffer);
        void reset();

    protected:
        FILE*	mFile;
};

XERCES_CPP_NAMESPACE_END
