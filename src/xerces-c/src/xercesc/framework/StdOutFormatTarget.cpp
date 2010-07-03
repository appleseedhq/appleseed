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
 * $Id: StdOutFormatTarget.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include <xercesc/framework/StdOutFormatTarget.hpp>
#include <stdio.h>

XERCES_CPP_NAMESPACE_BEGIN

StdOutFormatTarget::StdOutFormatTarget()
{}

StdOutFormatTarget::~StdOutFormatTarget()
{}

void StdOutFormatTarget::flush()
{
    fflush(stdout);
}

void StdOutFormatTarget::writeChars(const XMLByte* const  toWrite
                                  , const unsigned int    count
                                  , XMLFormatter* const)
{
    // Surprisingly, Solaris was the only platform on which
    // required the char* cast to print out the string correctly.
    // Without the cast, it was printing the pointer value in hex.
    // Quite annoying, considering every other platform printed
    // the string with the explicit cast to char* below.
    fwrite(toWrite, sizeof(XMLByte), (size_t)count, stdout);
    fflush(stdout);
}

XERCES_CPP_NAMESPACE_END

