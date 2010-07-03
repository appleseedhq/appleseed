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
 * $Id: Wrapper4DOMInputSource.cpp 568078 2007-08-21 11:43:25Z amassari $
 */


// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/framework/Wrapper4DOMInputSource.hpp>
#include <xercesc/dom/DOMInputSource.hpp>
#include <xercesc/util/NullPointerException.hpp>

XERCES_CPP_NAMESPACE_BEGIN

// ---------------------------------------------------------------------------
//  Wrapper4DOMInputSource: Constructor and Destructor
// ---------------------------------------------------------------------------
Wrapper4DOMInputSource::Wrapper4DOMInputSource(DOMInputSource* const inputSource,
                                               const bool adoptFlag,
                                               MemoryManager* const  manager) :
    InputSource(manager)
    , fAdoptInputSource(adoptFlag)
    ,  fInputSource(inputSource)
{
    if (!inputSource)
        ThrowXMLwithMemMgr(NullPointerException, XMLExcepts::CPtr_PointerIsZero, getMemoryManager());
}

Wrapper4DOMInputSource::~Wrapper4DOMInputSource()
{
    if (fAdoptInputSource)
        delete fInputSource;
}


// ---------------------------------------------------------------------------
//  Wrapper4DOMInputSource: Getter methods
// ---------------------------------------------------------------------------
bool Wrapper4DOMInputSource::getIssueFatalErrorIfNotFound() const
{
    return fInputSource->getIssueFatalErrorIfNotFound();
}

const XMLCh* Wrapper4DOMInputSource::getEncoding() const
{
    return fInputSource->getEncoding();
}

const XMLCh* Wrapper4DOMInputSource::getSystemId() const
{
    return fInputSource->getSystemId();
}

const XMLCh* Wrapper4DOMInputSource::getPublicId() const
{
    return fInputSource->getPublicId();
}


// ---------------------------------------------------------------------------
//  Wrapper4DOMInputSource: Setter methods
// ---------------------------------------------------------------------------
void Wrapper4DOMInputSource::setIssueFatalErrorIfNotFound(const bool flag)
{
    fInputSource->setIssueFatalErrorIfNotFound(flag);
}


void Wrapper4DOMInputSource::setEncoding(const XMLCh* const encodingStr)
{
    fInputSource->setEncoding(encodingStr);
}


void Wrapper4DOMInputSource::setPublicId(const XMLCh* const publicId)
{
    fInputSource->setPublicId(publicId);
}


void Wrapper4DOMInputSource::setSystemId(const XMLCh* const systemId)
{
    fInputSource->setSystemId(systemId);
}


// ---------------------------------------------------------------------------
//  Wrapper4DOMInputSource: Stream methods
// ---------------------------------------------------------------------------
BinInputStream* Wrapper4DOMInputSource::makeStream() const
{
    return fInputSource->makeStream();
}

XERCES_CPP_NAMESPACE_END

