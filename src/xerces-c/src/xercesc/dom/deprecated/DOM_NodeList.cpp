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
 * $Id: DOM_NodeList.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include "DOM_NodeList.hpp"
#include "NodeListImpl.hpp"
#include "DOM_Node.hpp"
#include <assert.h>

XERCES_CPP_NAMESPACE_BEGIN


DOM_NodeList::DOM_NodeList()
{
    fImpl = 0;
}


DOM_NodeList::DOM_NodeList(NodeListImpl *impl)
{
    fImpl = impl;
    RefCountedImpl::addRef(fImpl);
}


DOM_NodeList::DOM_NodeList(const DOM_NodeList &other)
{
    fImpl = other.fImpl;
    RefCountedImpl::addRef(fImpl);
}


DOM_NodeList & DOM_NodeList::operator = (const DOM_NodeList &other)
{
    if (this->fImpl != other.fImpl)
    {
        RefCountedImpl::removeRef(this->fImpl);
        this->fImpl = other.fImpl;
        RefCountedImpl::addRef(this->fImpl);
    }
    return *this;
}


DOM_NodeList & DOM_NodeList::operator = (const DOM_NullPtr * /*other*/)
{
    RefCountedImpl::removeRef(this->fImpl);
    this->fImpl = 0;
    return *this;
}


DOM_NodeList::~DOM_NodeList()
{
    RefCountedImpl::removeRef(this->fImpl);
    fImpl = 0;
}


bool DOM_NodeList::operator == (const DOM_NodeList &other) const
{
    return this->fImpl == other.fImpl;
}


bool DOM_NodeList::operator != (const DOM_NodeList &other) const
{
    return this->fImpl != other.fImpl;
}


bool DOM_NodeList::operator == (const DOM_NullPtr * /*nullPtr*/) const
{
    return this->fImpl == 0;
}


bool DOM_NodeList::operator != (const DOM_NullPtr * /*nullPtr*/) const
{
    return this->fImpl != 0;
}




DOM_Node  DOM_NodeList::item(unsigned int index) const
{
    return DOM_Node(fImpl->item(index));
}


unsigned int DOM_NodeList::getLength() const
{
    return fImpl->getLength();
}

XERCES_CPP_NAMESPACE_END

