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
 * $Id: DOM_Range.cpp 568078 2007-08-21 11:43:25Z amassari $
 */



#include "DOM_Range.hpp"
#include "DocumentImpl.hpp"
#include "RangeImpl.hpp"

XERCES_CPP_NAMESPACE_BEGIN


DOM_Range::DOM_Range()
:fImpl(0)
{
}

DOM_Range::DOM_Range(RangeImpl* impl)
{
    fImpl = impl;
    RefCountedImpl::addRef(fImpl);
}

DOM_Range::DOM_Range(const DOM_Range& other)
{
    fImpl = other.fImpl;
    RefCountedImpl::addRef(fImpl);
}

DOM_Range::~DOM_Range()
{
    RefCountedImpl::removeRef (this->fImpl);
    fImpl = 0;
};

DOM_Range & DOM_Range::operator = (const DOM_Range &other)
{
    if (this->fImpl != other.fImpl)
    {
        RefCountedImpl::removeRef(this->fImpl);
        this->fImpl = other.fImpl;
        RefCountedImpl::addRef(this->fImpl);
    }
    return *this;
};


DOM_Range & DOM_Range::operator = (const DOM_NullPtr * /*other*/)
{
    RefCountedImpl::removeRef(this->fImpl);
    this->fImpl = 0;
    return *this;
};

bool       DOM_Range::operator != (const DOM_Range & other) const
{
    return this->fImpl != other.fImpl;
};


bool       DOM_Range::operator == (const DOM_Range & other) const
{
    return this->fImpl == other.fImpl;
};

bool       DOM_Range::operator != (const DOM_NullPtr * /*other*/) const
{
    return this->fImpl != 0;
};


bool       DOM_Range::operator == (const DOM_NullPtr * /*other*/) const
{
    return this->fImpl == 0;
}

//getter functions

DOM_Node DOM_Range::getStartContainer() const
{
    return ((RangeImpl *)fImpl)->getStartContainer();
}
unsigned int DOM_Range::getStartOffset() const
{
    return ((RangeImpl *)fImpl)->getStartOffset();
}
DOM_Node DOM_Range::getEndContainer() const
{
        return ((RangeImpl *)fImpl)->getEndContainer();
}
unsigned int DOM_Range::getEndOffset() const
{
        return ((RangeImpl *)fImpl)->getEndOffset();
}
const DOM_Node DOM_Range::getCommonAncestorContainer() const
{
        return ((RangeImpl *)fImpl)->getCommonAncestorContainer();
}

//setter functions

void DOM_Range::setStart(const DOM_Node& parent, unsigned int offset)
{
    this->fImpl->setStart(parent, offset);
}

void DOM_Range::setEnd(const DOM_Node& parent, unsigned int offset)
{
    this->fImpl->setEnd(parent, offset);
}

void DOM_Range::setStartBefore(const DOM_Node& refNode)
{
    this->fImpl->setStartBefore(refNode);
}

void DOM_Range::setStartAfter(const DOM_Node& refNode)
{
    this->fImpl->setStartAfter(refNode);
}

void DOM_Range::setEndBefore(const DOM_Node& refNode)
{
    this->fImpl->setEndBefore(refNode);
}

void DOM_Range::setEndAfter(const DOM_Node& refNode)
{
    this->fImpl->setEndAfter(refNode);
}

//misc functions
void DOM_Range::collapse(bool toStart)
{
    this->fImpl->collapse(toStart);
}

bool DOM_Range::getCollapsed() const
{
    return ((RangeImpl *)fImpl)->getCollapsed();
}

void DOM_Range::selectNode(const DOM_Node& node)
{
  ((RangeImpl *)fImpl)->selectNode(node);
}
void DOM_Range::selectNodeContents(const DOM_Node& node)
{
    ((RangeImpl *)fImpl)->selectNodeContents(node);
}

//Functions related to comparing ange Boundrary-Points
short DOM_Range::compareBoundaryPoints(CompareHow how, const DOM_Range& range) const
{
    return ((RangeImpl *)fImpl)->compareBoundaryPoints(how, range.fImpl);
}

void DOM_Range::deleteContents()
{
    ((RangeImpl *)fImpl)->deleteContents();
}

DOM_DocumentFragment DOM_Range::extractContents()
{
    return ((RangeImpl *)fImpl)->extractContents();
}

DOM_DocumentFragment DOM_Range::cloneContents() const
{
    return ((RangeImpl *)fImpl)->cloneContents();
}

void DOM_Range::insertNode(DOM_Node& node)
{
    ((RangeImpl *)fImpl)->insertNode(node);
}

//Misc functions
void DOM_Range::surroundContents(DOM_Node& node)
{
    ((RangeImpl *)fImpl)->surroundContents(node);
}

DOM_Range DOM_Range::cloneRange() const
{
    return DOM_Range( ((RangeImpl *)fImpl)->cloneRange() );
}

DOMString DOM_Range::toString() const
{
    return ((RangeImpl *)fImpl)->toString();
}

void DOM_Range::detach()
{
    ((RangeImpl *)fImpl)->detach();
}

XERCES_CPP_NAMESPACE_END

