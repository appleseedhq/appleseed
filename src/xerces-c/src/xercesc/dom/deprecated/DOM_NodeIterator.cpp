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
 * $Id: DOM_NodeIterator.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include "DOM_NodeIterator.hpp"
#include "NodeIteratorImpl.hpp"
#include "RefCountedImpl.hpp"

XERCES_CPP_NAMESPACE_BEGIN



DOM_NodeIterator::DOM_NodeIterator()
{
    fImpl = 0;
}


DOM_NodeIterator::DOM_NodeIterator(NodeIteratorImpl *impl)
{
    fImpl = impl;
    RefCountedImpl::addRef(fImpl);
}


DOM_NodeIterator::DOM_NodeIterator(const DOM_NodeIterator &other)
{
	  this->fImpl = other.fImpl;
    RefCountedImpl::addRef(fImpl);
}


DOM_NodeIterator & DOM_NodeIterator::operator = (const DOM_NodeIterator &other)
{
    if (this->fImpl != other.fImpl)
    {
        RefCountedImpl::removeRef(this->fImpl);
        this->fImpl = other.fImpl;
        RefCountedImpl::addRef(this->fImpl);
    }
    return *this;
};


DOM_NodeIterator & DOM_NodeIterator::operator = (const DOM_NullPtr * /*other*/)
{
    RefCountedImpl::removeRef(this->fImpl);
    this->fImpl = 0;
    return *this;
};



DOM_NodeIterator::~DOM_NodeIterator()
{
    RefCountedImpl::removeRef (this->fImpl);
    fImpl = 0;
};

//
//      Comparison operators.  Equivalent of Java object reference ==
//                                         Null references compare ==.
//
bool       DOM_NodeIterator::operator != (const DOM_NodeIterator & other) const
{
    return this->fImpl != other.fImpl;
};


bool       DOM_NodeIterator::operator == (const DOM_NodeIterator & other) const
{
    return this->fImpl == other.fImpl;
};

bool       DOM_NodeIterator::operator != (const DOM_NullPtr * /*other*/) const
{
    return this->fImpl != 0;
};


bool       DOM_NodeIterator::operator == (const DOM_NullPtr * /*other*/) const
{
    return this->fImpl == 0;
}


void DOM_NodeIterator::detach ()
{
	fImpl->detach();
}



DOM_Node DOM_NodeIterator::getRoot()
{
	  return fImpl->getRoot();
}


unsigned long DOM_NodeIterator::getWhatToShow ()
{
	  return fImpl->getWhatToShow();
}


DOM_NodeFilter*     DOM_NodeIterator::getFilter() {
    return fImpl->getFilter();
}

/** Get the expandEntity reference flag. */
bool DOM_NodeIterator::getExpandEntityReferences()
{
    if (fImpl !=0)
        return fImpl->getExpandEntityReferences();
    return false;
}


DOM_Node            DOM_NodeIterator::nextNode() {
    return fImpl->nextNode();
}


DOM_Node            DOM_NodeIterator::previousNode() {
  return fImpl->previousNode();
}

XERCES_CPP_NAMESPACE_END

