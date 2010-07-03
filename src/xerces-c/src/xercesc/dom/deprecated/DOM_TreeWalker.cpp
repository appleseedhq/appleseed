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
 * $Id: DOM_TreeWalker.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include "DOM_TreeWalker.hpp"
#include "RefCountedImpl.hpp"
#include "TreeWalkerImpl.hpp"

XERCES_CPP_NAMESPACE_BEGIN


DOM_TreeWalker::DOM_TreeWalker()
{
    fImpl = 0;
}


DOM_TreeWalker::DOM_TreeWalker(TreeWalkerImpl *impl)
{
    fImpl = impl;
    RefCountedImpl::addRef(fImpl);
}


DOM_TreeWalker::DOM_TreeWalker(const DOM_TreeWalker &other)
{
	this->fImpl = other.fImpl;
    RefCountedImpl::addRef(fImpl);
}


DOM_TreeWalker & DOM_TreeWalker::operator = (const DOM_TreeWalker &other)
{
    if (this->fImpl != other.fImpl)
    {
        RefCountedImpl::removeRef(this->fImpl);
        this->fImpl = other.fImpl;
        RefCountedImpl::addRef(this->fImpl);
    }
    return *this;
};


DOM_TreeWalker & DOM_TreeWalker::operator = (const DOM_NullPtr * /*other*/)
{
    RefCountedImpl::removeRef(this->fImpl);
    this->fImpl = 0;
    return *this;
};



DOM_TreeWalker::~DOM_TreeWalker()
{
    RefCountedImpl::removeRef (this->fImpl);
    fImpl = 0;
};

//
//      Comparison operators.  Equivalent of Java object reference ==
//                                         Null references compare ==.
//
bool       DOM_TreeWalker::operator != (const DOM_TreeWalker & other) const
{
    return this->fImpl != other.fImpl;
};


bool       DOM_TreeWalker::operator == (const DOM_TreeWalker & other) const
{
    return this->fImpl == other.fImpl;
};

bool       DOM_TreeWalker::operator != (const DOM_NullPtr * /*other*/) const
{
    return this->fImpl != 0;
};


bool       DOM_TreeWalker::operator == (const DOM_NullPtr * /*other*/) const
{
    return this->fImpl == 0;
};



DOM_Node     		DOM_TreeWalker::getRoot() {
    return fImpl->getRoot();
}


unsigned long		DOM_TreeWalker::getWhatToShow() {
    return fImpl->getWhatToShow();
}


DOM_NodeFilter*					DOM_TreeWalker::getFilter() {
    return fImpl->getFilter();
}


DOM_Node								DOM_TreeWalker::getCurrentNode() {
    return fImpl->getCurrentNode();
}


void										DOM_TreeWalker::setCurrentNode(DOM_Node currentNode) {
    fImpl->setCurrentNode(currentNode);
}


DOM_Node								DOM_TreeWalker::parentNode() {
    return fImpl->parentNode();
}


DOM_Node								DOM_TreeWalker::firstChild() {
    return fImpl->firstChild();
}


DOM_Node								DOM_TreeWalker::lastChild() {
    return fImpl->lastChild();
}


DOM_Node								DOM_TreeWalker::previousSibling() {
    return fImpl->previousSibling();
}


DOM_Node								DOM_TreeWalker::nextSibling() {
    return fImpl->nextSibling();
}


DOM_Node								DOM_TreeWalker::previousNode() {
    return fImpl->previousNode();
}


DOM_Node								DOM_TreeWalker::nextNode() {
    return fImpl->nextNode();
}

XERCES_CPP_NAMESPACE_END


