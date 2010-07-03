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
 * $Id: DOM_Node.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include "DOM_Node.hpp"
#include "DOM_NodeList.hpp"
#include "DOM_NamedNodeMap.hpp"
#include "DOM_Document.hpp"

#include "NodeImpl.hpp"
#include <assert.h>

XERCES_CPP_NAMESPACE_BEGIN


DOM_Node::DOM_Node()
{
    fImpl = null;
}


DOM_Node::DOM_Node(NodeImpl *impl)
{
    fImpl = impl;
    RefCountedImpl::addRef(fImpl);
}


DOM_Node::DOM_Node(const DOM_Node &other)
{
	this->fImpl = other.fImpl;
    RefCountedImpl::addRef(fImpl);
}


DOM_Node & DOM_Node::operator = (const DOM_Node &other)
{
    if (this->fImpl != other.fImpl)
    {
        RefCountedImpl::removeRef(this->fImpl);
        this->fImpl = other.fImpl;
        RefCountedImpl::addRef(this->fImpl);
    }
    return *this;
}


DOM_Node & DOM_Node::operator = (const DOM_NullPtr * /*other*/)
{
    RefCountedImpl::removeRef(this->fImpl);
    this->fImpl = 0;
    return *this;
}



DOM_Node::~DOM_Node()
{
    RefCountedImpl::removeRef (this->fImpl);
    fImpl = 0;
}

//
//      Comparison operators.  Equivalent of Java object reference ==
//                                         Null references compare ==.
//
bool       DOM_Node::operator != (const DOM_Node & other) const
{
    return this->fImpl != other.fImpl;
}


bool       DOM_Node::operator == (const DOM_Node & other) const
{
    return this->fImpl == other.fImpl;
}

bool       DOM_Node::operator != (const DOM_NullPtr * /*other*/) const
{
    return this->fImpl != 0;
}


bool       DOM_Node::operator == (const DOM_NullPtr * /*other*/) const
{
    return this->fImpl == 0;
}




DOM_Node   DOM_Node::appendChild(const DOM_Node &newChild)
{
    return DOM_Node(fImpl->appendChild(newChild.fImpl));
}


DOM_Node      DOM_Node::cloneNode(bool deep) const
{
    return DOM_Node(fImpl->cloneNode(deep));
}


DOMString  DOM_Node::getNodeName()  const
{
    return fImpl->getNodeName().clone();
}


DOMString  DOM_Node::getNodeValue() const
{
    return fImpl->getNodeValue().clone();
}


short   DOM_Node::getNodeType() const
{
    return fImpl->getNodeType();
}


DOM_Node      DOM_Node::getParentNode() const
{
    return DOM_Node(fImpl->getParentNode());
}


DOM_NodeList      DOM_Node::getChildNodes() const
{
    return DOM_NodeList(fImpl);
}


DOM_Node      DOM_Node::getFirstChild() const
{
    return DOM_Node(fImpl->getFirstChild());
}


DOM_Node      DOM_Node::getLastChild() const
{
    return DOM_Node(fImpl->getLastChild());
}


DOM_Node      DOM_Node::getPreviousSibling() const
{
    return DOM_Node(fImpl->getPreviousSibling());
}


DOM_Node       DOM_Node::getNextSibling() const
{
    return DOM_Node(fImpl->getNextSibling());
}


void          *DOM_Node::getUserData() const
{
    return fImpl->getUserData ();
}

DOM_NamedNodeMap DOM_Node::getAttributes() const
{
	if (getNodeType() == ELEMENT_NODE)
		return (fImpl->getAttributes() == null) ? DOM_NamedNodeMap(fImpl) : DOM_NamedNodeMap(fImpl->getAttributes());
	else
		return DOM_NamedNodeMap();
}


DOM_Document   DOM_Node::getOwnerDocument() const
{
    return fImpl->getOwnerDocument();
}


bool           DOM_Node::hasChildNodes() const
{
    return fImpl->hasChildNodes();
}


DOM_Node       DOM_Node::insertBefore(const DOM_Node &newChild, const DOM_Node &refChild){
    return DOM_Node(fImpl->insertBefore(newChild.fImpl, refChild.fImpl));
}


bool               DOM_Node::isNull() const
{
    return fImpl == null;
}


DOM_Node       DOM_Node::replaceChild(const DOM_Node &newChild, const DOM_Node &oldChild){
    return DOM_Node(fImpl->replaceChild(newChild.fImpl, oldChild.fImpl));
}


DOM_Node       DOM_Node::removeChild(const DOM_Node &oldChild){
    return DOM_Node(fImpl->removeChild(oldChild.fImpl));
}


void           DOM_Node::setNodeValue(const DOMString &nodeValue)
{
    fImpl->setNodeValue(nodeValue);
}


void            DOM_Node::setUserData(void *p)
{
    fImpl->setUserData(p);
}


//Introduced in DOM Level 2

void              DOM_Node::normalize()
{
    fImpl->normalize();
}


bool              DOM_Node::isSupported(const DOMString &feature,
	                       const DOMString &version) const
{
    return fImpl->isSupported(feature, version);
}

DOMString         DOM_Node::getNamespaceURI() const
{
    return fImpl->getNamespaceURI().clone();
}

DOMString         DOM_Node::getPrefix() const
{
    return fImpl->getPrefix().clone();
}

DOMString         DOM_Node::getLocalName() const
{
    return fImpl->getLocalName().clone();
}

void              DOM_Node::setPrefix(const DOMString &prefix)
{
    fImpl->setPrefix(prefix);
}

bool              DOM_Node::hasAttributes() const
{
    return fImpl->hasAttributes();
}

XERCES_CPP_NAMESPACE_END

