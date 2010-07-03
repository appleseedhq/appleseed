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
 * $Id: DOM_NamedNodeMap.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include "DOM_Node.hpp"
#include "DOM_NamedNodeMap.hpp"
#include "NamedNodeMapImpl.hpp"
#include "ElementImpl.hpp"

XERCES_CPP_NAMESPACE_BEGIN



const unsigned short DOM_NamedNodeMap::NNM_ELEMENT  = 0;
const unsigned short DOM_NamedNodeMap::NNM_OTHER    = 1;


DOM_NamedNodeMap::DOM_NamedNodeMap()
{
    fImpl = 0;
	flagElem = NNM_OTHER;
}


DOM_NamedNodeMap::DOM_NamedNodeMap(const DOM_NamedNodeMap & other)
{
    this->fImpl = other.fImpl;
	this->flagElem = other.flagElem;
	(other.flagElem == NNM_ELEMENT) ? NodeImpl::addRef((NodeImpl *)fImpl) :	
	                                  NamedNodeMapImpl::addRef((NamedNodeMapImpl *)fImpl);
}


DOM_NamedNodeMap::DOM_NamedNodeMap(NamedNodeMapImpl *impl)
{
	fImpl = impl;
	flagElem = NNM_OTHER;
	if (impl != null)
		NamedNodeMapImpl::addRef((NamedNodeMapImpl *)fImpl);
}

DOM_NamedNodeMap::DOM_NamedNodeMap(NodeImpl *impl)
{
	fImpl = impl;
	flagElem = NNM_ELEMENT;
	NodeImpl::addRef((NodeImpl *)fImpl);
}


DOM_NamedNodeMap::~DOM_NamedNodeMap()
{
	(flagElem == NNM_OTHER) ? NamedNodeMapImpl::removeRef((NamedNodeMapImpl *)fImpl) :
	                          NodeImpl::removeRef((NodeImpl *)fImpl);
}

bool DOM_NamedNodeMap::operator == (const DOM_NamedNodeMap &other) const
{
    return this->fImpl == other.fImpl;
}


bool DOM_NamedNodeMap::operator != (const DOM_NamedNodeMap &other) const
{
    return this->fImpl != other.fImpl;
}


bool DOM_NamedNodeMap::operator == (const DOM_NullPtr * /*p*/) const
{
    return this->fImpl == 0;
}


bool DOM_NamedNodeMap::operator != (const DOM_NullPtr * /*p*/) const
{
    return this->fImpl != 0;
}


DOM_NamedNodeMap & DOM_NamedNodeMap::operator = (const DOM_NamedNodeMap & other)
{
    if (this->fImpl != other.fImpl)
    {
		// update reference counts and change pointers
        (flagElem == NNM_OTHER) ? NamedNodeMapImpl::removeRef((NamedNodeMapImpl *)fImpl) : NodeImpl::removeRef((NodeImpl *)fImpl);

        this->fImpl = other.fImpl;
		this->flagElem = other.flagElem;

        (flagElem == NNM_OTHER) ? NamedNodeMapImpl::addRef((NamedNodeMapImpl *)fImpl) : NodeImpl::addRef((NodeImpl *)fImpl);
    }
    return *this;
}


DOM_NamedNodeMap & DOM_NamedNodeMap::operator = (const DOM_NullPtr * /*other*/)
{

    (flagElem == NNM_OTHER) ? NamedNodeMapImpl::removeRef((NamedNodeMapImpl *)fImpl) : NodeImpl::removeRef((NodeImpl *)fImpl);
    this->fImpl = 0;
	this->flagElem = NNM_OTHER;
    return *this;
}


DOM_Node DOM_NamedNodeMap::getNamedItem(const DOMString &name) const
{
	return (flagElem == NNM_OTHER) ? DOM_Node(((NamedNodeMapImpl *)fImpl)->getNamedItem(name)) :
	                                 DOM_Node(((ElementImpl *)fImpl)->NNM_getNamedItem(name));
}


DOM_Node DOM_NamedNodeMap::setNamedItem(DOM_Node arg)
{
	return (flagElem == NNM_OTHER) ? DOM_Node(((NamedNodeMapImpl *)fImpl)->setNamedItem(arg.fImpl)) :
	                                 DOM_Node(((ElementImpl *)fImpl)->NNM_setNamedItem(arg.fImpl));
}


DOM_Node DOM_NamedNodeMap::removeNamedItem(const DOMString &name)
{
	return (flagElem == NNM_OTHER) ? DOM_Node(((NamedNodeMapImpl *)fImpl)->removeNamedItem(name)) :
	                                 DOM_Node(((ElementImpl *)fImpl)->NNM_removeNamedItem(name));
}


DOM_Node DOM_NamedNodeMap::item(unsigned int index) const
{
	return (flagElem == NNM_OTHER) ? DOM_Node(((NamedNodeMapImpl *)fImpl)->item(index)) :
	                                 DOM_Node(((ElementImpl *)fImpl)->NNM_item(index));
}


unsigned int DOM_NamedNodeMap::getLength() const
{
	return (flagElem == NNM_OTHER) ? ((NamedNodeMapImpl *)fImpl)->getLength() :
	                                 ((ElementImpl *)fImpl)->NNM_getLength();
}


//Introduced in DOM Level 2

DOM_Node DOM_NamedNodeMap::getNamedItemNS(const DOMString &namespaceURI,
	const DOMString &localName)
{
	return (flagElem == NNM_OTHER) ? DOM_Node(((NamedNodeMapImpl *)fImpl)->getNamedItemNS(namespaceURI, localName)) :
									 DOM_Node(((ElementImpl *)fImpl)->NNM_getNamedItemNS(namespaceURI, localName));
}

DOM_Node DOM_NamedNodeMap::setNamedItemNS(DOM_Node arg)
{
    return (flagElem == NNM_OTHER) ? DOM_Node(((NamedNodeMapImpl *)fImpl)->setNamedItemNS(arg.fImpl)) :
	                                 DOM_Node(((ElementImpl *)fImpl)->NNM_setNamedItemNS(arg.fImpl));
}

DOM_Node DOM_NamedNodeMap::removeNamedItemNS(const DOMString &namespaceURI,
	const DOMString &localName)
{
	return (flagElem == NNM_OTHER) ? DOM_Node(((NamedNodeMapImpl *)fImpl)->removeNamedItemNS(namespaceURI, localName)) :
                                     DOM_Node(((ElementImpl *)fImpl)->NNM_removeNamedItemNS(namespaceURI, localName));
}

XERCES_CPP_NAMESPACE_END

