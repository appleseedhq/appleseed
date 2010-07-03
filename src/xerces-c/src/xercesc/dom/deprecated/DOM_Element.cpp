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
 * $Id: DOM_Element.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include "DOM_Element.hpp"
#include "DOM_Attr.hpp"
#include "DOM_NodeList.hpp"
#include "ElementImpl.hpp"
#include "DeepNodeListImpl.hpp"

XERCES_CPP_NAMESPACE_BEGIN


DOM_Element::DOM_Element()
: DOM_Node(null)
{
};


DOM_Element::DOM_Element(const DOM_Element & other)
: DOM_Node(other)
{
};


DOM_Element::DOM_Element(ElementImpl *impl) :
        DOM_Node(impl)
{
};


DOM_Element::~DOM_Element()
{
};


DOM_Element & DOM_Element::operator = (const DOM_Element & other)
{
    return (DOM_Element &) DOM_Node::operator = (other);
};


DOM_Element & DOM_Element::operator = (const DOM_NullPtr *other)
{
    return (DOM_Element &) DOM_Node::operator = (other);
};


DOMString DOM_Element::getTagName() const
{
        return ((ElementImpl *)fImpl)->getTagName().clone();
};


DOMString DOM_Element::getAttribute(const DOMString &name) const
{
        return ((ElementImpl *)fImpl)->getAttribute(name).clone();
};


void      DOM_Element::setAttribute(const DOMString &name,
                                    const DOMString &value)
{
        ((ElementImpl *)fImpl)->setAttribute(name, value);
};



void      DOM_Element::removeAttribute(const DOMString &name)
{
        ((ElementImpl *)fImpl)->removeAttribute(name);
};


DOM_Attr  DOM_Element::getAttributeNode(const DOMString &name) const
{
        return DOM_Attr(((ElementImpl *)fImpl)->getAttributeNode(name));
};


DOM_Attr  DOM_Element::setAttributeNode(DOM_Attr newAttr)
{
        return DOM_Attr(((ElementImpl *)fImpl)->
                             setAttributeNode((AttrImpl *)newAttr.fImpl));
};


DOM_Attr  DOM_Element::removeAttributeNode(DOM_Attr oldAttr)
{
        return DOM_Attr(((ElementImpl *)fImpl)->
                                                removeAttributeNode((AttrImpl *)oldAttr.fImpl));
};


DOM_NodeList DOM_Element::getElementsByTagName(const DOMString &name) const
{
        return DOM_NodeList(((ElementImpl *)fImpl)->getElementsByTagName(name));

};


//Introduced in DOM Level 2

DOMString DOM_Element::getAttributeNS(const DOMString &namespaceURI,
	const DOMString &localName) const
{
    return ((ElementImpl *)fImpl)->getAttributeNS(namespaceURI, localName).clone();
}

void DOM_Element::setAttributeNS(const DOMString &namespaceURI,
	const DOMString &qualifiedName, const DOMString &value)
{
    ((ElementImpl *)fImpl)->setAttributeNS(namespaceURI, qualifiedName, value);
}


void DOM_Element::removeAttributeNS(const DOMString &namespaceURI,
	const DOMString &localName)
{
    ((ElementImpl *)fImpl)->removeAttributeNS(namespaceURI, localName);
}


DOM_Attr DOM_Element::getAttributeNodeNS(const DOMString &namespaceURI,
	const DOMString &localName) const
{
    return DOM_Attr(((ElementImpl *)fImpl)->getAttributeNodeNS(namespaceURI, localName));
}


DOM_Attr DOM_Element::setAttributeNodeNS(DOM_Attr newAttr)
{
    return DOM_Attr(((ElementImpl *)fImpl)->
                             setAttributeNodeNS((AttrImpl *)newAttr.fImpl));
}


DOM_NodeList DOM_Element::getElementsByTagNameNS(const DOMString &namespaceURI,
	const DOMString &localName) const
{
    return DOM_NodeList(((ElementImpl *)fImpl)->getElementsByTagNameNS(namespaceURI,
	localName));
}


bool DOM_Element::hasAttributes() const
{
        return ((ElementImpl *)fImpl)->hasAttributes();
};


bool DOM_Element::hasAttribute(const DOMString &name) const
{
        return ((ElementImpl *)fImpl)->hasAttribute(name);
};


bool DOM_Element::hasAttributeNS(const DOMString &namespaceURI,
	const DOMString &localName) const
{
    return ((ElementImpl *)fImpl)->hasAttributeNS(namespaceURI, localName);
}


XERCES_CPP_NAMESPACE_END

