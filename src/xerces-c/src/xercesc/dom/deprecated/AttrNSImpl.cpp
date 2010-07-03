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
 * $Id: AttrNSImpl.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include <xercesc/util/XMLUniDefs.hpp>
#include "AttrNSImpl.hpp"
#include "DocumentImpl.hpp"
#include "DOM_DOMException.hpp"

XERCES_CPP_NAMESPACE_BEGIN



AttrNSImpl::AttrNSImpl(DocumentImpl *ownerDoc, const DOMString &nam) :
    AttrImpl(ownerDoc, nam)
{
    this->namespaceURI=null;	//DOM Level 2
    this->localName=null;       //DOM Level 2
}

//Introduced in DOM Level 2
AttrNSImpl::AttrNSImpl(DocumentImpl *ownerDoc,
                       const DOMString &fNamespaceURI,
                       const DOMString &qualifiedName) :
    AttrImpl(ownerDoc, qualifiedName)
{
    DOMString xmlns = NodeImpl::getXmlnsString();
    DOMString xmlnsURI = NodeImpl::getXmlnsURIString();
    this->name = qualifiedName.clone();

    int index = DocumentImpl::indexofQualifiedName(qualifiedName);
    DOMString prefix;
    if (index < 0)
	throw DOM_DOMException(DOM_DOMException::NAMESPACE_ERR, null);
    bool xmlnsAlone = false;	//true if attribute name is "xmlns"
    if (index == 0) {	//qualifiedName contains no ':'
        if (this->name.equals(xmlns)) {
	    if (!fNamespaceURI.equals(xmlnsURI))
		throw DOM_DOMException(DOM_DOMException::NAMESPACE_ERR, null);
	    xmlnsAlone = true;
	}
	prefix = null;
	this -> localName = this -> name;
    } else {	//0 < index < this->name.length()-1
	prefix = this->name.substringData(0, index);
	this -> localName =
            this->name.substringData(index+1, this->name.length()-index-1);
    }

    const DOMString& URI = xmlnsAlone ?
        xmlnsURI : mapPrefix(prefix, fNamespaceURI, DOM_Node::ATTRIBUTE_NODE);
    this -> namespaceURI = URI == null ? DOMString(null) : URI.clone();
};

AttrNSImpl::AttrNSImpl(const AttrNSImpl &other, bool deep) :
    AttrImpl(other, deep)
{
    this->namespaceURI = other.namespaceURI.clone();	//DOM Level 2
    this->localName = other.localName.clone();          //DOM Level 2
};

NodeImpl * AttrNSImpl::cloneNode(bool deep)
{
    return new (getOwnerDocument()->getMemoryManager()) AttrNSImpl(*this, deep);
};

DOMString AttrNSImpl::getNamespaceURI()
{
    return namespaceURI;
}

DOMString AttrNSImpl::getPrefix()
{
    int index = DocumentImpl::indexofQualifiedName(name);
    if (index == 0)
        return null;
    else
        return name.substringData(0, index);
}

DOMString AttrNSImpl::getLocalName()
{
    return localName;
}

void AttrNSImpl::setPrefix(const DOMString &prefix)
{
    DOMString xml = NodeImpl::getXmlString();
    DOMString xmlURI = NodeImpl::getXmlURIString();
    DOMString xmlns = NodeImpl::getXmlnsString();
    DOMString xmlnsURI = NodeImpl::getXmlnsURIString();

    if (getOwnerDocument()->getErrorChecking()) {
        if (isReadOnly()) {
            throw DOM_DOMException(
                                 DOM_DOMException::NO_MODIFICATION_ALLOWED_ERR,
                                 null);
        }
        if (namespaceURI == null || localName.equals(xmlns)) {
            throw DOM_DOMException(DOM_DOMException::NAMESPACE_ERR, null);
        }
        if (prefix != null && !((DocumentImpl *)this->getOwnerDocument())->isXMLName(prefix)) {
            throw DOM_DOMException(DOM_DOMException::INVALID_CHARACTER_ERR,
                                   null);
        }
    }
    if (prefix == null || prefix.length() == 0) {
        name = localName;
        return;
    }
    if (getOwnerDocument()->getErrorChecking() &&
        (prefix.equals(xml) && !namespaceURI.equals(xmlURI) ||
         prefix.equals(xmlns) && !namespaceURI.equals(xmlnsURI))) {
        throw DOM_DOMException(DOM_DOMException::NAMESPACE_ERR, null);
    }
    const XMLCh *p = prefix.rawBuffer();
    for (int i = prefix.length(); --i >= 0;)
        if (*p++ == chColon)	//prefix is malformed
            throw DOM_DOMException(DOM_DOMException::NAMESPACE_ERR, null);

    name = prefix + chColon + localName; //nodeName is changed too
}

XERCES_CPP_NAMESPACE_END

