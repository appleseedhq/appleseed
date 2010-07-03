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
 * $Id: DOMAttrNSImpl.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include <xercesc/util/XMLUniDefs.hpp>
#include "DOMAttrNSImpl.hpp"
#include "DOMDocumentImpl.hpp"
#include <xercesc/dom/DOMDocument.hpp>
#include <xercesc/dom/DOMElement.hpp>
#include <xercesc/dom/DOMException.hpp>

#include "assert.h"

XERCES_CPP_NAMESPACE_BEGIN

DOMAttrNSImpl::DOMAttrNSImpl(DOMDocument *ownerDoc, const XMLCh *nam) :
DOMAttrImpl(ownerDoc, nam)
{
    this->fNamespaceURI=0;	//DOM Level 2
    this->fLocalName=0;       //DOM Level 2
    this->fPrefix=0;
}

//Introduced in DOM Level 2
DOMAttrNSImpl::DOMAttrNSImpl(DOMDocument *ownerDoc,
                           const XMLCh *namespaceURI,
                           const XMLCh *qualifiedName) :
DOMAttrImpl(ownerDoc, qualifiedName)
{
    setName(namespaceURI, qualifiedName);
}

DOMAttrNSImpl::DOMAttrNSImpl(const DOMAttrNSImpl &other, bool deep) :
DOMAttrImpl(other, deep)
{
    this->fNamespaceURI = other.fNamespaceURI;	//DOM Level 2
    this->fLocalName = other.fLocalName;          //DOM Level 2
    this->fPrefix = other.fPrefix;
}

DOMNode * DOMAttrNSImpl::cloneNode(bool deep) const
{
    DOMNode* newNode = new (getOwnerDocument(), DOMDocumentImpl::ATTR_NS_OBJECT) DOMAttrNSImpl(*this, deep);
    fNode.callUserDataHandlers(DOMUserDataHandler::NODE_CLONED, this, newNode);
    return newNode;
}

const XMLCh * DOMAttrNSImpl::getNamespaceURI() const
{
    return fNamespaceURI;
}

const XMLCh * DOMAttrNSImpl::getPrefix() const
{
    return fPrefix;
}

const XMLCh * DOMAttrNSImpl::getLocalName() const
{
    return fLocalName;
}

void DOMAttrNSImpl::setPrefix(const XMLCh *prefix)
{
    const XMLCh * xmlns = DOMNodeImpl::getXmlnsString();

    if (fNode.isReadOnly())
        throw DOMException(DOMException::NO_MODIFICATION_ALLOWED_ERR, 0, GetDOMNodeMemoryManager);
    if (fNamespaceURI == 0 || fNamespaceURI[0] == chNull || XMLString::equals(fLocalName, xmlns))
        throw DOMException(DOMException::NAMESPACE_ERR, 0, GetDOMNodeMemoryManager);

    if (prefix == 0 || prefix[0] == chNull) {
        fName = fLocalName;
        fPrefix = 0;
        return;
    }

    if (!((DOMDocumentImpl *)this->getOwnerDocument())->isXMLName(prefix))
        throw DOMException(DOMException::INVALID_CHARACTER_ERR,0, GetDOMNodeMemoryManager);

    const XMLCh * xml = DOMNodeImpl::getXmlString();
    const XMLCh * xmlURI = DOMNodeImpl::getXmlURIString();
    const XMLCh * xmlnsURI = DOMNodeImpl::getXmlnsURIString();

    if (XMLString::equals(prefix, xml)&&
        !XMLString::equals(fNamespaceURI, xmlURI)||
        XMLString::equals(prefix, xmlns)&&
        !XMLString::equals(fNamespaceURI, xmlnsURI))
        throw DOMException(DOMException::NAMESPACE_ERR, 0, GetDOMNodeMemoryManager);

    if (XMLString::indexOf(prefix, chColon) != -1) {
        throw DOMException(DOMException::NAMESPACE_ERR, 0, GetDOMNodeMemoryManager);
    }

    this-> fPrefix = ((DOMDocumentImpl *)this->getOwnerDocument())->getPooledString(prefix);

    int prefixLen = XMLString::stringLen(prefix);
    int newQualifiedNameLen = prefixLen+1+XMLString::stringLen(fLocalName);
    XMLCh* newName;
    XMLCh temp[4000];
    if (newQualifiedNameLen >= 3999)
        newName = (XMLCh*) ((DOMDocumentImpl *)this->getOwnerDocument())->getMemoryManager()->allocate
        (
            newQualifiedNameLen * sizeof(XMLCh)
        );//new XMLCh[newQualifiedNameLen];
    else
        newName = temp;

    // newName = prefix + chColon + fLocalName;
    XMLString::copyString(newName, prefix);
    newName[prefixLen] = chColon;
    XMLString::copyString(&newName[prefixLen+1], fLocalName);

    fName = ((DOMDocumentImpl *)this->getOwnerDocument())->
                                           getPooledString(newName);

    if (newQualifiedNameLen >= 3999)
        ((DOMDocumentImpl *)this->getOwnerDocument())->getMemoryManager()->deallocate(newName);//delete[] newName;

}

void DOMAttrNSImpl::release()
{
    if (fNode.isOwned() && !fNode.isToBeReleased())
        throw DOMException(DOMException::INVALID_ACCESS_ERR,0, GetDOMNodeMemoryManager);

    DOMDocumentImpl* doc = (DOMDocumentImpl*) getOwnerDocument();
    if (doc) {
        fNode.callUserDataHandlers(DOMUserDataHandler::NODE_DELETED, 0, 0);
        fParent.release();
        doc->release(this, DOMDocumentImpl::ATTR_NS_OBJECT);
    }
    else {
        // shouldn't reach here
        throw DOMException(DOMException::INVALID_ACCESS_ERR,0, GetDOMNodeMemoryManager);
    }
}


DOMNode* DOMAttrNSImpl::rename(const XMLCh* namespaceURI, const XMLCh* name)
{
    DOMElement* el = getOwnerElement();
    if (el)
        el->removeAttributeNode(this);

    setName(namespaceURI, name);

    if (el)
        el->setAttributeNodeNS(this);

    return this;
}

void DOMAttrNSImpl::setName(const XMLCh* namespaceURI, const XMLCh* qualifiedName)
{
    DOMDocumentImpl* ownerDoc = (DOMDocumentImpl *) getOwnerDocument();
    const XMLCh * xmlns = DOMNodeImpl::getXmlnsString();
    const XMLCh * xmlnsURI = DOMNodeImpl::getXmlnsURIString();
    this->fName = ownerDoc->getPooledString(qualifiedName);

    int index = DOMDocumentImpl::indexofQualifiedName(qualifiedName);
    if (index < 0)
        throw DOMException(DOMException::NAMESPACE_ERR, 0, GetDOMNodeMemoryManager);

    bool xmlnsAlone = false;	//true if attribute name is "xmlns"
    if (index == 0) {	//qualifiedName contains no ':'
        if (XMLString::equals(this->fName, xmlns)) {
            if (!XMLString::equals(namespaceURI, xmlnsURI))
                throw DOMException(DOMException::NAMESPACE_ERR, 0, GetDOMNodeMemoryManager);
            xmlnsAlone = true;
        }
        this -> fPrefix = 0;
        this -> fLocalName = this -> fName;
    } else {	//0 < index < this->name.length()-1
        XMLCh* newName;
        XMLCh temp[4000];
        if (index >= 3999)
            newName = (XMLCh*) ((DOMDocumentImpl *)this->getOwnerDocument())->getMemoryManager()->allocate
            (
                (XMLString::stringLen(qualifiedName) + 1) * sizeof(XMLCh)
            );//new XMLCh[XMLString::stringLen(qualifiedName)+1];
        else
            newName = temp;

        XMLString::copyNString(newName, fName, index);
        newName[index] = chNull;
        this-> fPrefix = ownerDoc->getPooledString(newName);
        this -> fLocalName = ownerDoc->getPooledString(fName+index+1);

        if (index >= 3999)
            ((DOMDocumentImpl *)this->getOwnerDocument())->getMemoryManager()->deallocate(newName);//delete[] newName;

        // Before we carry on, we should check if the prefix or localName are valid XMLName
        if (!((DOMDocumentImpl *)this->getOwnerDocument())->isXMLName(fPrefix) || !((DOMDocumentImpl *)this->getOwnerDocument())->isXMLName(fLocalName))
            throw DOMException(DOMException::NAMESPACE_ERR, 0, GetDOMNodeMemoryManager);
    }

    // DOM Level 3: namespace URI is never empty string.
    const XMLCh * URI = xmlnsAlone ? xmlnsURI
        : DOMNodeImpl::mapPrefix
          (
              fPrefix,
              (!namespaceURI || !*namespaceURI) ? 0 : namespaceURI,
              DOMNode::ATTRIBUTE_NODE
          );
    this -> fNamespaceURI = (URI == 0) ? 0 : ownerDoc->getPooledString(URI);
}

XERCES_CPP_NAMESPACE_END

