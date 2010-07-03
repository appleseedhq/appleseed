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
 * $Id: DOMElementNSImpl.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include <xercesc/util/XMLUniDefs.hpp>
#include "DOMElementNSImpl.hpp"
#include "DOMDocumentImpl.hpp"
#include "DOMTypeInfoImpl.hpp"
#include <xercesc/dom/DOMException.hpp>
#include <xercesc/util/XMLUri.hpp>
#include <xercesc/util/OutOfMemoryException.hpp>

XERCES_CPP_NAMESPACE_BEGIN

DOMElementNSImpl::DOMElementNSImpl(DOMDocument *ownerDoc, const XMLCh *nam) :
    DOMElementImpl(ownerDoc, nam)
{
    this->fNamespaceURI=0;	  //DOM Level 2
    this->fLocalName=0;       //DOM Level 2
    this->fPrefix=0;
    this->fSchemaType = 0;
}

//Introduced in DOM Level 2
DOMElementNSImpl::DOMElementNSImpl(DOMDocument *ownerDoc,
                             const XMLCh *namespaceURI,
                             const XMLCh *qualifiedName) :
    DOMElementImpl(ownerDoc, qualifiedName)
{
    setName(namespaceURI, qualifiedName);
    this->fSchemaType = 0;
}

DOMElementNSImpl::DOMElementNSImpl(const DOMElementNSImpl &other, bool deep) :
    DOMElementImpl(other, deep)
{
    this->fNamespaceURI = other.fNamespaceURI;	        //DOM Level 2
    this->fLocalName = other.fLocalName;                //DOM Level 2
    this->fPrefix = other.fPrefix;
    this->fSchemaType = other.fSchemaType;
}

DOMNode * DOMElementNSImpl::cloneNode(bool deep) const {
    DOMNode* newNode = new (getOwnerDocument(), DOMDocumentImpl::ELEMENT_NS_OBJECT) DOMElementNSImpl(*this, deep);
    fNode.callUserDataHandlers(DOMUserDataHandler::NODE_CLONED, this, newNode);
    return newNode;
}

const XMLCh * DOMElementNSImpl::getNamespaceURI() const
{
    return fNamespaceURI;
}

const XMLCh * DOMElementNSImpl::getPrefix() const
{
    return fPrefix;
}


const XMLCh * DOMElementNSImpl::getLocalName() const
{
    return fLocalName;
}

const XMLCh* DOMElementNSImpl::getBaseURI() const
{
    const XMLCh* baseURI = (fNode.fOwnerNode)->getBaseURI();
    if (fAttributes) {
        const XMLCh baseString[] =
        {
            chLatin_b, chLatin_a, chLatin_s, chLatin_e, chNull
        };
        DOMNode* attrNode = fAttributes->getNamedItemNS(DOMNodeImpl::getXmlURIString(), baseString);
        if (attrNode) {
            const XMLCh* uri =  attrNode->getNodeValue();
            if (uri && *uri) {// attribute value is always empty string
                // if there is a base URI for the parent node, use it to resolve relative URI
                if(baseURI)
                {
                    try {
                        XMLUri temp(baseURI, ((DOMDocumentImpl *)this->getOwnerDocument())->getMemoryManager());
                        XMLUri temp2(&temp, uri, ((DOMDocumentImpl *)this->getOwnerDocument())->getMemoryManager());
                        uri = ((DOMDocumentImpl *)this->getOwnerDocument())->cloneString(temp2.getUriText());
                    }
                    catch(const OutOfMemoryException&)
                    {
                        throw;
                    }
                    catch (...){
                        // REVISIT: what should happen in this case?
                        return 0;
                    }
                }
                return uri;
            }
        }
    }
    return baseURI;
}


void DOMElementNSImpl::setPrefix(const XMLCh *prefix)
{
    if (fNode.isReadOnly())
        throw DOMException(DOMException::NO_MODIFICATION_ALLOWED_ERR, 0, GetDOMNodeMemoryManager);
    if (fNamespaceURI == 0 || fNamespaceURI[0] == chNull)
        throw DOMException(DOMException::NAMESPACE_ERR, 0, GetDOMNodeMemoryManager);

    if (prefix == 0 || *prefix == 0) {
        fPrefix = 0;
        fName = fLocalName;
        return;
    }

    if(!((DOMDocumentImpl *)this->getOwnerDocument())->isXMLName(prefix))
        throw DOMException(DOMException::INVALID_CHARACTER_ERR,0, GetDOMNodeMemoryManager);

    const XMLCh * xml      = DOMNodeImpl::getXmlString();
    const XMLCh * xmlURI   = DOMNodeImpl::getXmlURIString();

    if (XMLString::equals(prefix, xml) &&
        !XMLString::equals(fNamespaceURI, xmlURI))
        throw DOMException(DOMException::NAMESPACE_ERR, 0, GetDOMNodeMemoryManager);


    if (XMLString::indexOf(prefix, chColon) != -1) {
        throw DOMException(DOMException::NAMESPACE_ERR, 0, GetDOMNodeMemoryManager);
    }

    this-> fPrefix = ((DOMDocumentImpl *)this->getOwnerDocument())->getPooledString(prefix);

    int prefixLen = XMLString::stringLen(prefix);
    int newQualifiedNameLen = prefixLen+1+XMLString::stringLen(fLocalName);

    XMLCh *newName;
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

void DOMElementNSImpl::release()
{
    if (fNode.isOwned() && !fNode.isToBeReleased())
        throw DOMException(DOMException::INVALID_ACCESS_ERR,0, GetDOMNodeMemoryManager);

    DOMDocumentImpl* doc = (DOMDocumentImpl*) getOwnerDocument();
    if (doc) {
        fNode.callUserDataHandlers(DOMUserDataHandler::NODE_DELETED, 0, 0);
        fParent.release();
        doc->release(this, DOMDocumentImpl::ELEMENT_NS_OBJECT);
    }
    else {
        // shouldn't reach here
        throw DOMException(DOMException::INVALID_ACCESS_ERR,0, GetDOMNodeMemoryManager);
    }
}

DOMNode* DOMElementNSImpl::rename(const XMLCh* namespaceURI, const XMLCh* name)
{
    setName(namespaceURI, name);
    fAttributes->reconcileDefaultAttributes(getDefaultAttributes());
    return this;
}

void DOMElementNSImpl::setName(const XMLCh *namespaceURI,
                               const XMLCh *qualifiedName)
{
    DOMDocumentImpl* ownerDoc = (DOMDocumentImpl *) getOwnerDocument();
    this->fName = ownerDoc->getPooledString(qualifiedName);

    int index = DOMDocumentImpl::indexofQualifiedName(qualifiedName);
    if (index < 0)
        throw DOMException(DOMException::NAMESPACE_ERR, 0, GetDOMNodeMemoryManager);

    if (index == 0) {	//qualifiedName contains no ':'
        this -> fPrefix = 0;
        this -> fLocalName = this -> fName;
    } else {	//0 < index < this->name.length()-1
        XMLCh* newName;
        XMLCh temp[4000];
        if (index >= 3999)
            newName = (XMLCh*) ownerDoc->getMemoryManager()->allocate
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
            ownerDoc->getMemoryManager()->deallocate(newName);//delete[] newName;

        // Before we carry on, we should check if the prefix or localName are valid XMLName
        if (!((DOMDocumentImpl *)this->getOwnerDocument())->isXMLName(fPrefix) || !((DOMDocumentImpl *)this->getOwnerDocument())->isXMLName(fLocalName))
            throw DOMException(DOMException::NAMESPACE_ERR, 0, GetDOMNodeMemoryManager);
    }

    // DOM Level 3: namespace URI is never empty string.
    const XMLCh * URI = DOMNodeImpl::mapPrefix
        (
            fPrefix,
            (!namespaceURI || !*namespaceURI) ? 0 : namespaceURI,
            DOMNode::ELEMENT_NODE
        );
    this -> fNamespaceURI = (URI == 0) ? 0 : ownerDoc->getPooledString(URI);
}

const DOMTypeInfo *DOMElementNSImpl::getTypeInfo() const
{
    if(!fSchemaType) 
        return &DOMTypeInfoImpl::g_DtdValidatedElement;
    return fSchemaType;
}

void DOMElementNSImpl::setTypeInfo(const DOMTypeInfoImpl* typeInfo) 
{
    fSchemaType = typeInfo;
}

bool DOMElementNSImpl::isSupported(const XMLCh *feature, const XMLCh *version) const
{
    // check for '+DOMPSVITypeInfo'
    if(feature && *feature=='+' && XMLString::equals(feature+1, XMLUni::fgXercescInterfacePSVITypeInfo))
        return true;
    return fNode.isSupported (feature, version);
}

DOMNode * DOMElementNSImpl::getInterface(const XMLCh* feature)
{
    if(XMLString::equals(feature, XMLUni::fgXercescInterfacePSVITypeInfo))
    {
        // go through a temp variable, as gcc 4.0.x computes the wrong offset if presented with two consecutive casts
        const DOMPSVITypeInfo* pTmp=fSchemaType;
        return (DOMNode*)pTmp;
    }
    return DOMElementImpl::getInterface(feature);
}

XERCES_CPP_NAMESPACE_END

