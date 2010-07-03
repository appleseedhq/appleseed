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
 * $Id: DOMCDATASectionImpl.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include "DOMCDATASectionImpl.hpp"
#include "DOMNodeImpl.hpp"
#include "DOMRangeImpl.hpp"
#include "DOMDocumentImpl.hpp"
#include "DOMCasts.hpp"
#include "DOMStringPool.hpp"
#include <xercesc/dom/DOMException.hpp>
#include <xercesc/util/XMLUniDefs.hpp>

XERCES_CPP_NAMESPACE_BEGIN

DOMCDATASectionImpl::DOMCDATASectionImpl(DOMDocument *ownerDoc,
                                   const XMLCh *dat)
    : fNode(ownerDoc), fParent(ownerDoc), fCharacterData(ownerDoc, dat)
{
}


DOMCDATASectionImpl::DOMCDATASectionImpl(const DOMCDATASectionImpl &other, bool /*deep*/)
    : DOMCDATASection(other),
    fNode(*castToNodeImpl(&other)),
    fParent(*castToParentImpl(&other)),
    fChild(*castToChildImpl(&other)),
    fCharacterData(other.fCharacterData)
{
    // revisit.  Something nees to make "deep" work.
}


DOMCDATASectionImpl::~DOMCDATASectionImpl()
{
}


DOMNode  *DOMCDATASectionImpl::cloneNode(bool deep) const
{
    DOMNode* newNode = new (this->getOwnerDocument(), DOMDocumentImpl::CDATA_SECTION_OBJECT) DOMCDATASectionImpl(*this, deep);
    fNode.callUserDataHandlers(DOMUserDataHandler::NODE_CLONED, this, newNode);
    return newNode;
}


const XMLCh * DOMCDATASectionImpl::getNodeName() const {
    static const XMLCh gcdata_section[] = {chPound, chLatin_c, chLatin_d, chLatin_a, chLatin_t, chLatin_a,
        chDash, chLatin_s, chLatin_e, chLatin_c, chLatin_t, chLatin_i, chLatin_o, chLatin_n, 0};
    return gcdata_section;
}


short DOMCDATASectionImpl::getNodeType() const {
    return DOMNode::CDATA_SECTION_NODE;
}


bool DOMCDATASectionImpl::isIgnorableWhitespace() const
{
    return fNode.ignorableWhitespace();
}


//
//  splitText.   revist - factor into a common function for use
//                             here and in DOMTextImpl
//
DOMText *DOMCDATASectionImpl::splitText(XMLSize_t offset)
{
    if (fNode.isReadOnly())
    {
        throw DOMException(DOMException::NO_MODIFICATION_ALLOWED_ERR, 0, GetDOMNodeMemoryManager);
    }
    XMLSize_t len = fCharacterData.fDataBuf->getLen();
    if (offset > len)
        throw DOMException(DOMException::INDEX_SIZE_ERR, 0, GetDOMNodeMemoryManager);

    DOMText *newText =
                getOwnerDocument()->createCDATASection(
                        this->substringData(offset, len - offset));

    DOMNode *parent = getParentNode();
    if (parent != 0)
        parent->insertBefore(newText, getNextSibling());

    fCharacterData.fDataBuf->chop(offset);

    if (this->getOwnerDocument() != 0) {
        Ranges* ranges = ((DOMDocumentImpl *)this->getOwnerDocument())->getRanges();
        if (ranges != 0) {
            XMLSize_t sz = ranges->size();
            if (sz != 0) {
                for (XMLSize_t i =0; i<sz; i++) {
                    ranges->elementAt(i)->updateSplitInfo( this, newText, offset);
                }
            }
        }
    }

    return newText;
}


bool DOMCDATASectionImpl::getIsWhitespaceInElementContent() const
{
    return isIgnorableWhitespace();
}

const XMLCh* DOMCDATASectionImpl::getWholeText() {
    throw DOMException(DOMException::NOT_SUPPORTED_ERR, 0, GetDOMNodeMemoryManager);
    return 0;
}

DOMText* DOMCDATASectionImpl::replaceWholeText(const XMLCh*){
    throw DOMException(DOMException::NOT_SUPPORTED_ERR, 0, GetDOMNodeMemoryManager);
    return 0;
}


void DOMCDATASectionImpl::release()
{
    if (fNode.isOwned() && !fNode.isToBeReleased())
        throw DOMException(DOMException::INVALID_ACCESS_ERR,0, GetDOMNodeMemoryManager);

    DOMDocumentImpl* doc = (DOMDocumentImpl*) getOwnerDocument();

    if (doc) {
        fNode.callUserDataHandlers(DOMUserDataHandler::NODE_DELETED, 0, 0);
        fParent.release();
        fCharacterData.releaseBuffer();
        doc->release(this, DOMDocumentImpl::CDATA_SECTION_OBJECT);
    }
    else {
        // shouldn't reach here
        throw DOMException(DOMException::INVALID_ACCESS_ERR,0, GetDOMNodeMemoryManager);
    }
}


//
//  Delegation stubs for other DOM_Node inherited functions.
//
           DOMNode*         DOMCDATASectionImpl::appendChild(DOMNode *newChild)          {return fParent.appendChild (newChild); }
           DOMNamedNodeMap* DOMCDATASectionImpl::getAttributes() const                   {return fNode.getAttributes (); }
           DOMNodeList*     DOMCDATASectionImpl::getChildNodes() const                   {return fParent.getChildNodes (); }
           DOMNode*         DOMCDATASectionImpl::getFirstChild() const                   {return fParent.getFirstChild (); }
           DOMNode*         DOMCDATASectionImpl::getLastChild() const                    {return fParent.getLastChild (); }
     const XMLCh*           DOMCDATASectionImpl::getLocalName() const                    {return fNode.getLocalName (); }
     const XMLCh*           DOMCDATASectionImpl::getNamespaceURI() const                 {return fNode.getNamespaceURI (); }
           DOMNode*         DOMCDATASectionImpl::getNextSibling() const                  {return fChild.getNextSibling (); }
     const XMLCh*           DOMCDATASectionImpl::getNodeValue() const                    {return fCharacterData.getNodeValue (); }
           DOMDocument*     DOMCDATASectionImpl::getOwnerDocument() const                {return fParent.fOwnerDocument; }
     const XMLCh*           DOMCDATASectionImpl::getPrefix() const                       {return fNode.getPrefix (); }
           DOMNode*         DOMCDATASectionImpl::getParentNode() const                   {return fChild.getParentNode (this); }
           DOMNode*         DOMCDATASectionImpl::getPreviousSibling() const              {return fChild.getPreviousSibling (this); }
           bool             DOMCDATASectionImpl::hasChildNodes() const                   {return fParent.hasChildNodes (); }
           DOMNode*         DOMCDATASectionImpl::insertBefore(DOMNode *newChild, DOMNode *refChild)
                                                                                         {return fParent.insertBefore (newChild, refChild); }
           void             DOMCDATASectionImpl::normalize()                             {fNode.normalize (); }
           DOMNode*         DOMCDATASectionImpl::removeChild(DOMNode *oldChild)          {return fParent.removeChild (oldChild); }
           DOMNode*         DOMCDATASectionImpl::replaceChild(DOMNode *newChild, DOMNode *oldChild)
                                                                                         {return fParent.replaceChild (newChild, oldChild); }
           bool             DOMCDATASectionImpl::isSupported(const XMLCh *feature, const XMLCh *version) const
                                                                                         {return fNode.isSupported (feature, version); }
           void             DOMCDATASectionImpl::setPrefix(const XMLCh  *prefix)         {fNode.setPrefix(prefix); }
           bool             DOMCDATASectionImpl::hasAttributes() const                   {return fNode.hasAttributes(); }
           bool             DOMCDATASectionImpl::isSameNode(const DOMNode* other) const  {return fNode.isSameNode(other); }
           bool             DOMCDATASectionImpl::isEqualNode(const DOMNode* arg) const   {return fParent.isEqualNode(arg); }
           void*            DOMCDATASectionImpl::setUserData(const XMLCh* key, void* data, DOMUserDataHandler* handler)
                                                                                         {return fNode.setUserData(key, data, handler); }
           void*            DOMCDATASectionImpl::getUserData(const XMLCh* key) const     {return fNode.getUserData(key); }
           const XMLCh*     DOMCDATASectionImpl::getBaseURI() const                      {return fNode.getBaseURI(); }
           short            DOMCDATASectionImpl::compareTreePosition(const DOMNode* other) const {return fNode.compareTreePosition(other); }
           const XMLCh*     DOMCDATASectionImpl::getTextContent() const                  {return fNode.getTextContent(); }
           void             DOMCDATASectionImpl::setTextContent(const XMLCh* textContent){fNode.setTextContent(textContent); }
           const XMLCh*     DOMCDATASectionImpl::lookupNamespacePrefix(const XMLCh* namespaceURI, bool useDefault) const  {return fNode.lookupNamespacePrefix(namespaceURI, useDefault); }
           bool             DOMCDATASectionImpl::isDefaultNamespace(const XMLCh* namespaceURI) const {return fNode.isDefaultNamespace(namespaceURI); }
           const XMLCh*     DOMCDATASectionImpl::lookupNamespaceURI(const XMLCh* prefix) const  {return fNode.lookupNamespaceURI(prefix); }
           DOMNode*         DOMCDATASectionImpl::getInterface(const XMLCh* feature)      {return fNode.getInterface(feature); }



//
//   Delegation of CharacerData functions.
//


           const XMLCh*     DOMCDATASectionImpl::getData() const                         {return fCharacterData.getData();}
           XMLSize_t        DOMCDATASectionImpl::getLength() const                       {return fCharacterData.getLength();}
           const XMLCh*     DOMCDATASectionImpl::substringData(XMLSize_t offset, XMLSize_t count) const
                                                                                         {return fCharacterData.substringData(this, offset, count);}
           void             DOMCDATASectionImpl::appendData(const XMLCh *arg)            {fCharacterData.appendData(this, arg);}
           void             DOMCDATASectionImpl::insertData(XMLSize_t offset, const  XMLCh *arg)
                                                                                         {fCharacterData.insertData(this, offset, arg);}
           void             DOMCDATASectionImpl::deleteData(XMLSize_t offset, XMLSize_t count)
                                                                                         {fCharacterData.deleteData(this, offset, count);}
           void             DOMCDATASectionImpl::replaceData(XMLSize_t offset, XMLSize_t count, const XMLCh *arg)
                                                                                         {fCharacterData.replaceData(this, offset, count, arg);}
           void             DOMCDATASectionImpl::setData(const XMLCh *data)              {fCharacterData.setData(this, data);}
           void             DOMCDATASectionImpl::setNodeValue(const XMLCh  *nodeValue)   {fCharacterData.setNodeValue (this, nodeValue); }

XERCES_CPP_NAMESPACE_END


