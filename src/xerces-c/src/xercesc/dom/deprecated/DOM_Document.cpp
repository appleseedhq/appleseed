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
 * $Id: DOM_Document.cpp 568078 2007-08-21 11:43:25Z amassari $
 */


#include "DOM_Document.hpp"
#include "DeepNodeListImpl.hpp"
#include "DocumentImpl.hpp"
#include "NodeIteratorImpl.hpp"
#include "TreeWalkerImpl.hpp"

XERCES_CPP_NAMESPACE_BEGIN


DOM_Document::DOM_Document (DocumentImpl *impl) :
    DOM_Node(impl)
{
};


DOM_Document::DOM_Document() :
        DOM_Node(null)
{
};


DOM_Document::DOM_Document(const DOM_Document &other)
: DOM_Node(other)
{
};


DOM_Document::~DOM_Document()
{
};

DOM_Document & DOM_Document::operator = (const DOM_Document &other)
{
    return (DOM_Document &) DOM_Node::operator = (other);
};


DOM_Document & DOM_Document::operator = (const DOM_NullPtr *other)
{
    return (DOM_Document &) DOM_Node::operator = (other);
};


DOM_Document    DOM_Document::createDocument(MemoryManager* const manager)
{
        return DOM_Document(new (manager) DocumentImpl(manager));
};


DOM_Notation    DOM_Document::createNotation(const DOMString &name)
{
        return DOM_Notation(((DocumentImpl *)fImpl)->createNotation(name));
};


DOM_DocumentType       DOM_Document::getDoctype() const {
        return DOM_DocumentType(((DocumentImpl *)fImpl)->getDoctype());
};


DOM_DOMImplementation  &DOM_Document::getImplementation() const {
        return DOM_DOMImplementation::getImplementation();
};

DOM_Element            DOM_Document::getDocumentElement() const {
        return DOM_Element(((DocumentImpl *)fImpl)->getDocumentElement());
};


DOM_Element            DOM_Document::createElement(const DOMString &tagName)
{
        return DOM_Element(((DocumentImpl *)fImpl)->createElement(tagName));
};



DOM_Element            DOM_Document::createElement(const XMLCh *tagName)
{
        return DOM_Element(((DocumentImpl *)fImpl)->createElement(tagName));
};



DOM_Entity            DOM_Document::createEntity(const DOMString &name)
{
        return DOM_Entity(((DocumentImpl *)fImpl)->createEntity(name));
};



DOM_DocumentFragment   DOM_Document::createDocumentFragment()
{
        return DOM_DocumentFragment(((DocumentImpl *)fImpl)->createDocumentFragment());
};


DOM_DocumentType DOM_Document::createDocumentType(const DOMString &name)
{
        return DOM_DocumentType(((DocumentImpl *)fImpl)->createDocumentType(name));
};



DOM_Text               DOM_Document::createTextNode(const DOMString &data) {
        return DOM_Text(((DocumentImpl *)fImpl)->createTextNode(data));
};


DOM_Comment            DOM_Document::createComment(const DOMString &data) {
        return DOM_Comment(((DocumentImpl *)fImpl)->createComment(data));
};


DOM_CDATASection       DOM_Document::createCDATASection(const DOMString &data) {
        return DOM_CDATASection(((DocumentImpl *)fImpl)->createCDATASection(data));
};


DOM_ProcessingInstruction DOM_Document::createProcessingInstruction(const DOMString &target,
                                                     const DOMString &data) {
        return DOM_ProcessingInstruction(((DocumentImpl *)fImpl)->createProcessingInstruction(target, data));
};


DOM_Attr               DOM_Document::createAttribute(const DOMString &name) {
        return DOM_Attr(((DocumentImpl *)fImpl)->createAttribute(name));
};


DOM_EntityReference    DOM_Document::createEntityReference(const DOMString &name) {
        return DOM_EntityReference(((DocumentImpl *)fImpl)->createEntityReference(name));
};


DOM_NodeIterator       DOM_Document::createNodeIterator(DOM_Node root, unsigned long whatToShow, DOM_NodeFilter* filter, bool entityReferenceExpansion) {
        return DOM_NodeIterator(DocumentImpl::createNodeIterator(root, whatToShow, filter, entityReferenceExpansion));
};


DOM_TreeWalker DOM_Document::createTreeWalker(DOM_Node root, unsigned long whatToShow, DOM_NodeFilter* filter, bool entityReferenceExpansion) {
    return DOM_TreeWalker(DocumentImpl::createTreeWalker(root, whatToShow, filter, entityReferenceExpansion));
};


DOM_NodeList DOM_Document::getElementsByTagName(const DOMString &tagname) const  {
	return DOM_NodeList(((DocumentImpl *)fImpl)->getElementsByTagName(tagname));
};


//Introduced in DOM Level 2

DOM_Node DOM_Document::importNode(const DOM_Node &importedNode, bool deep)
{
	return DOM_Node(((DocumentImpl *)fImpl)->importNode(importedNode.fImpl, deep));
};


DOM_Element         DOM_Document::createElementNS(const DOMString &namespaceURI,
	const DOMString &qualifiedName)
{
        return DOM_Element(((DocumentImpl *)fImpl)->createElementNS(namespaceURI, qualifiedName));
}


DOM_Attr            DOM_Document::createAttributeNS(const DOMString &namespaceURI,
	const DOMString &qualifiedName)
{
        return DOM_Attr(((DocumentImpl *)fImpl)->createAttributeNS(namespaceURI, qualifiedName));
}


DOM_NodeList        DOM_Document::getElementsByTagNameNS(const DOMString &namespaceURI,
	const DOMString &localName) const
{
        return DOM_NodeList(((DocumentImpl *)fImpl)->getElementsByTagNameNS(namespaceURI, localName));
}


DOM_Element         DOM_Document::getElementById(const DOMString &elementId)
{
        return DOM_Element(((DocumentImpl *)fImpl)->getElementById(elementId));
}


DOM_XMLDecl DOM_Document::createXMLDecl(const DOMString& version, const DOMString& encoding, const DOMString& standalone)
{
    return DOM_XMLDecl( ((DocumentImpl *)fImpl)->createXMLDecl(version, encoding, standalone));
}

DOM_Range    DOM_Document::createRange()
{
    return DOM_Range( ((DocumentImpl *)fImpl)->createRange() );
}


void DOM_Document::setErrorChecking(bool check) {
    ((DocumentImpl *)fImpl)->setErrorChecking(check);
}

bool DOM_Document::getErrorChecking() {
    return ((DocumentImpl *)fImpl)->getErrorChecking();
}

XERCES_CPP_NAMESPACE_END

