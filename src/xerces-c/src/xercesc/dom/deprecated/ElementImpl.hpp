#ifndef ElementImpl_HEADER_GUARD_
#define ElementImpl_HEADER_GUARD_

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
 * $Id: ElementImpl.hpp 568078 2007-08-21 11:43:25Z amassari $
 */

//
//  This file is part of the internal implementation of the C++ XML DOM.
//  It should NOT be included or used directly by application programs.
//
//  Applications should include the file <xercesc/dom/deprecated/DOM.hpp> for the entire
//  DOM API, or DOM_*.hpp for individual DOM classes, where the class
//  name is substituded for the *.
//


#include <xercesc/util/XercesDefs.hpp>
#include "AttrImpl.hpp"
#include "AttrMapImpl.hpp"
#include "ParentNode.hpp"

XERCES_CPP_NAMESPACE_BEGIN


class DeepNodeListImpl;

class DEPRECATED_DOM_EXPORT ElementImpl: public ParentNode {
protected:
    DOMString name;
    AttrMapImpl *attributes;

public:
    ElementImpl(DocumentImpl *ownerDoc, const DOMString &name);
    ElementImpl(const ElementImpl &other, bool deep=false);
    virtual ~ElementImpl();

    virtual bool isElementImpl();
    virtual NodeImpl * cloneNode(bool deep);
    virtual DOMString getNodeName();
    virtual short getNodeType();
    virtual DOMString getAttribute(const DOMString &name);
    virtual AttrImpl *getAttributeNode(const DOMString &name);
    virtual NamedNodeMapImpl * getAttributes();
    virtual DeepNodeListImpl * getElementsByTagName(const DOMString &tagname);
    virtual DOMString getTagName();
    virtual void removeAttribute(const DOMString &name);
    virtual AttrImpl * removeAttributeNode(AttrImpl * oldAttr);
    virtual AttrImpl *setAttribute(const DOMString &name, const DOMString &value);
    virtual AttrImpl *setAttributeNode(AttrImpl *newAttr);
    virtual void setReadOnly(bool readOnly, bool deep);

    //Introduced in DOM Level 2
    virtual DOMString getAttributeNS(const DOMString &namespaceURI,
	const DOMString &localName);
    virtual AttrImpl *setAttributeNS(const DOMString &namespaceURI,
	const DOMString &qualifiedName, const DOMString &value);
    virtual void removeAttributeNS(const DOMString &namespaceURI,
	const DOMString &localName);
    virtual AttrImpl *getAttributeNodeNS(const DOMString &namespaceURI,
	const DOMString &localName);
    virtual AttrImpl *setAttributeNodeNS(AttrImpl *newAttr);
    virtual DeepNodeListImpl *getElementsByTagNameNS(const DOMString &namespaceURI,
	const DOMString &localName);

    virtual void setOwnerDocument(DocumentImpl *doc);
    virtual bool hasAttributes();
    virtual bool hasAttribute(const DOMString &name);
    virtual bool hasAttributeNS(const DOMString &namespaceURI, const DOMString &localName);

	//Utils for the DOM_NamedNodeMap wrapper class
	//All NamedNodeMap utils begin with NNM
    virtual NamedNodeMapImpl *NNM_cloneMap(NodeImpl *nnm_ownerNode);
    virtual int             NNM_findNamePoint(const DOMString &nnm_name);
    virtual unsigned int    NNM_getLength();
    virtual NodeImpl       *NNM_getNamedItem(const DOMString &nnm_name);
    virtual NodeImpl       *NNM_item(unsigned int nnm_index);
    virtual void            NNM_removeAll();
    virtual NodeImpl       *NNM_removeNamedItem(const DOMString &nnm_name);
    virtual NodeImpl       *NNM_setNamedItem(NodeImpl *nnm_arg);
    virtual void            NNM_setReadOnly(bool nnm_readOnly, bool nnm_deep);
    //Introduced in DOM Level 2
    virtual int             NNM_findNamePoint(const DOMString &nnm_namespaceURI, const DOMString &nnm_localName);
    virtual NodeImpl       *NNM_getNamedItemNS(const DOMString &nnm_namespaceURI, const DOMString &nnm_localName);
    virtual NodeImpl       *NNM_setNamedItemNS(NodeImpl *nnm_arg);
    virtual NodeImpl       *NNM_removeNamedItemNS(const DOMString &nnm_namespaceURI, const DOMString &nnm_localName);
    virtual void            NNM_setOwnerDocument(DocumentImpl *nnm_doc);

	// default attribute helper functions
	virtual AttrMapImpl *getDefaultAttributes();
	virtual void setupDefaultAttributes();

    // -----------------------------------------------------------------------
    //  Notification that lazy data has been deleted
    // -----------------------------------------------------------------------
	static void reinitElementImpl();
};

XERCES_CPP_NAMESPACE_END

#endif
