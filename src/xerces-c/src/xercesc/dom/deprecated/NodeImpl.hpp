#ifndef NodeImpl_HEADER_GUARD_
#define NodeImpl_HEADER_GUARD_

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
 * $Id: NodeImpl.hpp 568078 2007-08-21 11:43:25Z amassari $
 */

//
//  This file is part of the internal implementation of the C++ XML DOM.
//  It should NOT be included or used directly by application programs.
//
//  Applications should include the file <xercesc/dom/deprecated/DOM.hpp> for the entire
//  DOM API, or DOM_*.hpp for individual DOM classes, where the class
//  name is substituded for the *.
//

/**
 * A NodeImpl doesn't have any children, and can therefore only be directly
 * inherited by classes of nodes that never have any, such as Text nodes. For
 * other types, such as Element, classes must inherit from ParentNode.
 * <P>
 * All nodes in a single document must originate
 * in that document. (Note that this is much tighter than "must be
 * same implementation") Nodes are all aware of their ownerDocument,
 * and attempts to mismatch will throw WRONG_DOCUMENT_ERR.
 * <P>
 * However, to save memory not all nodes always have a direct reference
 * to their ownerDocument. When a node is owned by another node it relies
 * on its owner to store its ownerDocument. Parent nodes always store it
 * though, so there is never more than one level of indirection.
 * And when a node doesn't have an owner, ownerNode refers to its
 * ownerDocument.
 **/

#include <xercesc/util/XMemory.hpp>
#include "NodeListImpl.hpp"
#include "DOMString.hpp"

XERCES_CPP_NAMESPACE_BEGIN


class NamedNodeMapImpl;
class NodeListImpl;
class DocumentImpl;

//  define 'null' is used extensively in the DOM implementation code,
//  as a consequence of its Java origins.
//  MSVC 5.0 compiler has problems with overloaded function resolution
//	when using the const int definition.
//
#if defined(XML_CSET)
const int null = 0;
#else
#define null 0
#endif


class DEPRECATED_DOM_EXPORT NodeImpl: public NodeListImpl {
public:
    NodeImpl                *ownerNode; // typically the parent but not always!

    // data

    unsigned short flags;

    static const unsigned short READONLY;
    static const unsigned short SYNCDATA;
    static const unsigned short SYNCCHILDREN;
    static const unsigned short OWNED;
    static const unsigned short FIRSTCHILD;
    static const unsigned short SPECIFIED;
    static const unsigned short IGNORABLEWS;
    static const unsigned short SETVALUE;
    static const unsigned short ID_ATTR;
    static const unsigned short USERDATA;
    static const unsigned short HASSTRING;

    static int              gLiveNodeImpls; // Counters for debug & tuning.
    static int              gTotalNodeImpls;

public:
    NodeImpl(DocumentImpl *ownerDocument);
    NodeImpl(const NodeImpl &other);
    virtual ~NodeImpl();

    // Dynamic Cast replacement functions.
    virtual bool isAttrImpl();
    virtual bool isCDATASectionImpl();
    virtual bool isDocumentFragmentImpl();
    virtual bool isDocumentImpl();
    virtual bool isDocumentTypeImpl();
    virtual bool isElementImpl();
    virtual bool isEntityReference();
    virtual bool isTextImpl();

    virtual void changed();
    virtual int changes();

    virtual NodeImpl *appendChild(NodeImpl *newChild);
    virtual NodeImpl * cloneNode(bool deep) = 0;
    static void deleteIf(NodeImpl *thisNode);
    virtual NamedNodeMapImpl * getAttributes();
    virtual NodeListImpl *getChildNodes();
    virtual NodeImpl * getFirstChild();
    virtual NodeImpl * getLastChild();
    virtual unsigned int getLength();
    virtual NodeImpl * getNextSibling();
    virtual DOMString getNodeName() = 0;
    virtual short getNodeType() = 0;
    virtual DOMString getNodeValue();
    virtual DocumentImpl * getOwnerDocument();
    virtual NodeImpl * getParentNode();
    virtual NodeImpl*  getPreviousSibling();
    virtual void *getUserData();
    virtual bool        hasChildNodes();
    virtual NodeImpl    *insertBefore(NodeImpl *newChild, NodeImpl *refChild);
    static  bool        isKidOK(NodeImpl *parent, NodeImpl *child);
    virtual NodeImpl    *item(unsigned int index);
    virtual void        referenced();
    virtual NodeImpl    * removeChild(NodeImpl *oldChild);
    virtual NodeImpl    *replaceChild(NodeImpl *newChild, NodeImpl *oldChild);
    virtual void        setNodeValue(const DOMString &value);
    virtual void        setReadOnly(bool readOnly, bool deep);
    virtual void        setUserData(void *value);
    virtual DOMString   toString();
    virtual void        unreferenced();

    //Introduced in DOM Level 2
    virtual void	normalize();
    virtual bool	isSupported(const DOMString &feature, const DOMString &version);
    virtual DOMString	getNamespaceURI();
    virtual DOMString   getPrefix();
    virtual DOMString   getLocalName();
    virtual void        setPrefix(const DOMString &prefix);
    virtual bool        hasAttributes();

protected:
    //Utility, not part of DOM Level 2 API
    static const DOMString&	mapPrefix(const DOMString &prefix,
	const DOMString &namespaceURI, short nType);
    static DOMString getXmlnsString();
    static DOMString getXmlnsURIString();
    static DOMString getXmlString();
    static DOMString getXmlURIString();

public: // should really be protected - ALH

    virtual void setOwnerDocument(DocumentImpl *doc);
    // NON-DOM
    // unlike getOwnerDocument this never returns null, even for Document nodes
    virtual DocumentImpl * getDocument();

    /*
     * Flags setters and getters
     */

    inline bool isReadOnly() const {
        return (flags & READONLY) != 0;
    }

    inline void isReadOnly(bool value) {
        flags = (value ? flags | READONLY : flags & ~READONLY);
    }

    inline bool needsSyncData() const {
        return (flags & SYNCDATA) != 0;
    }

    inline void needsSyncData(bool value) {
        flags = (value ? flags | SYNCDATA : flags & ~SYNCDATA);
    }

    inline bool needsSyncChildren() const {
        return (flags & SYNCCHILDREN) != 0;
    }

    inline void needsSyncChildren(bool value) {
        flags = (value ? flags | SYNCCHILDREN : flags & ~SYNCCHILDREN);
    }

    inline bool isOwned() const {
        return (flags & OWNED) != 0;
    }

    inline void isOwned(bool value) {
        flags = (value ? flags | OWNED : flags & ~OWNED);
    }

    inline bool isFirstChild() const {
        return (flags & FIRSTCHILD) != 0;
    }

    inline void isFirstChild(bool value) {
        flags = (value ? flags | FIRSTCHILD : flags & ~FIRSTCHILD);
    }

    inline bool isSpecified() const {
        return (flags & SPECIFIED) != 0;
    }

    inline void isSpecified(bool value) {
        flags = (value ? flags | SPECIFIED : flags & ~SPECIFIED);
    }

    inline bool ignorableWhitespace() const {
        return (flags & IGNORABLEWS) != 0;
    }

    inline void ignorableWhitespace(bool value) {
        flags = (value ? flags | IGNORABLEWS : flags & ~IGNORABLEWS);
    }

    inline bool setValueCalled() const {
        return (flags & SETVALUE) != 0;
    }

    inline void setValueCalled(bool value) {
        flags = (value ? flags | SETVALUE : flags & ~SETVALUE);
    }

    inline bool isIdAttr() const {
        return (flags & ID_ATTR) != 0;
    }

    inline void isIdAttr(bool value) {
        flags = (value ? flags | ID_ATTR : flags & ~ID_ATTR);
    }

    inline bool hasUserData() const {
        return (flags & USERDATA) != 0;
    }

    inline void hasUserData(bool value) {
        flags = (value ? flags | USERDATA : flags & ~USERDATA);
    }

    inline bool hasStringValue() const {
        return (flags & HASSTRING) != 0;
    }

    inline void hasStringValue(bool value) {
        flags = (value ? flags | HASSTRING : flags & ~HASSTRING);
    }

    // -----------------------------------------------------------------------
    //  Notification that lazy data has been deleted
    // -----------------------------------------------------------------------
	static void reinitNodeImpl();

};


XERCES_CPP_NAMESPACE_END

#endif
