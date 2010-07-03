#ifndef NamedNodeMapImpl_HEADER_GUARD_
#define NamedNodeMapImpl_HEADER_GUARD_

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
 * $Id: NamedNodeMapImpl.hpp 568078 2007-08-21 11:43:25Z amassari $
 */

//
//  This file is part of the internal implementation of the C++ XML DOM.
//  It should NOT be included or used directly by application programs.
//
//  Applications should include the file <xercesc/dom/deprecated/DOM.hpp> for the entire
//  DOM API, or DOM_*.hpp for individual DOM classes, where the class
//  name is substituded for the *.
//

#include <xercesc/util/XMemory.hpp>
#include "NodeImpl.hpp"

XERCES_CPP_NAMESPACE_BEGIN


class NodeVector;
class DocumentImpl;
class NodeImpl;

class DEPRECATED_DOM_EXPORT NamedNodeMapImpl: public XMemory {
protected:
    NodeVector       *nodes;
    NodeImpl         *ownerNode;    // the node this map belongs to
    bool              readOnly;
    int               refCount;
    static int        gLiveNamedNodeMaps;
    static int        gTotalNamedNodeMaps;


    friend class      DOM_NamedNodeMap;
    friend class      DomMemDebug;
    friend class      ElementImpl;
   	friend class	  DocumentImpl;

	virtual void	cloneContent(NamedNodeMapImpl *srcmap);

public:
    NamedNodeMapImpl(NodeImpl *ownerNode);

    virtual                 ~NamedNodeMapImpl();
    virtual NamedNodeMapImpl *cloneMap(NodeImpl *ownerNode);
    static  void            addRef(NamedNodeMapImpl *);
    virtual int             findNamePoint(const DOMString &name);
    virtual unsigned int    getLength();
    virtual NodeImpl        *getNamedItem(const DOMString &name);
    virtual NodeImpl        *item(unsigned int index);
    virtual void            removeAll();
    virtual NodeImpl        *removeNamedItem(const DOMString &name);
    static  void            removeRef(NamedNodeMapImpl *);
    virtual NodeImpl        *setNamedItem(NodeImpl *arg);
    virtual void            setReadOnly(bool readOnly, bool deep);

    //Introduced in DOM Level 2
    virtual int             findNamePoint(const DOMString &namespaceURI,
	const DOMString &localName);
    virtual NodeImpl        *getNamedItemNS(const DOMString &namespaceURI,
	const DOMString &localName);
    virtual NodeImpl        *setNamedItemNS(NodeImpl *arg);
    virtual NodeImpl        *removeNamedItemNS(const DOMString &namespaceURI,
	const DOMString &localName);

    virtual void setOwnerDocument(DocumentImpl *doc);
};

XERCES_CPP_NAMESPACE_END

#endif

