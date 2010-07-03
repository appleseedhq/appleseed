#ifndef EntityImpl_HEADER_GUARD_
#define EntityImpl_HEADER_GUARD_

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
 * $Id: EntityImpl.hpp 568078 2007-08-21 11:43:25Z amassari $
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
#include "ParentNode.hpp"
#include "EntityReferenceImpl.hpp"

XERCES_CPP_NAMESPACE_BEGIN


class DEPRECATED_DOM_EXPORT EntityImpl: public ParentNode {
private:
    DOMString name;
    DOMString publicId;
    DOMString systemId;
    DOMString notationName;

	void setEntityRef(EntityReferenceImpl*);

    friend class DOMParser;

public:
    EntityImpl(DocumentImpl*, const DOMString &eName);
    EntityImpl(const EntityImpl &other, bool deep=false);
    virtual ~EntityImpl();

    virtual NodeImpl*   cloneNode(bool deep);
    virtual DOMString getNodeName();
    virtual short getNodeType();
    virtual DOMString   getPublicId();
    virtual DOMString   getSystemId();
    virtual DOMString   getNotationName();
    virtual void        setNotationName(const DOMString &arg);
    virtual void        setPublicId(const DOMString &arg);
    virtual void        setSystemId(const DOMString &arg);
};

XERCES_CPP_NAMESPACE_END

#endif

