#ifndef NotationImpl_HEADER_GUARD_
#define NotationImpl_HEADER_GUARD_
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
 * $Id: NotationImpl.hpp 568078 2007-08-21 11:43:25Z amassari $
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
#include "NodeImpl.hpp"

XERCES_CPP_NAMESPACE_BEGIN


class DocumentImpl;

class DEPRECATED_DOM_EXPORT NotationImpl: public NodeImpl {
private:
    DOMString name;
    DOMString publicId;
    DOMString systemId;

public:
    NotationImpl(DocumentImpl *, const DOMString &);
    NotationImpl(const NotationImpl &other, bool deep=false);

    virtual ~NotationImpl();

    virtual NodeImpl *cloneNode(bool deep);
    virtual DOMString getNodeName();
    virtual short getNodeType();
    virtual NodeImpl * getParentNode();

    //
    // The Public Identifier for this Notation. If no public identifier
    // was specified, this will be null.
    virtual DOMString getPublicId();

    // The System Identifier for this Notation. If no system identifier
    // was specified, this will be null.
    virtual DOMString getSystemId();


    // NON-DOM: The Public Identifier for this Notation. If no public
    // identifier was specified, this will be null.  */
    virtual void setPublicId(const DOMString &arg);


    // NON-DOM: The System Identifier for this Notation. If no system
    // identifier was specified, this will be null.  */
    virtual void setSystemId(const DOMString &arg);

};

XERCES_CPP_NAMESPACE_END

#endif
