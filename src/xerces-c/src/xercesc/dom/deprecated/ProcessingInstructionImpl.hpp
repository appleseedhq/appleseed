#ifndef ProcessingInstructionImpl_HEADER_GUARD_
#define ProcessingInstructionImpl_HEADER_GUARD_
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
 * $Id: ProcessingInstructionImpl.hpp 568078 2007-08-21 11:43:25Z amassari $
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
#include "ChildNode.hpp"

XERCES_CPP_NAMESPACE_BEGIN


class    DocumentImpl;


class DEPRECATED_DOM_EXPORT ProcessingInstructionImpl: public ChildNode {
protected:
    DOMString target;
    DOMString data;

public:
    ProcessingInstructionImpl(DocumentImpl *ownerDoc,
                              const DOMString & target,
                              const DOMString &data);
    ProcessingInstructionImpl(const ProcessingInstructionImpl &other,
                              bool deep=false);
    virtual ~ProcessingInstructionImpl();
    virtual NodeImpl *cloneNode(bool deep);
    virtual DOMString getNodeValue();
    virtual void setNodeValue(const DOMString &value);
    virtual DOMString getNodeName();
    virtual short getNodeType();
    virtual DOMString getData();
    virtual DOMString getTarget();
    virtual void setData(const DOMString &arg);
};

XERCES_CPP_NAMESPACE_END

#endif

