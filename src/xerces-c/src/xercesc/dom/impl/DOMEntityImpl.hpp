#ifndef DOMEntityImpl_HEADER_GUARD_
#define DOMEntityImpl_HEADER_GUARD_

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
 * $Id: DOMEntityImpl.hpp 568078 2007-08-21 11:43:25Z amassari $
 */

//
//  This file is part of the internal implementation of the C++ XML DOM.
//  It should NOT be included or used directly by application programs.
//
//  Applications should include the file <xercesc/dom/DOM.hpp> for the entire
//  DOM API, or xercesc/dom/DOM*.hpp for individual DOM classes, where the class
//  name is substituded for the *.
//

#include <xercesc/util/XercesDefs.hpp>
#include "DOMNodeImpl.hpp"
#include "DOMParentNode.hpp"
#include <xercesc/dom/DOMEntity.hpp>

XERCES_CPP_NAMESPACE_BEGIN


class    DOMEntityReference;

class CDOM_EXPORT DOMEntityImpl: public DOMEntity {
private:
    DOMNodeImpl      fNode;
    DOMParentNode    fParent;

    const XMLCh *   fName;
    const XMLCh *   fPublicId;
    const XMLCh *   fSystemId;
    const XMLCh *   fNotationName;
    DOMEntityReference*	fRefEntity;

    // New data introduced in DOM Level 3
    const XMLCh*          fActualEncoding;
    const XMLCh*          fEncoding;
    const XMLCh*          fVersion;
    const XMLCh*          fBaseURI;
    bool                  fEntityRefNodeCloned;
    // private helper function
    void	cloneEntityRefTree() const;

    friend class XercesDOMParser;

public:
    DOMEntityImpl(DOMDocument *doc, const XMLCh *eName);
    DOMEntityImpl(const DOMEntityImpl &other, bool deep=false);
    virtual ~DOMEntityImpl();

    // Declare all of the functions from DOMNode.
    DOMNODE_FUNCTIONS;


    virtual const XMLCh *   getPublicId() const;
    virtual const XMLCh *   getSystemId() const;
    virtual const XMLCh *   getNotationName() const;
    virtual void            setNotationName(const XMLCh *arg);
    virtual void            setPublicId(const XMLCh *arg);
    virtual void            setSystemId(const XMLCh *arg);

    //DOM Level 2 additions. Non standard functions
    virtual void		setEntityRef(DOMEntityReference *);
    virtual DOMEntityReference*	getEntityRef() const;

    //Introduced in DOM Level 3
    virtual const XMLCh*           getActualEncoding() const;
    virtual void                   setActualEncoding(const XMLCh* actualEncoding);
    virtual const XMLCh*           getEncoding() const;
    virtual void                   setEncoding(const XMLCh* encoding);
    virtual const XMLCh*           getVersion() const;
    virtual void                   setVersion(const XMLCh* version);
    virtual void                   setBaseURI(const XMLCh *arg);

private:
    // -----------------------------------------------------------------------
    // Unimplemented constructors and operators
    // -----------------------------------------------------------------------    
    DOMEntityImpl & operator = (const DOMEntityImpl &);
};

XERCES_CPP_NAMESPACE_END

#endif

