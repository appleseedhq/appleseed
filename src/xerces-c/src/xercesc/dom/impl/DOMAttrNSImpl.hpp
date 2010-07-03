#ifndef DOMAttrNSImpl_HEADER_GUARD_
#define DOMAttrNSImpl_HEADER_GUARD_

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
 * $Id: DOMAttrNSImpl.hpp 568078 2007-08-21 11:43:25Z amassari $
 */

//
//  This file is part of the internal implementation of the C++ XML DOM.
//  It should NOT be included or used directly by application programs.
//
//  Applications should include the file <xercesc/dom/DOM.hpp> for the entire
//  DOM API, or xercesc/dom/DOM*.hpp for individual DOM classes, where the class
//  name is substituded for the *.
//

#include "DOMAttrImpl.hpp"

XERCES_CPP_NAMESPACE_BEGIN


class CDOM_EXPORT DOMAttrNSImpl: public DOMAttrImpl {
protected:
    //Introduced in DOM Level 2
    const XMLCh * fNamespaceURI;     //namespace URI of this node
    const XMLCh * fLocalName;        //local part of qualified name
    const XMLCh * fPrefix;           // prefix part of qualified name
                           // revisit - can return local part
                           //    by pointing into the qualified (L1) name.

public:
    DOMAttrNSImpl(DOMDocument *ownerDoc, const XMLCh *name);
    DOMAttrNSImpl(DOMDocument *ownerDoc, //DOM Level 2
	                const XMLCh *namespaceURI, const XMLCh *qualifiedName);
    DOMAttrNSImpl(const DOMAttrNSImpl &other, bool deep=false);

    virtual DOMNode * cloneNode(bool deep) const;
    //Introduced in DOM Level 2
    virtual const XMLCh *   getNamespaceURI() const;
    virtual const XMLCh *   getPrefix() const;
    virtual const XMLCh *   getLocalName() const;
    virtual void            setPrefix(const XMLCh *prefix);
    virtual void            release();

   // helper function for DOM Level 3 renameNode
   virtual DOMNode* rename(const XMLCh* namespaceURI, const XMLCh* name);
   void setName(const XMLCh* namespaceURI, const XMLCh* name);

private:
    // -----------------------------------------------------------------------
    // Unimplemented constructors and operators
    // -----------------------------------------------------------------------    
    DOMAttrNSImpl & operator = (const DOMAttrNSImpl &);
};

XERCES_CPP_NAMESPACE_END

#endif
