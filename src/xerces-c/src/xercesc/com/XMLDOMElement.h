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
 * $Id: XMLDOMElement.h 568078 2007-08-21 11:43:25Z amassari $
 */

#ifndef ___xmldomelement_h___
#define ___xmldomelement_h___

#include <xercesc/dom/DOMElement.hpp>
#include "IXMLDOMNodeImpl.h"

XERCES_CPP_NAMESPACE_USE

class ATL_NO_VTABLE CXMLDOMElement :
	public CComObjectRootEx<CComSingleThreadModel>,
	public IXMLDOMNodeImpl<IXMLDOMElement, &IID_IXMLDOMElement>
{
public:
	CXMLDOMElement()
	{}

	void	FinalRelease()
	{
		ReleaseOwnerDoc();
	}

	virtual DOMNode* get_DOMNode()	  { return element;}
	virtual DOMNodeType get_DOMNodeType() const  { return NODE_ELEMENT; }

DECLARE_NOT_AGGREGATABLE(CXMLDOMElement)

DECLARE_PROTECT_FINAL_CONSTRUCT()

BEGIN_COM_MAP(CXMLDOMElement)
	COM_INTERFACE_ENTRY(IXMLDOMElement)
	COM_INTERFACE_ENTRY(IXMLDOMNode)
	COM_INTERFACE_ENTRY(IIBMXMLDOMNodeIdentity)
	COM_INTERFACE_ENTRY(IDispatch)
	COM_INTERFACE_ENTRY(ISupportErrorInfo)
END_COM_MAP()

	// IXMLDOMElement methods
	STDMETHOD(get_tagName)(BSTR  *pVal);
	STDMETHOD(getAttribute)(BSTR name, VARIANT  *value);
	STDMETHOD(setAttribute)(BSTR name, VARIANT value);
	STDMETHOD(removeAttribute)(BSTR name);
	STDMETHOD(getAttributeNode)(BSTR name, IXMLDOMAttribute  * *attr);
	STDMETHOD(setAttributeNode)(IXMLDOMAttribute  *attr, IXMLDOMAttribute  * *attributeNode);
	STDMETHOD(removeAttributeNode)(IXMLDOMAttribute  *attr, IXMLDOMAttribute  * *attributeNode);
	STDMETHOD(getElementsByTagName)(BSTR tagName, IXMLDOMNodeList  * *resultList);
	STDMETHOD(normalize)(void);

	DOMElement* element;
};

typedef CComObject<CXMLDOMElement> CXMLDOMElementObj;

#endif // ___xmldomelement_h___